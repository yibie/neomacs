# Emacs Elisp Execution Engine: Architecture, Performance, and Rewritability

An analysis of the Emacs Lisp execution engine, based on source-level examination of
the GNU Emacs C codebase (as used in the Neomacs fork). All line numbers reference
the `src/` directory.

---

## Part I: How the Elisp Machine Works

### 1. Data Representation — The Tagged Pointer (`lisp.h`)

Every Lisp value is a single machine word: a 64-bit integer (`Lisp_Object`) with a
type tag embedded in the low 3 bits (LSB tagging). This works because all heap
allocations are 8-byte aligned, leaving the low 3 bits free.

**Tag encoding** (lisp.h:498-525):

| Tag Value | Type              | Description                              |
|-----------|-------------------|------------------------------------------|
| 0         | `Lisp_Symbol`     | Pointer to `struct Lisp_Symbol`          |
| 2, 6      | `Lisp_Int0/Int1`  | Fixnum (two tags = extra bit, ~±2^61)    |
| 3         | `Lisp_Cons`       | Cons cell                                |
| 4         | `Lisp_String`     | String                                   |
| 5         | `Lisp_Vectorlike` | Vectors, closures, subrs, frames, ...    |
| 7         | `Lisp_Float`      | Float                                    |

Fixnum creation: `(value << 2) | 2`. Extraction: arithmetic right-shift by 2.
The fixnum `3` is stored as the machine word `(3 << 2) | 2 = 14`.

**Key data structures**:

- **Symbol** (`struct Lisp_Symbol`, lisp.h:786): `name`, `value` (with redirect:
  PLAINVAL/VARALIAS/LOCALIZED/FORWARDED), `function` cell, `plist`, `next` (obarray
  chain). The `redirect` field determines lookup strategy.
- **Cons** (`struct Lisp_Cons`, lisp.h:1429): just `car` and `cdr`.
- **String** (`struct Lisp_String`, lisp.h:1558): `size` (chars), `size_byte` (bytes,
  negative = unibyte), `intervals` (text properties), `data` (separate allocation).
- **Vector** (`struct Lisp_Vector`, lisp.h:1732): header + flexible array of
  `Lisp_Object`. Pseudovectors (closures, subrs, frames, buffers, etc.) are specialized
  vectors distinguished by `PSEUDOVECTOR_FLAG` + subtype in the header.
- **Subr** (`struct Lisp_Subr`, lisp.h:2178): C function pointer (union of arities
  a0..a9, aMANY, aUNEVALLED), `min_args`, `max_args`, `symbol_name`.
- **Closure**: pseudovector (`PVEC_CLOSURE`) with slots: `CLOSURE_ARGLIST` (or packed
  int for bytecode), `CLOSURE_CODE` (bytecode string or body), `CLOSURE_CONSTANTS`,
  `CLOSURE_STACK_DEPTH`, `CLOSURE_DOC_STRING`, `CLOSURE_INTERACTIVE`.

### 2. The Evaluator (`eval.c`)

#### `Feval` (eval.c:2515)

Entry point for `(eval FORM LEXICAL)`:
1. Saves specpdl position
2. Binds `internal-interpreter-environment` to the lexical env (nil = dynamic,
   alist = lexical)
3. Calls `eval_sub(form)`
4. Restores bindings via `unbind_to`

#### `eval_sub` — The Core (eval.c:2552)

This is the heart of Elisp evaluation:

**Self-evaluating forms** (lines 2554-2565):
- Symbols → look up in lexical env alist first (`Fassq`), then `Fsymbol_value`
  (dynamic/global)
- Non-cons, non-symbol → return as-is (numbers, strings, vectors, etc.)

**Cons forms** `(fun . args)` (lines 2567+):
1. Check recursion depth, push backtrace entry
2. Resolve function: if symbol, get `XSYMBOL(fun)->u.s.function`; follow alias chain
   via `indirect_function`
3. Dispatch by type:
   - **C Subr** (`SUBRP`):
     - `UNEVALLED` (special forms: `if`, `let`, `cond`): pass raw arg list
     - `MANY` (variadic: `+`, `list`): eval each arg into array, call
       `subr->function.aMANY(n, vals)`
     - Fixed arity (0-9): eval args, dispatch via switch on arity
   - **Closure** (`CLOSUREP`): `apply_lambda` → eval args → `funcall_lambda`
   - **Macro**: expand via `apply1(Fcdr(fun), original_args)`, then `eval_sub`
     the expansion
   - **Autoload** `(autoload FILE ...)`: load the file, `goto retry`

#### `Ffuncall` (eval.c:3159)

Handles pre-evaluated arguments. `args[0]` is the function, `args[1..]` are args.
Dispatches via `funcall_general` → `funcall_subr` (C primitives) or
`funcall_lambda` (closures/lambdas).

#### `funcall_subr` — C Primitive Dispatch (eval.c:3228)

1. Verify `numargs >= min_args`
2. If `numargs < max_args`, pad with `Qnil`
3. Switch on `max_args`: 0-9 calls specific arity, `MANY` calls
   `subr->function.aMANY(numargs, args)`, `UNEVALLED` signals error

#### `funcall_lambda` — Closure Dispatch (eval.c:3327)

**Bytecode closures** (when `CLOSURE_ARGLIST` is an integer): fast path directly to
`exec_byte_code(fun, args_template, nargs, args)`.

**Interpreted closures**: walk the formal parameter list as a Lisp list. For each
parameter:
- Check for `&rest` / `&optional`
- Lexical binding: `lexenv = Fcons(Fcons(param, arg), lexenv)` (cons onto alist)
- Dynamic binding: `specbind(param, arg)` (push onto specpdl)
Then execute body: `Fprogn(AREF(fun, CLOSURE_CODE))`.

### 3. The Bytecode Interpreter (`bytecode.c`)

#### `exec_byte_code` (bytecode.c:481)

**Frame setup** (lines 494-549):
1. Extract bytecode string, constants vector, max stack depth from closure
2. Allocate stack frame on the bytecode stack
3. Parse `args_template` (packed int: bits 0-6 = min args, bit 7 = &rest, bits
   8-14 = max args)
4. Push arguments, pad optional args with nil, collect &rest into list

**Main dispatch loop** (lines 551+):
- **Threaded mode** (GCC): computed gotos via `goto *(targets[op = FETCH])` with a
  256-entry dispatch table
- **Fallback**: `switch(op)` statement

~168 distinct bytecodes. `top` and `pc` are pinned to registers (`rbx`, `r12` on
x86-64).

**Key bytecodes**:

- **`Bvarref`** (line 621): fast path reads `SYMBOL_PLAINVAL` directly (2 pointer
  chases), falls back to `Fsymbol_value` for buffer-local/forwarded/aliased symbols
- **`Bcall`** (line 758): if callee is a bytecode closure with integer arglist, does
  `goto setup_frame` (no C recursion — reuses bytecode stack). Otherwise falls through
  to `funcall_subr` or `funcall_general`
- **`Breturn`** (line 892): if returning to a bytecode caller, pops stack frame inline.
  If returning to C, `goto exit`
- **`Bpushconditioncase`** (line 970): pushes handler with `setjmp`, stores branch
  target for catch path

### 4. The Specpdl — Dynamic Binding Stack

A contiguous array of `union specbinding` entries (lisp.h:3581). 12 entry types:

| Tag                        | Purpose                                  |
|----------------------------|------------------------------------------|
| `SPECPDL_LET`              | Dynamic `let` binding (plain symbol)     |
| `SPECPDL_LET_LOCAL`        | Buffer-local `let` binding               |
| `SPECPDL_LET_DEFAULT`      | Global binding for localized variable    |
| `SPECPDL_UNWIND`           | `unwind-protect` on Lisp_Object          |
| `SPECPDL_UNWIND_PTR`       | `unwind-protect` on void*                |
| `SPECPDL_UNWIND_INT`       | `unwind-protect` on int                  |
| `SPECPDL_UNWIND_EXCURSION` | `save-excursion` (marker + window)       |
| `SPECPDL_UNWIND_VOID`      | `unwind-protect` with no arg             |
| `SPECPDL_BACKTRACE`        | Backtrace frame                          |
| `SPECPDL_NOP`              | No-op (placeholder)                      |
| ...                        | Plus module runtime/environment variants  |

**`specbind(sym, val)`** (eval.c:3633): saves old value on the stack, sets new value.
O(1) for plain symbols (4 field writes + pointer bump).

**`unbind_to(count, val)`** (eval.c:3916): pops entries in LIFO order, restoring old
values. This is how `let`, `save-excursion`, and `unwind-protect` are cleaned up.

Lexical binding uses a different mechanism: an alist
(`Vinternal_interpreter_environment`). Closures capture this alist in their
`CLOSURE_CONSTANTS` slot.

### 5. Error Handling — setjmp/longjmp

**`Fsignal`** (eval.c:1874) → **`signal_or_quit`** (eval.c:1914):
1. Construct error object `(error-symbol . data)`
2. Walk `handlerlist` (linked list of `struct handler`):
   - `CONDITION_CASE`: match conditions via `find_handler_clause`
   - `CATCHER_ALL`: matches everything
   - `HANDLER_BIND`: call handler function
3. If matched: `unwind_to_catch(handler, NONLOCAL_EXIT_SIGNAL, error)`

**`unwind_to_catch`** (eval.c:1400):
1. Walk `handlerlist` backward, calling `unbind_to()` for each handler's `pdlcount`
2. Restore `lisp_eval_depth`
3. `sys_longjmp(catch->jmp, 1)` — non-local jump back to the `setjmp` point

**`internal_condition_case`** (eval.c:1676) — C-level condition-case:
```c
struct handler *c = push_handler(handlers, CONDITION_CASE);
if (sys_setjmp(c->jmp)) {
    // caught: call hfun(error_value)
} else {
    // normal: call bfun()
}
```

### 6. Complete Execution Trace: `(eval '(+ 1 2))`

```
Feval('(+ 1 2), nil)
 │
 ├─ specbind(internal-interpreter-environment, nil)
 ├─ eval_sub('(+ 1 2))
 │   │
 │   ├─ form is CONSP → function call
 │   ├─ maybe_quit(), maybe_gc(), lisp_eval_depth++
 │   ├─ original_fun = '+ , original_args = '(1 2)
 │   ├─ record_in_backtrace('+, &args, UNEVALLED)
 │   │
 │   ├─ [Resolve function]
 │   │   fun = XSYMBOL('+)->u.s.function = Splus (Lisp_Subr, MANY)
 │   │
 │   ├─ [SUBRP dispatch, max_args=MANY]
 │   │   numargs = list_length('(1 2)) = 2
 │   │   vals[0] = eval_sub(1) → 1   (fixnum, self-evaluating)
 │   │   vals[1] = eval_sub(2) → 2   (fixnum, self-evaluating)
 │   │
 │   ├─ XSUBR(fun)->function.aMANY(2, vals)
 │   │   = Fplus(2, [1, 2])
 │   │     │
 │   │     ├─ check_number_coerce_marker(args[0]) → fixnum 1
 │   │     ├─ arith_driver(Aadd, 2, args, fixnum(1))
 │   │     │   ├─ accum = 1
 │   │     │   ├─ integer_to_intmax(args[1]) → next = 2
 │   │     │   ├─ ckd_add(&a, 1, 2) → a=3, no overflow
 │   │     │   └─ return make_fixnum(3) → XIL((3 << 2) | 2) = 14
 │   │     └─ return fixnum(3)
 │   │
 │   ├─ lisp_eval_depth--, specpdl_ptr-- (pop backtrace)
 │   └─ return fixnum(3)
 │
 ├─ unbind_to(count, fixnum(3))
 └─ return fixnum(3)
```

The result is `Lisp_Object` fixnum 3, encoded as machine word 14.

### 7. The Arithmetic Path in Detail

`arith_driver` (data.c:3215) is the generic integer arithmetic driver:

1. Start with first arg as `intmax_t` accumulator
2. For each subsequent arg:
   - `check_number_coerce_marker` — verify it's a number (FIXNUMP || FLOATP ||
     BIGNUMP), auto-convert markers to their position
   - `integer_to_intmax` — extract value if it fits in `intmax_t`
   - `ckd_add`/`ckd_sub`/`ckd_mul` — checked arithmetic with overflow detection
   - On overflow → `bignum_arith_driver` (GMP arbitrary precision)
   - On float operand → `float_arith_driver`
3. `make_int(accum)` — if fits in fixnum range, `make_fixnum`; otherwise allocate
   bignum

### 8. Symbol Value Lookup (`data.c`)

`find_symbol_value` (data.c:1585):
```
switch (sym->u.s.redirect):
  SYMBOL_VARALIAS   → follow alias chain, retry
  SYMBOL_PLAINVAL   → return sym->u.s.val.value
  SYMBOL_LOCALIZED  → swap in buffer-local, resolve forwarding
  SYMBOL_FORWARDED  → do_symval_forwarding (read C variable)
```

The fast path (PLAINVAL) is a single field read. Buffer-local and forwarded variables
require multiple function calls and buffer state inspection.

---

## Part II: Why Rewriting the Elisp Core Is Hard

### 1. There Is No Boundary Between "Runtime" and "Editor"

Most successful incremental rewrites (Firefox Stylo/WebRender, etc.) work because there
is a clear API boundary — "give me a DOM, I'll give you pixels." Emacs has no such
boundary.

`Lisp_Object` — a 64-bit tagged pointer — is the universal currency of the entire
codebase. Its internal bit layout (3-bit LSB tag, pointer in upper bits) is known to
every file:

- **134 `.c` files** reference `Lisp_Object` (100% of `src/`)
- **57 `.h` files** reference it
- **13,535 total occurrences** across the codebase

Every DEFUN directly accesses cons cells, symbol fields, and vector internals through
macros like `XCAR`, `XSYMBOL`, `AREF`. The representation IS the interface. You cannot
put an abstraction layer in the middle without destroying performance in an already-slow
runtime.

### 2. The Coupling Numbers

| Dimension                                    | Count        |
|----------------------------------------------|--------------|
| `.c` files referencing `Lisp_Object`         | **134**      |
| `DEFUN` C primitives exposed to Lisp         | **1,909**    |
| `DEFSYM` interned symbols                    | **2,336**    |
| Ordered `syms_of_*` init functions           | **82+**      |
| `.c` files using specpdl / handler chain     | **73**       |
| Pseudovector types GC must trace             | **28+**      |
| Specpdl entry kinds GC must mark             | **12**       |
| Cross-module `Fxxx` calls from eval.c alone  | **44**       |
| GC root-marking subsystem callbacks          | **15+**      |

A rewritten Elisp core must either keep all 1,909 DEFUN functions compatible with the
new runtime (impossible without sharing the same `Lisp_Object` representation), or
rewrite them all — touching 107 files.

### 3. Circular Dependencies in the Core

The eval/data/buffer triangle has **bidirectional coupling**:

1. **eval.c → data.c**: `specbind()` calls `set_internal()` and `find_symbol_value()`
   for variable binding.
2. **data.c → eval.c**: `set_internal()` calls `let_shadows_buffer_binding_p()` to
   check if a buffer-local variable is shadowed by a `let` binding on the specpdl.
3. **eval.c → buffer.c**: Buffer-local `let` bindings record which buffer they belong
   to; unwinding checks if the buffer is still alive.
4. **keyboard.c → eval.c (all threads)**: When a KBOARD is deleted, keyboard.c walks
   ALL threads' specpdl stacks to clean up references.
5. **eval.c → X11 backend**: The handler chain stores the X11 error handler depth;
   `unwind_to_catch()` calls `x_unwind_errors_to()`.

You cannot extract eval.c and rewrite it in isolation. It is not a module — it is
connective tissue.

### 4. The GC Must Know Everything

`garbage_collect()` (alloc.c) is a stop-the-world mark-and-sweep collector that must
trace every live `Lisp_Object` in the system:

**Per-thread state** (from `mark_one_thread()` in thread.c):
- The specpdl stack (12 different entry types, each with different fields to mark)
- The handler chain (tag, value per handler)
- The bytecode stack (all live frames)
- The C stack (conservative scanning — every pointer-aligned word)

**Global roots** (15+ subsystem callbacks):
```
visit_static_gc_roots()    mark_lread()          mark_terminals()
mark_kboards()             mark_threads()        mark_charset()
mark_composite()           mark_profiler()       mark_fringe_data()
mark_fns()                 mark_pgtkterm()       xg_mark_data()
mark_xterm()               mark_xselect()        ...
```

Each of the 28 pseudovector types (`PVEC_BUFFER`, `PVEC_FRAME`, `PVEC_WINDOW`,
`PVEC_CLOSURE`, etc.) requires special-case handling in the mark phase. Rewriting the
GC means understanding and correctly tracing every one of these types — a single missed
field means silent corruption.

### 5. The DEFUN/DEFSYM Registration System

At startup, 82+ `syms_of_*` functions are called from `main()` in `emacs.c` in a
carefully ordered sequence. Each calls `defsubr()` to register C functions into the
Lisp world, `DEFSYM()` for symbols, and `DEFVAR_LISP`/`DEFVAR_BOOL`/`DEFVAR_INT` for
variables. The ordering matters — e.g., `syms_of_data` must precede `syms_of_eval`
because data.c defines error condition symbols that eval.c uses.

A new runtime must either replicate this initialization chain exactly, or provide an
equivalent mechanism that 1,909 DEFUN functions and 2,336 DEFSYM symbols can register
through.

### 6. What Happened to Remacs

The Remacs project (Rust Emacs rewrite) started by porting simple leaf functions — type
predicates, string operations. These are genuinely easy, just transliteration. After
~100 functions, they hit the core: eval.c, alloc.c, xdisp.c, keyboard.c.

Three walls they couldn't get past:

1. **The GC wall**: Either rewrite the entire allocator + all 28 pseudovector types in
   Rust (massive scope), or keep the C GC and have Rust code live under C's memory
   model (defeating the purpose of Rust).
2. **The eval↔everything wall**: The evaluator has circular dependencies with data.c,
   buffer.c, keyboard.c, and even the X11 backend. You can't port half of it.
3. **The representation wall**: Rust's type system wants to own memory layout, but
   `Lisp_Object` is a raw tagged pointer whose bit layout is hardcoded into every file.
   Wrapping it in a Rust type adds overhead; exposing it as raw bits negates Rust's
   safety.

The project's own FAQ (by maintainer Nick Drozd) admitted: "I don't know of anyone who
has switched completely from using Emacs to using Remacs," including the developers
themselves. It functioned primarily as an educational exercise.

### 7. What Does Work

Approaches that succeed have a **clean boundary**:

- **Neomacs**: Keeps the Elisp core in C, rewrites the rendering/display in Rust. The
  boundary is a `FrameGlyphBuffer` sent over a channel — a clean data interface.
- **native-comp**: Compiles Elisp to native code via libgccjit at package-install time.
  Doesn't change the runtime, just eliminates bytecode dispatch overhead.
- **Rune**: Rewrites from scratch, bottom-up, starting with the hard parts (GC,
  bytecode VM). Honest about the scope — it means reimplementing everything.

---

## Part II: Why the Elisp Machine Is Slow

### 1. Every Operation Pays for Dynamic Typing

`Lisp_Object` is a 64-bit tagged pointer (lisp.h:498). The low 3 bits encode the type:

| Tag   | Type             |
|-------|------------------|
| 0     | Symbol           |
| 2, 6  | Fixnum (two tags for extra range, ~±2^61) |
| 3     | Cons             |
| 4     | String           |
| 5     | Vectorlike       |
| 7     | Float            |

Every operation must check these tags at runtime. `(+ a b)` with two fixnums:

**Bytecode fast path** (bytecode.c:1331, `Bplus` opcode):
```c
if (FIXNUMP(v1) && FIXNUMP(v2)
    && (res = XFIXNUM(v1) + XFIXNUM(v2),
        !FIXNUM_OVERFLOW_P(res)))
    TOP = make_fixnum(res);
else
    TOP = Fplus(2, &TOP);
```
**3 branches** (FIXNUMP? FIXNUMP? overflow?) for the common case.

**Generic path** through `Fplus` → `check_number_coerce_marker` × 2 →
`arith_driver` → overflow check → `make_int`: **~10 branches**.

Compare: a native `ADD` instruction is 1 cycle, 0 branches.

There are **no specialized fixnum bytecodes**. The byte compiler does zero type
inference. `(+ x 1)` always compiles to `Bvarref` + `Bconstant` + `Bplus` — the
same opcodes regardless of whether `x` is known to be an integer.

### 2. No Inline Caching

**Variable access** (`Bvarref`, bytecode.c:621):
```c
Lisp_Object v1 = vectorp[arg], v2;
if (XBARE_SYMBOL(v1)->u.s.redirect != SYMBOL_PLAINVAL
    || (v2 = XBARE_SYMBOL(v1)->u.s.val.value, BASE_EQ(v2, Qunbound)))
    v2 = Fsymbol_value(v1);
PUSH(v2);
```
**2 pointer chases + 2 branches** on every access. If the symbol is buffer-local or
forwarded (`redirect != SYMBOL_PLAINVAL`), it falls through to `Fsymbol_value()` — a
full function call with a 4-way switch on the redirect type.

**Function calls** (`Bcall`): every call re-resolves `symbol → function cell` by
dereferencing `XBARE_SYMBOL(fun)->u.s.function`.

There are zero monomorphic, polymorphic, or megamorphic inline caches. No hidden
classes. No shape transitions. Every access starts fresh. Modern VMs (V8, SpiderMonkey,
LuaJIT) use inline caches to amortize symbol resolution to near-zero cost after the
first access.

### 3. Enormous Function Call Overhead

Every `Ffuncall` (eval.c:3159) does **15-18 branches and 10+ memory writes** before
reaching user code:

| Step                                    | Cost                     |
|-----------------------------------------|--------------------------|
| `maybe_quit()` — check quit + signals   | 2 reads, 1 branch       |
| `++lisp_eval_depth` + depth check       | 1 write, 1 branch       |
| `record_in_backtrace()` — 5 fields      | 5 writes, 1 branch      |
| `maybe_gc()` — check allocation counter | 1 read, 1 branch        |
| `debug_on_next_call` check              | 1 read, 1 branch        |
| Symbol → function resolution            | 2 branches, 1 chase      |
| Type dispatch (SUBRP? CLOSUREP? ...)    | 3-8 branches             |
| On return: depth--, debug, specpdl pop  | 3 operations             |

A tight loop calling a trivial function spends most time on bookkeeping.

The bytecode interpreter has a **fast path** for bytecode-to-bytecode calls
(bytecode.c:809, `goto setup_frame`) that avoids C-level recursion by reusing the
bytecode stack — but still does ~8 branches and 10+ writes per call.

For **interpreted closures** (non-bytecode), `funcall_lambda` must walk the argument
list as a Lisp list, calling `specbind()` per parameter. A 3-argument interpreted
function call does ~25-30 branches just for argument binding.

### 4. Stop-the-World, Non-Generational GC

`garbage_collect()` (alloc.c:5779) is fully stop-the-world mark-and-sweep:

- **No generational collection**: marks the ENTIRE heap every cycle. No young/old
  generation split. No write barriers. A 100MB heap gets fully traced even if only
  1KB was allocated since the last GC.
- **Conservative stack scanning** (alloc.c:4997): walks every pointer-aligned word on
  the C stack, performing a **red-black tree lookup** (`mem_find`) per word to check if
  it points to a heap object. This is O(stack_depth × log(heap_blocks)).
- **Conservative scanning prevents moving GC**: cannot distinguish real pointers from
  integers that coincidentally look like pointers, so objects can never be relocated.
  This prevents compacting, prevents bump-pointer allocation, and causes heap
  fragmentation.
- **Separate sweep per type**: `gc_sweep()` calls `sweep_conses()`, `sweep_strings()`,
  `sweep_floats()`, `sweep_symbols()`, `sweep_vectors()`, etc. — each walks ALL blocks
  of its type.

GC is triggered when `consing_until_gc < 0` (default threshold ~800KB). The check
(`maybe_gc()`) runs on every function call and every ~256 loop iterations.

### 5. Cache-Hostile Allocation

Cons cells use a **free-list allocator** (alloc.c:2604):
```c
if (cons_free_list) {
    XSETCONS(val, cons_free_list);
    cons_free_list = cons_free_list->u.s.u.chain;
}
```
Freed cons cells are scattered across memory. Newly allocated conses go to random
locations in previously-freed blocks. This is **cache-hostile** — traversing a freshly
built list touches memory all over the heap.

A bump-pointer allocator (like V8's young generation) would give sequential,
cache-local allocation. But conservative GC prevents a moving collector, which prevents
bump allocation.

`(list a b c d)` calls `Fcons` **4 times** — 4 free-list pops, 4 `consing_until_gc`
decrements, 4 potentially scattered memory locations.

### 6. String Representation Is Expensive

Strings (`struct Lisp_String`, lisp.h:1558) have structural overhead:

- **Two allocations per string**: a header (from free list) and a separate `sdata`
  block for character data. The data pointer is a separate heap pointer, not inline.
- **Two pointer chases** to read content: `Lisp_Object` → `struct Lisp_String *` →
  `data`.
- **Multibyte character indexing is O(n)**: `string_char_to_byte()` scans from the
  start to convert character index to byte index. No index table for O(1) access.
- **Every `concat` copies**: `(concat "a" "b" "c")` allocates a new string and
  memcpys all data. No ropes, no copy-on-write, no string builder. Incremental string
  building in a loop is O(n^2).

### 7. The Interpreter Dispatch Tax

Even with computed gotos (GCC), each bytecode dispatch is:
```
FETCH (*pc++)  →  targets[op]  →  indirect jump
```
The indirect jump causes frequent **branch target buffer (BTB) mispredictions** on
modern CPUs. The BTB can only predict one target per source address, but every opcode's
`NEXT` jumps to a different destination depending on the next opcode. With ~168
bytecodes and a 256-entry dispatch table, prediction accuracy is poor.

Misprediction penalty: ~5-20 cycles per opcode dispatch on modern x86.

The interpreter manually pins `top` and `pc` to registers (bytecode.c:455):
```c
#define BC_REG_TOP asm ("rbx")
#define BC_REG_PC asm ("r12")
```
This is a workaround for GCC's poor register allocation in the giant function — a
sign of fighting the compiler rather than having a clean design.

### 8. No JIT, No Specialization

There is **zero JIT compilation**. The `native-comp` feature (libgccjit) is
ahead-of-time, not runtime-adaptive. It compiles the same unspecialized bytecodes to
native code — eliminating dispatch overhead but not type-check branches, GC pressure,
or symbol resolution cost.

There is no:
- Type-specializing JIT (like V8 TurboFan or LuaJIT)
- On-stack replacement (OSR) for hot loops
- Speculative optimization with deoptimization
- Profile-guided type feedback
- Escape analysis or scalar replacement

---

## Per-Operation Cost Summary

| Operation               | Elisp Cost                          | Native Equivalent       |
|-------------------------|-------------------------------------|-------------------------|
| `(+ a b)` two fixnums   | 3 branches (bytecode), 10 (generic) | 1 ADD instruction       |
| `(funcall f a b)`       | 15-18 branches, 10+ writes          | 1 CALL instruction      |
| Variable lookup         | 2 pointer chases, 2 branches        | 1 MOV from known offset |
| `(cons a b)`            | Free-list pop, scattered memory     | Bump pointer (1 ADD)    |
| `(aref vec i)`          | 5 branches (type checks)            | 1 MOV with index        |
| `(length list)`         | O(n) traversal                      | O(1) field read         |
| `(concat str1 str2)`   | Full copy of both strings           | Pointer/view (0 copy)   |
| GC pause                | Marks entire heap, stops world      | ~microseconds (young gen) |
| Bytecode dispatch       | Indirect jump + BTB misprediction   | Native branch (predicted) |

---

## What Would Fix It

The Elisp machine is slow because it was designed in the 1980s and has had no
fundamental runtime innovation since. Modern dynamic language VMs address these exact
problems:

| Technique                          | Eliminates                          | Used By                     |
|------------------------------------|-------------------------------------|-----------------------------|
| JIT with type specialization       | Branch overhead, type checks        | V8, SpiderMonkey, LuaJIT    |
| Inline caches                      | Symbol resolution per-access        | V8, SpiderMonkey, PyPy      |
| Generational moving GC             | Full-heap pauses, fragmentation     | V8, JVM, .NET               |
| Bump-pointer allocation            | Free-list overhead, cache misses    | V8, JVM                     |
| Hidden classes / shapes            | Property lookup indirection         | V8, SpiderMonkey             |
| Escape analysis                    | Unnecessary heap allocation         | JVM HotSpot, V8 TurboFan    |
| On-stack replacement               | Interpreted hot loops               | JVM, V8                      |
| Copy-on-write / ropes              | String copy overhead                | Java (interning), Ropes libs |

Emacs `native-comp` addresses only bytecode dispatch overhead — it is a speedup but
not an architectural fix. The type system, GC, allocation strategy, and lack of runtime
feedback remain unchanged.

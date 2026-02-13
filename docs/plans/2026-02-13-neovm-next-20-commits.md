# NeoVM Next 20 Small Commits Queue

This queue is ordered, compatibility-first, and sized for small commits.
Each commit should be pushed immediately after local verification.

Global gate for every commit:

```bash
make -C test/neovm/vm-compat check-neovm FORMS=<changed-case>.forms EXPECTED=<changed-case>.expected.tsv
```

Periodic full gate (after commits 5, 10, 15, 20):

```bash
make -C test/neovm/vm-compat check-all-neovm
```

## Commit Queue

1. `vm-compat: add autoload-macro introspection corpus`
- Change: add `cases/autoload-macro-introspection.forms` + `.expected.tsv` + Makefile entry.
- Focus: `macrop`, `symbol-function`, `indirect-function` for autoloaded macros.

2. `neovm: make macrop recognize autoload macro wrappers`
- Change: minimal logic in `rust/neovm-core/src/elisp/subr_info.rs` and/or `builtins.rs`.
- Focus: return `t` when function cell is macro-autoload equivalent.

3. `vm-compat: add functionp callable-object matrix`
- Change: new corpus for symbol vs subr vs lambda vs macro list forms.
- Focus: lock current behavior to oracle.

4. `neovm: tighten functionp around macro marker objects`
- Change: small fix in `builtin_functionp_eval` path.
- Focus: reject non-callable macro markers consistently.

5. `vm-compat: add special-form edge corpus (inline/interactive/etc)`
- Change: new corpus for public special forms vs evaluator-only forms.
- Focus: prevent future special-form drift.

6. `neovm: normalize special-form table comments and invariants`
- Change: refactor-only/clarity in `subr_info.rs` with debug assertions.
- Focus: no behavior change, improve maintainability.

7. `vm-compat: add func-arity coverage for macro fallbacks`
- Change: new corpus for `when/unless/dotimes/...` arity shapes.
- Focus: keep fallback arity oracle-aligned.

8. `neovm: centralize fallback macro arity metadata`
- Change: deduplicate arity mapping helper in `subr_info.rs`.
- Focus: single source of truth for fallback macro arity.

9. `vm-compat: add fboundp macro-boundary corpus`
- Change: new case for macro-only names and unresolved names.
- Focus: lock `fboundp` boundary behavior.

10. `neovm: make fboundp reuse fallback-macro classifier`
- Change: small internal wiring cleanup in `builtins.rs`.
- Focus: avoid divergent logic paths.

11. `vm-compat: add symbol-function error-path corpus`
- Change: new case for void/non-symbol type errors and noerror paths.
- Focus: oracle-checked error symbols.

12. `neovm: align symbol-function wrong-type and void-function edges`
- Change: minimal fix in `builtin_symbol_function`.
- Focus: exact error signaling compatibility.

13. `vm-compat: add indirect-function alias-loop/chain corpus`
- Change: new corpus for alias chains + noerror handling.
- Focus: lock indirect resolution behavior.

14. `neovm: harden indirect-function alias traversal limits`
- Change: small safety fix in alias traversal (cycle-safe behavior).
- Focus: compatibility + robustness.

15. `vm-compat: add condition-case/unwind micro corpus`
- Change: new corpus for non-local exits through introspection call sites.
- Focus: regression guard around control flow.

16. `neovm: improve invalid-function diagnostics for introspection paths`
- Change: minimal error payload alignment.
- Focus: better compatibility for downstream predicates/tests.

17. `ci(vm-compat): add focused introspection suite target`
- Change: Makefile target grouping new introspection cases.
- Focus: faster local/CI loop for this area.

18. `docs: update elisp-vm-design with new introspection compatibility guarantees`
- Change: small docs update in `docs/elisp-vm-design.md`.
- Focus: reflect implemented behavior and oracle coverage.

19. `bench: add introspection microbench scaffold`
- Change: add tiny benchmark example under `rust/neovm-core/examples/`.
- Focus: baseline perf tracking for predicate-heavy paths.

20. `chore(vm-compat): freeze this batch and record pass artifact notes`
- Change: append short pass summary in `test/neovm/vm-compat/README.md`.
- Focus: reproducibility and handoff notes.

## Execution Notes

- Keep commits tiny; avoid mixing unrelated fixes.
- If a commit needs oracle update, regenerate only the touched case.
- Do not touch unrelated working-tree changes.

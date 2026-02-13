# Neovm Untracked Elisp Port Inventory

This document tracks the currently untracked Elisp port files under `rust/neovm-core/src/elisp/`.

## What These Files Are

These files are direct/partial Rust ports of specific Emacs C domains.
They are not wired into `elisp/mod.rs` yet, so they are not active in the main build path.

## Module Map

- `buffer.rs` (+ `buffer/`): buffer management builtins, overlay stubs, buffer-local helpers.
- `callproc.rs` (+ `callproc/`): subprocess and environment variable builtins.
- `character.rs`: character width/direction/conversion builtins.
- `data.rs`: symbol/variable/type operations, including buffer-local variable stubs.
- `dispnew.rs`: redisplay/sleep/sit-for related builtins, mostly stubs.
- `keyboard.rs`: event/input/command-key builtins, largely stubs.
- `terminal.rs`: terminal/display capability query builtins for GUI defaults.
- `xfaces.rs`: face/color/font related builtins, primarily stub behavior.

## Current Organization Status

- Refactored to submodules:
  - `buffer.rs` -> `buffer/{args.rs,pure.rs,stateful.rs,tests.rs}`
  - `callproc.rs` -> `callproc/{args.rs,env.rs,process.rs,tests.rs}`
  - `character.rs` -> `character/{helpers.rs,builtins.rs,tests.rs}`
  - `data.rs` -> `data/{helpers.rs,pure.rs,eval_stateful.rs,tests.rs}`
  - `dispnew.rs` -> `dispnew/{args.rs,pure.rs,timing.rs,tests.rs}`
  - `keyboard.rs` -> `keyboard/{args.rs,pure.rs,eval_stateful.rs,tests.rs}`
  - `terminal.rs` -> `terminal/{args.rs,terminal_builtins.rs,tty_builtins.rs,display_builtins.rs,tests.rs}`
  - `xfaces.rs` -> `xfaces/{args.rs,builtins.rs,tests.rs}`
- Still monolithic and pending split:
  - none

## Integration Note

Before enabling these ports in `elisp/mod.rs`, each module should be reviewed for duplicate builtins already implemented in tracked modules (for example `process.rs`, `display.rs`, `encoding.rs`, and existing buffer/data functionality).

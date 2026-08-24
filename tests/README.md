# Emulator regression sources

Build the source variables from the repository development shell:

```sh
nix develop -c python3 fmake.py tests/kernel.fs --assemble
nix develop -c python3 fmake.py tests/interp.fs --assemble
```

Load `FORTH.8xp`, an `Asm(prgmFORTH)` BASIC wrapper, and the generated source
variable into a TI-84 Plus emulator. Run `LOAD KERNEL` or `LOAD INTERP`.

`kernel.fs` writes its failure count to `ABS @` and writes `$1234` to
`ABS 2+ @` on completion. Even offsets from `ABS+4` through `ABS+14` contain
cumulative per-section failure counts; `ABS+44` onward diagnoses the
double-cell section. A passing run has zero failures and the completion magic.

`interp.fs` uses a failure count at `ABS+64` and writes an early `$1234`
checkpoint at `ABS+66`. The checkpoint makes the dictionary/`FORGET` path
observable even in accelerated emulator runs that reach the OS auto-power-down
timer before later parser checks finish; it is not an end-of-suite marker.

`ABS` is logical address `$9872`. A physical TilEm RAM dump must be interpreted
using the active RAM page mapping; in the tested OS 2.55MP run it appeared at
raw RAM offset `$5872`. Do not hard-code that physical offset for other models
or mappings.

# Emulator regression sources

## Flash App build and persistence

`nix build` assembles both package formats and runs `check_flash_app.py` on the
raw App page and its labels. The check pins the one-page header, App entry,
Flash-resident dispatch/lifecycle code, the 640-byte workspace boundary, return
stack separation, and maximum dictionary address without requiring a ROM.

`flash-app-smoke.macro` is the end-to-end TI-OS test for a clean TI-84 Plus OS
2.55MP image after transferring `forth.8xk`. It assumes Finance is App 1 and
TI84FTH is App 2. The macro defines `42 CONST ANSWER`, executes it, writes a
snapshot, exits (which writes the other slot), relaunches the App, and executes
`ANSWER .` again. The final screen must contain `ANSWER . 42 ok`, and the ROM
dump must contain two committed `TIF4` records with consecutive generations,
matching payloads, and valid CRC16-CCITT values.

The macro uses alpha lock plus `scanstring` because per-character `type`
commands do not reliably reproduce modifier timing in the Forth line editor.
It also avoids treating App `ABS` as persistent: `ABS` is volatile workspace
scratch and is cleared at every App launch.

The macro is intentionally not part of `nix build`: a TI-OS ROM is copyrighted
and not distributed by this repository, and upstream TilEm does not provide a
deterministic command-line App-transfer/launch runner. Use an identified ROM
and record the emulator build when reporting results.

The 2026-08-25 verification used TI-84 Plus OS 2.55MP ROM SHA-256
`dbb47afae091ab36f9abe74e32083013fbeff3d7e0516bbf5d1abf4ee57adc09`
and TilEm commit `d1bdc58dd321ae462a701e556fcb62bb925a78b1` with local headless
macro/transfer support. The local transfer runner sometimes left the first App
status byte at `00` instead of completing it to `80`; the test run completed
that one monotonic status transition before launch. That runner limitation was
also reproduced with an unrelated known-good App.

## Legacy program suites

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

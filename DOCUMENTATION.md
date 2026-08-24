# Forth word reference

Cells are 16-bit unsigned integers and addresses. Arithmetic wraps modulo
65536. Booleans are `0` and `1`. Unless stated otherwise, behavior on stack
underflow, division by zero, or invalid addresses is undefined. The Flash App
checks dictionary growth; the legacy assembly program does not.

The implementation is intentionally small and is not a complete ANS Forth.
Notable differences are called out below. `WORDS` is the authoritative live
word list; commented-out assembly experiments are not part of the dictionary.

## Stack and arithmetic

- `DUP ( a -- a a )`, `DROP ( a -- )`, `SWAP ( a b -- b a )`,
  `OVER ( a b -- a b a )`, `ROT`, `-ROT`, `NIP`, and `TUCK` provide the usual
  cell-stack operations.
- `2DROP`, `2DUP`, `2SWAP`, and `2OVER` operate on cell pairs.
- `+`, `-`, `1+`, `1-`, `2+`, `2-`, `*`, `NEGATE`, `AND`, `OR`, `XOR`, and
  `INVERT` wrap to 16 bits. `<<` and `>>` shift by exactly one bit; they do not
  consume a shift count.
- `/MOD ( dividend divisor -- remainder quotient )`, `/`, and `MOD` are
  unsigned. `SQRT` is an integer square root.
- `=`, `<>`, `<`, `>`, `<=`, and `>=` are unsigned comparisons. `0=` tests for
  zero. `WITHIN ( n low high -- flag )` is inclusive at both endpoints.
- Double cells are written high cell first, low cell second. `UM* ( a b -- high
  low )`, `D+ ( h1 l1 h2 l2 -- h l )`, `M+ ( high low n -- high low )`, `DS
  ( high low u8 -- high low )`, and `D/MOD ( high low divisor -- remainder
  quotient-high quotient-low )` use unsigned arithmetic.

`SP@`, `SP!`, `RP@`, `RP!`, `>R`, `R>`, `R@`, `2>R`, `2R>`, `RDROP`, and
`2RDROP` expose the VM stacks directly. `DEPTH` reports parameter-stack depth;
`.S` prints it without consuming it.

## Memory and strings

- `! ( value address -- )`, `@ ( address -- value )`, `+!`, and `-!` access
  cells. `C!` and `C@` access bytes. `C@C! ( source destination -- )` copies a
  byte.
- `CMOVE ( source destination count -- )` copies from low to high addresses;
  it is overlap-safe when the destination is below the source. `CMOVE>` copies
  high to low and is overlap-safe when the destination is above the source.
  A zero count is a no-op for both.
- `STRLEN`, `STRCHR`, `STR=`, `TELL`, `PUTS`, and `PUTLN` operate on
  NUL-terminated byte strings.
- `S"` parses a quoted string. In interpretation it returns `address length`;
  during compilation it emits an inline string. `."` prints an inline quoted
  string.
- `BUF` names the 64-byte terminal buffer. `GETS` reads at most 63 characters
  and NUL-terminates it. `GETC` returns the next byte and returns zero at the
  end; `UNGETC` backs up the current input pointer by one byte.
- `WORD ( -- address length )` skips spaces, tabs, newlines, and backslash
  comments. Names are safely truncated to the dictionary's 31-character
  header limit while the remainder of the token is consumed.

## Terminal and display

- `KEY` blocks in TI OS `_GetKey` and returns a cooked key code, including
  2nd/ALPHA state. `KEYC` calls nonblocking `_GetCSC` and returns a raw scan
  code. These number spaces are different.
- `AKEY` blocks until a supported cooked key maps to ASCII. `TO_ASCII ( key --
  character )` performs the same table lookup without reading a key.
- The line editor recognizes both `kEnter` and `kAlphaEnter`. Left deletes,
  right inserts a space, and CLEAR erases the current line. ENTER is echoed as
  a space. `ok` appears when `WORD` exhausts its current input and requests a
  new terminal line.
- The direct `STO▶` key types `!`; `2nd` then `STO▶` (`RCL`) types `@`.
  TI-OS opens menus for `2nd PRGM` and `2nd APPS` before the Forth terminal can
  consume those chords, so they are not punctuation bindings.
- `EMIT`, `SPACE`, `SPACES`, `CR`, `PUTS`, and `PUTLN` use the OS large-font
  text routines. `EMITS` uses the small-font routine. `AT-XY ( row column -- )`
  sets `curRow`/`curCol`; `ATS-XY` sets the small-font pen coordinates.
- `PAGE` clears the LCD. `INVTXT` toggles inverse text and `TOG-SCRL` toggles
  scrolling. `PLOT` copies `plotSScreen` to the LCD.

## Dictionary and compiler

- `FIND ( address length -- header|0 )` searches the linked dictionary and
  ignores hidden entries. `>NFA`, `>CFA`, `>DFA`, `CFA>`, `?IMMED`, and
  `?HIDDEN` inspect headers.
- `CREATE ( address length -- )` accepts names of 1 through 31 bytes. Invalid
  direct calls are ignored. `:` and `;` define colon words; `IMMED`, `HIDDEN`,
  `HIDE`, `LITERAL`, `RECURSE`, `DOES>`, and `(DOES>)` support defining words.
- `HIDE name` and `FORGET name` are no-ops if the name is absent. `FORGET`
  refuses to rewind into the built-in image below `H0`.
- `,`, `C,`, `ALLOT`, `CELLS`, `HERE`, `LATEST`, `STATE`, `[` and `]` expose
  compilation state. In this implementation `STATE=0` means compiling and
  `STATE=1` means interpreting. In App mode, positive growth is bounds-checked
  and negative `ALLOT` cannot move below `H0`.
- `IF`/`ELSE`/`THEN`, `BEGIN`/`UNTIL`/`AGAIN`/`WHILE`/`REPEAT`, and
  `CASE`/`OF`/`ENDOF`/`ENDCASE` are immediate compile-time words.
- `DO`/`LOOP` and `DO`/`+LOOP` use `I` and `J` for loop indices. `+LOOP`
  finishes only when the updated index equals the limit; it does not implement
  ANS boundary-crossing semantics.
- `CONST` and `VAR` are defining words. `PICK`, `CHAR`, `'`, `EXECUTE`, `SEE`,
  and `WORDS` provide the expected interactive facilities.
- `PARSE-NUM ( nul-string -- n )` parses unsigned decimal modulo 65536 and sets
  `NUMST` to `1` on success or `0` on failure. `NUM?` tests one ASCII decimal
  digit. `HEX` and `DEC` affect numeric output; input remains decimal.

## Storage, blocks, graphics, and exit

- In the Flash App, `SCR` and `H0` are the beginning of the dynamically sized
  user dictionary at `$A015`. `CAPACITY` reports its total byte capacity,
  `USED` reports `HERE-H0`, and `AVAILABLE` reports `CAPACITY-USED`. On the
  measured clean OS 2.55MP state, `CAPACITY` is 11,442 bytes; it varies with
  free RAM at launch and is capped at 16 KiB.
- In the Flash App, `WB` and `SIMG` write a CRC-protected generation to the
  inactive `FTHSAVA`/`FTHSAVB` AppVar and archive it. `LIMG` selects the newest
  structurally valid, checksum-valid generation and resets the terminal around
  it. `BYE` and TI-OS put-away save automatically. A system error does not
  replace the last valid snapshot.
- In the Flash App, `ABS` returns a private 128-byte arena scratch area and
  `UALT` is a compatibility no-op. In the legacy `.8xp`, `ABS` returns
  `appBackUpScreen` (`$9872`, 768 bytes), `UALT` changes `HERE` to that OS
  scratch region, and `WB`/`SIMG`/`LIMG` operate on the fixed 354-byte data
  segment (350 dictionary bytes plus saved `LATEST` and `HERE`).
- `PLOTSS` returns `plotSScreen` (`$9340`, 768 bytes) in both builds.
- `CBLK ( name length -- data|0 )` creates a 255-byte normal program and `FBLK
  ( name length -- data|0 )` finds normal or protected programs resident in
  RAM. Archived programs return `0`; unarchive them before `FBLK` or `LOAD`.
  Names must contain 1 through 8 bytes. Returned addresses skip the program's
  two-byte size field.
- `RUN ( source -- )` switches the interpreter to a NUL-terminated source.
  `LOAD name` combines `FBLK` and `RUN`; when source ends, the interpreter
  refills from the keyboard.
- `SMIT ( frequency duration -- )` drives the link port for sound. `PLOT`, `WR`,
  `TELLS`, `CSCR`, and the coordinate/display words are calculator-specific.
- `QUIT` resets the parameter stack and refills the terminal. `BYE` restores
  the saved OS stack and returns to the caller.

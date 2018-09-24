# A Forth for the TI-83+/TI-84+

Hi!  This is an ongoing project to bring a Forth to the TI-83+/TI-84+
calculator series.

## Why TI-83+/TI-84+?
This is a calculator that is more or less ubiquitous among high school
students throughout the world.  It's not going extinct anytime soon
(expect perhaps to newer models such as the TI-84 CE).  They're solid
calculators, but they have notable disadvantages.  You can't really
customize it.  There's no REPL to define procedures or perform tasks.
Variables are limited... the list goes on.  The TI calculators are the
microcomputers of today; so it makes sense to learn it at its lowest
levels---starting with assembly.

## Why Forth?
Assembly is painful to program in.  Programs crash at the slightest
hint of error, without rhyme or reason.  There's really no way to make
an interactive REPL without creating the READ, EVAL and PRINT parts of
it.

Wouldn't it be great to have a programming language on the
TI-83+/TI-84+ that's much faster than TI-BASIC but easier to
understand and as low level as assembly?  Forth is just that.  (read
_Starting FORTH_ for an excellent introduction to Forth).  It's low
level, it's simple, but most importantly it's _easy to type_,
especially when you're on a calculator with a non-QWERTY keyboard.  It
is a very powerful language, allowing you to do things like change the
syntax of the language itself.  `IF`, `WHILE`, `CONSTANT`
etc. statements are all implemented in Forth!  Think of it as a
untyped C with a REPL and the lower of Lisp macros.

It's also easy to implement incrementally, testing as you go along.  Once
I/O is done, the rest of Forth can be implemented in itself.
## Requirements
- [spasm-ng Z80 assembler](https://github.com/alberthdev/spasm-ng)
  - If you're on a Mac you may need to run the following commands.
  - Compile the assembler with `make` (check required packages and so
    on).
- A TI-84 calculator!
  - I haven't tested this program on any emulator.

```shell
brew install openssl
cd /usr/local/include
ln -s ../opt/openssl/include/openssl .
```

## Building
Once you have compiled spasm-ng, copy the executable into the same
directly as this assembly file.  Also make sure to copy the `inc/`
directory from the spasm-ng repository.  Once that's done, run:
```shell
./spasm -T forth.asm forth.8xp
```
The `-T` option is useful for debugging macro expansion, it's
optional.

## Features
- 16-bit Forth on a 8-bit chip
- Direct-threaded
- Programs are lists of 16-bit addresses
- Takes advantage of system calls such as `_GetKey`, `_PutC` and more
  to come (such as drawing pixels!)

## Example Programs
Let's make a program to interactively display the keypresses of the
user.  We have an `AKEY` word that returns the calculator keypress as
an ASCII number on the stack using a conversion table (see `key_table`
for the exact conversion).  The following is a valid program that you
can enter into the calculator.

```forth
\ comments can be omitted, shown here for educational purposes
: SHOW_KEYS \ define a new word called SHOW_KEYS
        AKEY \ read an ASCII character from the user
        BEGIN
                DUP 0 <> \ test if the last key entered was not ENTER
                         \ , as AKEY returns 0 in such a case
        WHILE
                DUP . SPACE SPACE EMIT CR \ duplicate the character,
                                          \ type two spaces, print
                                          \ it and type a newline
                AKEY \ read another character
        REPEAT \ repeat the body while the condition is true
        DROP \ the last key entered was ENTER, so drop it and return
;
```

## Available Words
```text
EXIT FOO BAR DUP + - AND OR XOR INVERT DROP SWAP OVER
ROT -ROT 2DROP 2DUP 2SWAP 1+ 1- 4+ 4- >R R> RDROP LIT !
@ +! -! C! C@ C@C! CMOVE EXECUTE BASE STATE LATEST S0 x
] ?SE HERE DOCOL BUF WBUF R0 F_IMMED F_HIDDEN F_LENMASK '
, SP@ SP! BRANCH 0BRANCH ?DUP = <> >= < > 0= RAND ASK
UPRESS KEY EMIT . ? AKEY TO_ASCII * TESTA 0 1 2 3 4 5
6 7 8 9 10 TS SPACE CR AT-XY PUTS PUTLN GETS GETC WORD
IMMED? IMMED >NFA >CFA STR= FIND >DFA CREATE DOCOL_H : ;
PAGE HIDDEN ?HIDDEN HIDE IF THEN ELSE BEGIN UNTIL AGAIN
WHILE REPEAT CHAR [COMP] CONST ALLOT CELLS VAR BYE STAR
```

## Screenshots
### Freedom, on your calculator.
![Read memory.  One byte at a time.](repl3.png)

### TI-84+ inside
![key-prog program](demo2.png)

### An interactive REPL
![REPL demonstration](repl1.png)

### Define new words...
![Defining DOUBLE](repl4.png)

### ...and use them.
![Using DOUBLE](repl5.png)

## Design Notes
### Use of Macros
Judicious use of macros has greatly improved readability of the code.
This was directly inspired by the _jonesforth_ implementation (see
Reading List).
### Register Allocation
One notable features of this Forth is the use of a register to keep
track of the top element in the stack.

| Z80 Register | Forth VM Register             |
| :---:        | :---:                         |
| DE           | Instruction pointer (IP)      |
| HL           | Working register (W)          |
| BC           | Top of stack (TOS)            |
| IX           | Return stack pointer (RSP)    |
| SP           | Parameter stack pointer (PSP) |
### Reading List
Documentation can vary from very well-documented to resorting to
having to read the source code of `spasm-ng` to figure out how
`#macro` worked.  See examples such as `defcode` and `defword`.  I
couldn't make `defconst` or `defvar`, however, but this was fixed by
writing it out manually.

- [General Z80 guide](http://jgmalcolm.com/z80/#advanced)
- [Moving Forth](http://www.bradrodriguez.com/papers/moving1.htm)
- [Learn TI-83 Plus Assembly In 28 Days](http://tutorials.eeems.ca/ASMin28Days/welcome.html)
- [KnightOS Kernel](https://github.com/KnightOS/kernel)
- [Starting FORTH](https://www.forth.com/starting-forth/)
- [Jonesforth](http://git.annexia.org/?p=jonesforth.git)

## To be Implemented
- [ ] Ability to read/write programs
- [x] User input
  - [x] String reading routines
  - [ ] Number reading routines
- [x] Output
  - [x] Displaying strings
- [x] Proper support for compile/interpret mode
- [ ] Assembler to convert Forth words into `.dw` data segments to be
pasted into the program.
- [ ] Ability to switch to a "plot"
  - [ ] So you may want to plot something and still go back to the
        REPL when needed.
- [ ] REPL
  - [x] Basic Read/Eval/Print/Loop
  - [x] Allowing more than one word at a time input
  - [ ] Respect hidden flag to avoid infinite looping
  - [ ] Reading numbers (support for 0-10 inclusive hardcoded, but not
        a general algorithm)
- [ ] Document Forth words

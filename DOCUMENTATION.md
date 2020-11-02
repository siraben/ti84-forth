# Forth Word Documentation
All stack elements are 16-bit unsigned integers.
## CODE words
### EXIT ( -- )
### DUP ( n -- n n )
Duplicates the top element of the stack.
### + ( a b -- a+b )
Adds the top two elements of the stack.
### - ( a b -- a-b )
Subtracts the top element of the stack from the one immediately before
it.
### AND ( a b -- a&b )
Bitwise `and` of the top two elements of the stack.
### OR ( a b -- a||b )
Bitwise `or` of the top two elements of the stack.
### XOR ( a b -- a^b )
Bitwise `xor` of the top two elements of the stack.
### << ( a -- a<<1 )
Left shift of the top element of the stack.
### >> ( a -- a>>1 )
Right shift of the top element of the stack.
### INVERT
### DROP ( a b -- a )
Drop the top element of the stack.
### SWAP ( a b -- b a )
Swap the top two elements of the stack.
### OVER ( a b -- a b a )
Push the second-top element of the stack onto the top of the stack.
### ROT
### -ROT
### 2DROP
### 2DUP
### 2SWAP
### 1+ ( n -- n+1 )
Increment the top element of the stack.
### 1- ( n -- n-1 )
Decrement the top element of the stack.
### 2+ ( n -- n+2 )
Increment the top element of the stack by 2.
### 2- ( n -- n-2 )
Decrement the top element of the stack by 2.
### 4+ ( n -- n+4 )
Increment the top element of the stack by 4.
### 4- ( n -- n-4 )
Decrement the top element of the stack by 4.
### >R ( a -- )
### R> ( -- a)
### RDROP ( -- )
### LIT
### LITSTR
### TELL
### STRLEN
### STRCHR
### ! ( val addr -- )
### @ ( addr -- val )
### +!
### -!
### C!
### C@
### C@C!
### CMOVE
### EXECUTE
### BASE
### STATE
### LATEST
### SP0
### [
### ]
### ?SE
### HERE
### DOCOL
### BUF
### BUFSZ
### WBUF
### WBUFSZ
### RP0
### H0
### F_IMMED
### F_HIDDEN
### F_LENMASK
### SCR
### PLOTSS
### '
### ,
### SP@
### SP!
### RP@
### RP!
### BRANCH
### 0BRANCH
### ?DUP
### = ( n1 n2 -- b )
### <> ( n1 n2 -- b )
### >= ( n1 n2 -- b )
### <= ( n1 n2 -- b )
### < ( n1 n2 -- b )
### > ( n1 n2 -- b )
### 0= ( n1 -- b ) 
### RAND ( -- n )
### ASK
### KEY
### KEYC
Non-blocking form of `KEY`.
### EMIT ( a -- )
Print the character with the ASCII code form the top of the stack.
### . ( a -- )
Pop the top element from the stack and print it.
### ? ( addr -- )
Print the value pointed to by `addr`.
### AKEY ( -- a )
Block until a key that corresponds to a printable ASCII character is
given, then push that value onto the stack.
### TO_ASCII ( a -- b )
Convert a value given by `KEY` to the corresponding ASCII character code.
### * ( a b -- a*b )
Multiply the top two elements on the stack.
### /MOD
### CR
Carriage return.
### AT-XY
### PUTS ( addr -- )
Print a NUL-delimited string.
### PUTLN ( addr -- )
Print a NUL-delimited string followed by a carriage return.
### GETS ( -- )
Get characters from the user until `[ENTER]` is pressed, storing the
result in the memory location`BUF`.
### GETC ( -- a )
Get the next character from `BUF`.
### UNGETC ( -- )
Unget the last character from `BUF`.
### WORD ( -- addr len )
Read a full, space-delimited word from `BUF` and push the address of
`WBUF` followed by the length of the word onto the stack.
### ?IMMED ( addr -- b )
Given a pointer returned from `FIND`, return whether or not the word
is marked `IMMEDIATE`.  Follows boolean convention.
### IMMED ( -- )
Mark the last word (or currently-being-defined word) as `IMMEDIATE`.
### >NFA ( fptr -- addr )
Given a `FIND`-returned pointer, return the address of the start of
the word's name string.
### >CFA ( fptr -- addr )
Given a `FIND`-returned pointer, return the address of the start of
the word's Code Field Address (CFA).
### STR= ( addr1 addr2 -- b )
Given two addresses of NUL-terminated string, check whether they are
equal character-by-character.
### FIND ( addr len -- fptr )
Given the address of a word string and its length, return a `FIND`
pointer to the word in the dictionary.  Return `0` if the word is not
found.
### WB
Writeback the (possibly) modified contents from `data_start` to `data_end`.
### CREATE
### DOCOL_H
Writes the three bytes corresponding to `call docol` to the memory
location pointed to by `HERE`.
### (DOES>)
Created by `DOES>`, or can be called as well.  Sets the `call`
destination of the `LATEST` word's Code Field Address to the address
directly after `DOES>`, which is the instruction pointer at the time
`DOES>` is invoked.  See `DOES>` for more information.
### DOES>
Used in words that can create new words.  See the following example:
```forth
: CONSTANT
  WORD CREATE DOCOL_H ,
  DOES> @
;
```

The word `CONSTANT` in the example reads a word, creates the link and
name header, followed by three bytes corresponding to `call docol`,
followed by the top element of the stack.  `DOES>` denotes the end of
`CONSTANT`'s action and the start of the action of what the word
__created by__ `CONSTANT` will do.  In other words, we can use it like
this:

```forth
31415 CONSTANT PI
PI . \ => 31415
```
The words following `DOES>` are executed on the same stack, but with
the top element of the stack begin the address of the word defined by
`CONSTANT`'s children.  That's why we can just deference the pointer
with `@` and thus get the constant value out.

What's happening is that `DOES>` is an immediate word that compiles
`(DOES>)` followed by the 3 bytes representing `call dodoes` to the
current word being defined (i.e. `CONSTANT`).  When `CONSTANT` is
invoked, the invocation of `(DOES>)` sets the destination address of
the `call` instruction in the _new_ word (whatever it may be) being
defined (in this case, `PI`) to the byte _after_ `(DOES>)`, so that
the new word starts its Code Field Address with `call XXXX`,
where `XXXX` is the address after the location of `(DOES>)` in
`CONSTANT`.  Then, `(DOES>)` acts like `EXIT`, resuming execution.

This means you can share the same body code between words created by a
word using `DOES>`, reducing wasted space.

### PAGE
Clear the screen.
### HIDDEN
### ?HIDDEN ( fptr -- b )
Given a `FIND` pointer, return whether or not the word is hidden.
### NIP ( a b -- b )
### TUCK ( a b  -- b a b )
Tucks the top element two locations prior.
### '0'
### '9'
### I
### SMIT ( freq dur --  ) 
Sound emit.  Play the "frequency" with the duration.  The convention
is that the lower the frequency number the higher it actually is in
real life.
### PLAY
### PLOT ( -- )
### GETP
### DARKP
### TOGP
### LITP
### PN
### BYE
Exit the program.

## WORD Words
### SQ
### .Q
### USED ( -- n )
Returns how many bytes have been used (starting from `H0`).
### SIMG
### LIMG
### >DFA
### :
### ;
### MOD
### /
### NEGATE
### TRUE
### FALSE
### NOT
### LITERAL
### ID.
### HIDE
### IF
### THEN
### ELSE
### BEGIN
### UNTIL
### AGAIN
### WHILE
### REPEAT
### CHAR
### (COMP)
### CONST
### ALLOT
### CELLS
### RECURSE
### VAR
### DO
### LOOP
### +LOOP
### FORGET
### WITHIN
### NUM?
### NUM ( -- n )
### CFA>
### PICK
### U. ( n -- )
### UWIDTH
### SPACES ( n -- )
### U.R
### U.
### .
### .S
### SEE
### WORDS
### CASE
### OF
### ENDOF
### ENDCASE
### WR
### STAR

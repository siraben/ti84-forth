# Forth Word Documentation
```
All stack elements are 16-bit unsigned integers.
```
## CODE words
### EXIT ( -- )
### FOO ( -- n )
Pushes the number 1234 onto the stack.
### BAR ( -- n)
Pushes the number 5678 onto the stack.
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
### IMMED? ( addr -- b )
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
### PAGE
Clear the screen.
### HIDDEN
### ?HIDDEN ( fptr -- b )
Given a `FIND` pointer, return whether or not the word is hidden.
### NIP
### TUCK
### '0'
### '9'
### I
### SMIT ( freq dur --  ) 
Sound emit.  Play the "frequency" with the duration.  The convention
is that the lower the frequency number the higher it actually is in
real life.
### PLAY
### PLOT
### GETP
### DARKP
### TOGP
### LITP
### PN
### BYE
Exit the program.

## WORD words
### 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ( -- n )
Pushes the number the word names onto the stack.
### TS ( n -- n )
Print the top of the stack non-destructively.
### SPACE
### USED
### SIMG
Writeback the "image" (data from `H0` to `HERE @` and the values of
the `LATEST` and `HERE` pointers).  Note that if a previous image was
there already it is overwritten.
### LIMG
Restore from the image (see SIMG).
### >DFA
### :
Begin a definition and enter compile mode.
### ;
End the current definition and exit compile mode.
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
### NUM
### CFA>
### PICK
### SEE
### WORDS
### CASE
### OF
### ENDOF
### ENDCASE
### WR
### STAR
Print a star `*` onto the screen.  Must be the last word defined
because `LATEST` is initialized to point to it.

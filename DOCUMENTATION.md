# Forth Word Documentation
    All stack elements are 16-bit unsigned integers.
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
### DROP
### SWAP
### OVER
### ROT
### -ROT
### 2DROP
### 2DUP
### 2SWAP
### 1+
### 1-
### 4+
### 4-
### >R
### R>
### RDROP
### LIT
### LITSTR
### TELL
### STRLEN
### STRCHR
### !
### @
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
### =
### <>
### >=
### <=
### <
### >
### 0=
### RAND
### ASK
### KEY
### KEYC
### EMIT
### .
### ?
### AKEY
### TO_ASCII
### *
### /MOD
### CR
### AT-XY
### PUTS
### PUTLN
### GETS
### GETC
### UNGETC
### WORD
### IMMED?
### IMMED
### >NFA
### >CFA
### STR=
### FIND
### WB
### CREATE
### DOCOL_H
### PAGE
### HIDDEN
### ?HIDDEN
### NIP
### TUCK
### '0'
### '9'
### I
### SMIT
### PLAY
### PLOT
### GETP
### DARKP
### TOGP
### LITP
### PN
### BYE


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

\ This file contains the original definitions some of the words now
\ defined with `defword'.  You can wrap quotes around them and replace
\ the empty space at `word_buffer' with a string, then the default
\ interpreter will execute the contents of word_buffer.

\ This is useful when you don't want to type on the calculator, but
\ keep in mind that if you get an invalid program warning you may have
\ to remove the empty space starting at `scratch'.  (There's a limit
\ on ASM program size)


: SEE WORD FIND HERE @ LATEST @ BEGIN 2 PICK OVER <> WHILE NIP DUP @
REPEAT DROP SWAP NUM 58 EMIT SPACE DUP ID. SPACE DUP IMMED? IF NUM 73
EMIT SPACE THEN >DFA BEGIN KEY DROP 2DUP > WHILE DUP @ CASE ' LIT OF 2
+ DUP @ CR . ENDOF ' 0BRANCH OF SPACE NUM 48 EMIT NUM 66 EMIT 2 + DUP
@ CR . ENDOF ' BRANCH OF NUM 66 EMIT SPACE 2 + DUP @ CR . ENDOF ' ' OF
NUM 39 EMIT SPACE 2 + DUP @ CFA> ID. SPACE ENDOF ' EXIT OF 2DUP 2 + <>
IF NUM 69 EMIT SPACE THEN ENDOF DUP CFA> ID. SPACE ENDCASE 2 + REPEAT
NUM 59 EMIT CR 2DROP ;

: WORDS LATEST @ BEGIN KEY DROP ?DUP WHILE DUP ?HIDDEN NOT IF DUP
ID. SPACE THEN @ REPEAT CR ;

: QUIT BEGIN RP0 RP! INTERP AGAIN ;

: C, HERE @ C! 1 HERE +! ;

: S" IMMED STATE @ NOT IF ' LITSTR , HERE @ 0 , BEGIN GETC DUP NUM 34 <>
WHILE C, REPEAT DROP 0 C, DUP HERE @ SWAP - 2 - SWAP ! ELSE HERE @
BEGIN GETC DUP NUM 34 <> WHILE OVER C! 1+ REPEAT DROP HERE @ - HERE @
SWAP THEN
;


\ The interpreter.
: INTERP
  GETS \ First time we run, read a string from the user, or omit this
       \ line if you want to run a preloaded program.
  BEGIN
    WORD FIND
    DUP
    IF
      >CFA ?IMMED \ if it's immediate, execute it.
      IF
        
    ELSE
      LIT WBUF RNUM
      DUP 0= IF
        LIT UNDEFMSG PUTLN
      THEN
      
    THEN
    
    
  AGAIN
;

NUM 48 CONST '0'
NUM 65 CONST 'A'

\ Testing double length routines
\ requires rand.fs and ddot.fs
VAR SEED
7 SEED !

: SETSEED             \ SEED THE RNG WITH X
    DUP 0= OR         \ MAP 0 TO -1
    SEED !
;

7 SEED !

: RAND           \ RETURN A 16-BIT RANDOM NUMBER X
    SEED @
    DUP << << << << << << << << << << << << << XOR
    DUP >> >> >> >> >> >> >> >> >> XOR
    DUP << << << << << << << XOR
    DUP SEED !
;

HERE SETSEED

: D+_TEST
  BEGIN
    KEY 5 <> PAGE
  WHILE
    RAND RAND 2DUP D. CR
    RAND RAND 2DUP D. CR
    ." ----------" CR
    D+ D. CR
  REPEAT
;

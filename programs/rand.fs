\ Random number generator.

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

: PP PAGE PLOT ;

: RANDF 0 DO RAND OVER ! 2+ LOOP DROP ;

383 CONST PLOTSZ

: RANDT PLOTSS PLOTSZ RANDF PP ;

: RANDS
  BEGIN
    KEY 5 <>
  WHILE
    RANDT
  REPEAT
;

." RAND loaded."

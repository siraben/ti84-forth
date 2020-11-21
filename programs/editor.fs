\ An editor.  Since we're using a lot of memory we're going to assign
\ it to use AppBackupScreen.

UALT
: (
    BEGIN
      GETC 41 = IF EXIT THEN
    AGAIN
; IMMED

3 CONST UP
4 CONST DN
1 CONST R
2 CONST L
10 CONST DEL
9 CONST CL

: NT S"      EDITOR     " DROP ;

: TYPE ( c-addr u -- ) 0 DO DUP C@ EMIT 1+ LOOP DROP ;

: PL 16 TYPE ;

( Display the text area )
: TXT ( addr -- addr )
  DUP
  1 0 AT-XY
  112 TYPE
;

\ Short names conserve some space.
( addr -- addr )
: NL 16 + ;
: PL 16 - ;
: NP 80 + ;
: PP 80 - ;

VAR CT

\ Set cursor to (0,0)
: ORI 0 0 AT-XY ;

\ Display the title.
: T ORI CT @ INVTXT PL INVTXT ;

\ Draw a frame.
: F PAGE T TXT ;

\ Convert a line number to character number.
: CN ( linum -- charnum )
  16 *
;

\ Mark line.
: ML ( addr linum -- addr linum )
  2DUP
  DUP 1+ 0 AT-XY
  CN + INVTXT PL INVTXT
;

: MF ( addr linum -- addr linum ) OVER F DROP ML ;

( addr count char -- )
: FILL -ROT 0 DO 2DUP C! 1+ LOOP 2DROP ;

: DL ( addr linum -- addr linum )
  2DUP CN + 16 32 FILL
;

: DT S"  --- DELETE --- " DROP ;

: DM ( addr -- addr )
  DT CT !
  0
  BEGIN
    MF
    KEY DUP 10 <>
  WHILE
    CASE
      UP  OF DUP 0=  IF DROP 6 ELSE 1- THEN ENDOF
      DN  OF DUP 6 = IF DROP 0 ELSE 1+ THEN ENDOF
      CL  OF DL ENDOF
    ENDCASE
  REPEAT
  2DROP
;

: RT NT CT ! ;

: EDIT ( addr -- addr )
  TOG-SCRL
  BEGIN
    RT F
    KEY DUP 5 <>
  WHILE
    CASE
      UP OF PL ENDOF
      DN OF NL ENDOF
      R OF NP ENDOF
      L OF PP ENDOF
      DEL OF DM ENDOF
    ENDCASE
  REPEAT
  TOG-SCRL
  DROP
  PAGE
;

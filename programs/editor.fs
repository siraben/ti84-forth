\ An editor.  Since we're using a lot of memory we're going to assign
\ it to use AppBackupScreen.

UALT
: ( IMMED
    BEGIN
      GETC NUM 41 = IF
        EXIT
      THEN
    AGAIN
;

( Display the title )
: TITLE 0 0 AT-XY
        INVTXT
        ."      EDITOR     "
        INVTXT
;


( Display the text area )
: TXTAREA ( addr -- addr )
  DUP
  1 0 AT-XY
  NUM 112 0
  DO
    DUP C@ EMIT 1+
  LOOP
  DROP
;

: NEXT-LINE NUM 16 + ;
: PREV-LINE NUM 16 - ;
: NEXT-PAGE NUM 80 + ;
: PREV-PAGE NUM 80 - ;

: FRAME PAGE TITLE TXTAREA ;
: DELETE-MODE ;

: EDITOR ( addr -- addr )
  TOG-SCRL
  BEGIN
    FRAME
    KEY DUP 5 <>
  WHILE
    CASE
      3 OF PREV-LINE ENDOF
      4 OF NEXT-LINE ENDOF
      1 OF NEXT-PAGE ENDOF
      2 OF PREV-PAGE ENDOF
      10 OF DELETE-MODE ENDOF
    ENDCASE
  REPEAT
  TOG-SCRL
  DROP
;

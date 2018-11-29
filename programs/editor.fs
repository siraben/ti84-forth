\ An editor.
: ( IMMED
    BEGIN
      GETC NUM 41 = IF
        EXIT
      THEN
    AGAIN
;

: DASH NUM 45 EMIT ;

( Display the bar )
: BAR NUM 16 0 DO I 1 AT-XY DASH LOOP ;

( Display the title )
: TITLE 0 0 AT-XY ." EDITOR" ;


( Display the text area )
: TXTAREA ( addr -- addr )
  DUP
  0 2 AT-XY
  NUM 95 0
  DO
    DUP C@ EMIT 1+
  LOOP
  DROP
;

: NEXT-LINE NUM 16 + ;
: PREV-LINE NUM 16 - ;

: FRAME PAGE TITLE BAR TXTAREA ;
: EDITOR ( addr -- addr )
  BEGIN
    FRAME
    KEY DUP 5 <>
  WHILE
    CASE
      3 OF PREV-LINE ENDOF
      4 OF NEXT-LINE ENDOF
    ENDCASE
  REPEAT
  DROP
;

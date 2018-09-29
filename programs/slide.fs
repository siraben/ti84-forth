\ Interactively use the top and bottom arrows of the calculator to
\ edit the top stack item.

: SLIDE
  BEGIN
    KEY DUP 5 <>
  WHILE
    PAGE
    CASE
      3 OF 1+ ENDOF \ up arrow
      4 OF 1- ENDOF \ down arrow
    ENDCASE
    TS CR
  REPEAT
  DROP
;

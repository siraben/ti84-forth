\ Interactively use the top and bottom arrows of the calculator to
\ edit the top stack item.

: SLIDE
  BEGIN
    KEY DUP 5 <>
  WHILE
    PAGE
    DUP 3 = IF DROP \ up arrow
               1+
            ELSE
              DUP 4 =  \ down arrow
              IF DROP
                 1-
              ELSE
                 DROP \ not up or down
              THEN
            THEN
    TS CR
  REPEAT
  DROP
;

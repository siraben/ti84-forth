\ Allows use of a CASE statement to make things easier.

: CASE IMMED
       0
;

: OF IMMED
     ' OVER ,
     ' = ,
     (COMP) IF
     ' DROP ,
;

: ENDOF IMMED
        (COMP) ELSE
;

: ENDCASE IMMED
          ' DROP ,
          BEGIN
            ?DUP
          WHILE
            (COMP) THEN
          REPEAT
;

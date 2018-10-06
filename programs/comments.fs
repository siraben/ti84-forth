\ Define comment style.

: ( IMMED
    BEGIN
      GETC NUM 41 = IF
        EXIT
      THEN
    AGAIN
;    

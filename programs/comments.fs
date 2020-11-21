\ Define comment style.

: ( IMMED
    BEGIN
      GETC 41 = IF
        EXIT
      THEN
    AGAIN
;

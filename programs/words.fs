\ Taken directly from jonesforth

: WORDS
  LATEST @
  BEGIN
    ?DUP
  WHILE
    DUP ?HIDDEN NOT IF
      DUP ID.
      SPACE
      KEY DROP
    THEN
    @
  REPEAT
  CR
;

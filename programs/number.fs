\ Reading a number, Forth style.

\ We use these definitions because it's "faster".
: '0' 10 << 4+ << ; \ 48
: '9' '0' 9 + ; \ 57

( c a b -- < a <= c OR c <= b > )
: WITHIN OVER - >R - R> <= ;

: NUM? '0' '9' WITHIN ;

: NUM IMMED
  0
  BEGIN
    GETC DUP NUM? NOT
    IF
      DROP \ we're done reading
      STATE @ IF
        \ interpreting
      ELSE
        ' LIT ,
        , \ compiling
      THEN
      EXIT
    ELSE
      '0' - SWAP 10 * +
    THEN
  AGAIN
;

\ NUM 1234 .
\ => 1234

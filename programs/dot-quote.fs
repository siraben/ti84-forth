\ Run-time behavior of ."
: ." BEGIN
    GETC DUP 10 3 * 4 + = IF \ 10 3 * 4 + is equivalent to 34, but I
                            \ haven't implemented number parsing yet.
      DROP EXIT
    THEN
    EMIT
  AGAIN
;
\ ." HELLO, WORLD!" => HELLO, WORLD!

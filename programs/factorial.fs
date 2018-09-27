\ Factorial, recursive definition.

\ Look ma, recursion is easy!  Just use the RECURSE word!
: FACT
  DUP 0= IF
    DROP 1         \ base case
  ELSE
    DUP 1- RECURSE \ compute factorial of n - 1
    *              \ and multiply with n
  THEN
;
\ 5 FACT . => 120

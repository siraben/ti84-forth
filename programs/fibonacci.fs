\ Recursive Fibonacci.  Not very efficient as N goes up.
: FIB
  DUP 2 <
  IF
    \ BASE CASE, DO NOTHING
  ELSE
    1- DUP 1- RECURSE
    SWAP RECURSE
    +
  THEN
;

\ Display double length numbers.
: D.
  BASE @ D/MOD
  2DUP OR
  IF
         RECURSE
  ELSE
         2DROP
  THEN

  DUP 10 <
  IF
         48
  ELSE
         10 - 65
  THEN + EMIT
;

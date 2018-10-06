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
         NUM 48
  ELSE
         10 - NUM 65
  THEN + EMIT
;

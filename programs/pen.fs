\ Interactive Pen Program

\ See case.fs for definition of CASE

\ Key codes
\  3
\ 2 1
\  4

\ ENTER
\ 5

VARIABLE PX
VARIABLE PY

: REDRAW
  PAGE
  PX ?
  PY ?
;

: PEN_UP
  1 PY +!
;

: PEN_DOWN
  65535 PY +!
;

: PEN_RIGHT
  1 PX +!
;

: PEN_LEFT
  65535 PX +!
;

: PEN
  BEGIN
    KEY DUP 5 <>
  WHILE
    REDRAW
    CASE
      3 OF PEN_UP ENDOF
      4 OF PEN_DOWN ENDOF
      1 OF PEN_RIGHT ENDOF
      2 OF PEN_LEFT ENDOF
    ENDCASE
  REPEAT
  DROP
;

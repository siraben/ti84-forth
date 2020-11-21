\ Display key codes interactively and play the notes

: SONG
  BEGIN
    KEY DUP 5 <>
  WHILE
    DUP CR . 40000 SMIT
  REPEAT
  DROP
;

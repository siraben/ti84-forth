\ Interactive Keypresses

\ Let's make a program to interactively display the keypresses of the
\ user.  We have an `AKEY` word that returns the calculator keypress as
\ an ASCII number on the stack using a conversion table (see `key_table`
\ for the exact conversion).  The following is a valid program that you
\ can enter into the calculator.

: SHOW_KEYS \ define a new word called SHOW_KEYS
        AKEY \ read an ASCII character from the user
        BEGIN
                DUP 0 <> \ test if the last key entered was not ENTER
                         \ , as AKEY returns 0 in such a case
        WHILE
                DUP . SPACE SPACE EMIT CR \ duplicate the character,
                                          \ type two spaces, print
                                          \ it and type a newline
                AKEY \ read another character
        REPEAT \ repeat the body while the condition is true
        DROP \ the last key entered was ENTER, so drop it and return
;

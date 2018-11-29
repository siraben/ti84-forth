: TRUE 1 ;
: FALSE 0 ;

: PRIME? \ ( N -- F )
        DUP 2 < IF      DROP FALSE
    ELSE DUP 2 = IF      DROP TRUE
    ELSE DUP 1 AND 0= IF DROP FALSE
    ELSE 3
        BEGIN 2DUP DUP * >=
        WHILE 2DUP MOD 0=
              IF       2DROP FALSE EXIT
              THEN 2 +
        REPEAT         2DROP TRUE
    THEN THEN THEN ;

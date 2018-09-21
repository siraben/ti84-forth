.NOLIST

;; Doesn't care what's in HL.
#define NEXT ld a, (de)
#defcont   \ ld l, a
#defcont   \ inc de
#defcont   \ ld a, (de)
#defcont   \ ld h, a
#defcont   \ inc de
#defcont   \ jp (hl)

;; Verified correct.
#define PUSH_BC_RS dec ix
#defcont         \ ld (ix + 0), b
#defcont         \ dec ix
#defcont         \ ld (ix + 0), c

#define POP_BC_RS  ld c, (ix + 0)
#defcont         \ inc ix
#defcont         \ ld b, (ix + 0)
#defcont         \ inc ix

;; Verified correct.
#define PUSH_HL_RS dec ix
#defcont         \ ld (ix + 0), h
#defcont         \ dec ix
#defcont         \ ld (ix + 0), l

#define POP_HL_RS  ld l, (ix + 0)
#defcont         \ inc ix
#defcont         \ ld h, (ix + 0)
#defcont         \ inc ix

;; Verified correct.
#define PUSH_DE_RS dec ix
#defcont         \ ld (ix + 0), d
#defcont         \ dec ix
#defcont         \ ld (ix + 0), e

#define POP_DE_RS  ld e, (ix + 0)
#defcont         \ inc ix
#defcont         \ ld d, (ix + 0)
#defcont         \ inc ix

#define BC_TO_HL ld h, b
#defcont       \ ld l, c

#define HL_TO_BC ld b, h
#defcont       \ ld c, l

#define HL_TO_DE ld d, h
#defcont       \ ld e, l

#define BC_TO_DE ld d, b
#defcont       \ ld e, c

;; BC = TOS   IX = RSP
;; DE = IP    IY = UP
;; HL = W     SP = PSP


#include "inc/ti83plus.inc"


start:
        .org 9D93h
        .db $BB,$6D
        b_call _RunIndicOff
        b_call _ClrLCDFull
        b_call _HomeUp
        
        pop bc ;; Save the place where this program needs to go.
        ld hl, prog_exit
        ld (hl), c
        inc hl
        ld (hl), b
        push bc

        call setup_data_segment
        ld bc, 1234
        ld de, prog
        NEXT
docol:
        PUSH_DE_RS
        pop de
        NEXT
exit:
        POP_DE_RS
        NEXT
done:
        b_call _GetKey
        b_call _ClrScrnFull

        ;; Even if we blew up the stack during execution, we can try to restore it and exit cleanly.
        ld sp, (save_sp)
        ld hl, (prog_exit)
        jp (hl)



#macro defcode(name,len,flags,label)
  ;; This is the first word to be defined, so set the link.
  #ifndef prev
    #define prev 0
  #else
    #define prev eval(-_)
  #endif
_:
  #define label_str concat("\"",label,"\"")
  #define name_label concat("\"name_",label,"\"")  
  #define name_str concat("\"",name,"\"")
  clr()  
  wr(name_label,":")
  wr(".dw ", eval(prev))
  wr(".db ", eval(len+flags))
  wr(".db ", "\"",name_str,"\",0")
  wr(label_str, ":")
  run()
  
#endmacro

#define CALL_DOCOL call docol

#define F_IMMED $80
#define F_HIDDEN $20
#define F_LEN $20
#define F_LENMASK $1F

#macro defword(name,len,flags,label)

  ;; This is the first word to be defined, so set the link.
  #ifndef prev
    #define prev 0
  #else
    #define prev eval(-_)
  #endif
_:
  #define label_str concat("\"",label,"\"")
  #define name_label concat("\"name_",label,"\"")  
  #define name_str concat("\"",name,"\"")
  clr()  
  wr(name_label,":")
  wr(".dw ", eval(prev))
  wr(".db ", eval(len+flags))
  wr(".db ", "\"",name_str,"\",0")
  wr(label_str, ":")
  run()
  CALL_DOCOL
#endmacro

        defcode("FOO",3,0,foo)
        push bc
        ld bc, 1234
        NEXT

        defcode("BAR",3,0,bar)
        push bc
        ld bc, 5678
        NEXT
        
        defcode("DUP",3,0,dup)
        push bc
        NEXT

        defcode("ADD",3,0,add)
        pop hl
        add hl, bc
        HL_TO_BC
        NEXT

        defcode("SUB",3,0,sub)
        xor a
        pop hl
        sbc hl, bc
        HL_TO_BC
        NEXT

        defcode("DROP",4,0,drop)
        pop bc
        NEXT

        defcode("SWAP",4,0,swap)
        pop hl
        push bc
        HL_TO_BC
        NEXT

        defcode("OVER",4,0,over)
        pop hl
        push hl
        push bc
        HL_TO_BC
        NEXT

        defcode("ROT",3,0,rot)
        PUSH_DE_RS
        pop hl
        pop de
        push hl
        push bc
        push de
        POP_DE_RS
        NEXT

        defcode("-ROT",4,0,nrot)
        PUSH_DE_RS
        pop hl
        pop de
        push bc
        push de
        push hl
        POP_DE_RS
        NEXT

        defcode("2DROP",5,0,two_drop)
        pop bc
        pop bc
        NEXT

        defcode("2DUP",5,0,two_dup)
        pop hl
        push hl
        push bc
        push hl
        push bc
        NEXT

        ;; Register contents (return stack contents)
        ;; a b c d => c d a b
        defcode("2SWAP",5,0,two_swap)
        PUSH_DE_RS
        pop de
        ;; Now DE = C, BC = D, (old-DE)
        pop hl
        PUSH_HL_RS
        pop hl
        ;; DE = C, BC = D, HL = A (old-DE B)
        push de
        push bc
        push hl
        POP_BC_RS
        ;; c d a, BC = b
        POP_DE_RS
        NEXT

        defcode("1+", 2, 0, one_plus)
        inc bc
        NEXT


        defcode("1-", 2, 0, one_minus)
        dec bc
        NEXT

        defcode("4+", 2, 0, four_plus)
        ld hl, 4
        add hl, bc
        HL_TO_BC
        NEXT
        
        defcode("4-", 2, 0, four_minus)
        ld hl, -4
        add hl, bc
        HL_TO_BC
        NEXT

        defcode(">R", 2, 0, to_r)
        PUSH_BC_RS
        pop bc
        NEXT

        defcode("R>", 2, 0, from_r)
        POP_BC_RS
        NEXT
        
        defcode("RDROP",5,0,r_drop)
        POP_HL_RS
        NEXT

        defcode("LIT",3,0,lit)
        ld a, (de)
        ld l, a
        inc de
        ld a, (de)
        ld h, a
        inc de
        push bc
        HL_TO_BC
        NEXT

        defcode("!", 1,0,store)
        pop hl
        ld a, l
        ld (bc), a
        inc bc
        ld a, h
        ld (bc), a
        NEXT

        defcode("@", 1,0,fetch)
        ld a, (bc)
        ld l, a
        inc bc
        ld a, (bc)
        ld h, a
        HL_TO_BC
        NEXT

        defcode("+!",2,0,add_store)
        pop hl
        push de
        ld a, (bc)
        ld e, a
        inc bc
        ld a, (bc)
        ld d, a
        dec bc
        add hl, de
        ld a, l
        ld (bc), a
        inc bc
        ld a, h
        ld (bc), a
        inc bc

        pop de
        NEXT

        defcode("-!",2,0,sub_store)
        pop hl
        push de
        ld a, (bc)
        ld e, a
        inc bc
        ld a, (bc)
        ld d, a
        dec bc
        xor a
        ex de, hl
        sbc hl, de

        ld a, l
        ld (bc), a
        inc bc
        ld a, h
        ld (bc), a
        pop de
        NEXT

        defcode("C!", 2,0, store_byte)
        pop hl
        ld a, l
        ld (bc), a
        NEXT
        
        defcode("C@", 2,0, fetch_byte)
        ld a, (bc)
        ld c, a
        ld b, 0
        NEXT

        ;; Set the instruction pointer to the top of the stack (i.e. 
        defcode("EXECUTE",7,0,execute)
        BC_TO_DE
        NEXT

;; defvar("foo",3,0,foo,1000)
;; We want that to expand to:
;; name_foo:
;; .dw <pointer to previous>
;; .db 3
;; .db "foo",0
;; foo:
;; push bc
;; ld bc, var_foo
;; NEXT
;; var_foo:
;; .dw 1000

;; Make some variables!

;; Well since there's only like 6 variables defined in Jonesforth it's
;; not forth the effort trying to debug a macro (which took up a lot
;; of time in this program!)

#define PSP_PUSH(x) push bc \ ld bc, x

#macro cell_alloc(name,initial)
clr()
#define name_str concat("\"",name,"\"")
wr(name_str, ":")
wr(".dw ", initial)
run()
#endmacro

        ;; Actually a variable, but oh well.
        cell_alloc(var_base,10)
        defcode("BASE",4,0,base)
        PSP_PUSH(var_base)
        NEXT

        ;; Are we compiling or are we interpreting?
        cell_alloc(var_state,0)
        defcode("STATE",5,0,state)
        PSP_PUSH(var_state)
        NEXT

        ;; Address of the most recently defined word.
        ;; We have to do a very hacky thing.
var_latest:
        .dw name_star
        defcode("LATEST",6,0,latest)
        PSP_PUSH(var_latest)
        NEXT

        ;; Top (i.e. bottom) of the stack.
        cell_alloc(var_sz,0)
        defcode("S0",2,0,sz)
        PSP_PUSH(var_sz)
        NEXT


        defcode("[",1,F_IMMED,lbrac)
        xor a
        ld (var_state), a
        NEXT

        defcode("]",1,F_IMMED,rbrac)
        ld a, 1
        ld (var_state), a
        NEXT
        

        cell_alloc(var_stack_empty,1)
        defcode("?SE", 3, 0, stack_emptyq)
        PSP_PUSH(var_stack_empty)
        NEXT

        cell_alloc(var_here,0)
        defcode("HERE",4,0,here)
        push bc
        ld hl, (var_here)
        ld b, h
        ld c, l
        NEXT


        ;; Here are some constants.
        defcode("DOCOL", 5, 0, __docol)
        PSP_PUSH(docol)
        NEXT

        defcode("BUF", 3, 0, __buffer)
        PSP_PUSH(buffer)
        NEXT

        defcode("R0", 2, 0, rz)
        PSP_PUSH(return_stack_top)
        NEXT

        defcode("F_IMMED",7,0,__F_IMMED)
        PSP_PUSH(F_IMMED)
        NEXT

        defcode("F_HIDDEN",8,0,__F_HIDDEN)
        PSP_PUSH(F_HIDDEN)
        NEXT

        defcode("F_LENMASK",9,0,__F_LENMASK)
        PSP_PUSH(F_LENMASK)
        NEXT


        defcode("'",1,0,tick)
        ld a, (de)
        ld l, a   
        inc de    
        ld a, (de)
        ld h, a   
        inc de
        push bc
        HL_TO_BC
        NEXT

        defcode(",",1,0,comma)
        call _comma
        pop bc
        NEXT
_comma:
        ;; Remember that var_here is a pointer, so you need to do
        ;; double indirection!
        push de
        
        ld hl, (var_here)

        ld (hl), c
        inc hl
        ld (hl), b
        inc hl
        
        ld de, var_here
        ex de, hl
        ld (hl), e
        inc hl
        ld (hl), d
        inc hl

        pop de

                
        ret
        
        ;; Actually, we do have a stack pointer, but it's not probably
        ;; what is normally expected of Forths.
        defcode("SP@", 3, 0, sp_fetch)
        ld (var_sp), sp
        ld hl, (var_sp)
        push hl
        NEXT

var_sp:
        .dw 0
        
        defcode("SP!", 3, 0, sp_store)
        BC_TO_HL
        ld sp, hl
        NEXt
        
        defcode("BRANCH", 6, 0, branch)
        ld a, (de)
        ld l, a
        inc de
        ld a, (de)
        ld h, a
        dec de

        add hl, de
        HL_TO_DE
        NEXT

;; cpHLDE [Maths]
;;  Compares HL to DE.
;; Output:
;;  Same as z80 CP instruction.
cpHLDE:
        or a
        sbc hl, de
        add hl,de
        ret
;; cpHLBC [Maths]
;;  Compares HL to BC.
;; Output:
;;  Same as z80 CP instruction.
cpHLBC:
        or a
        sbc hl, bc
        add hl,bc
        ret
;; cpBCDE [Maths]
;;  Compares DE to BC.
;; Output:
;;  Same as z80 CP instruction.
cpBCDE:
        push hl
        ld h, b
        ld l, c
        or a
        sbc hl, de
        pop hl
        ret



;; We need this because we want to pop the top of the stack it was indeed 0.
zbranch_:
        pop bc
        jp branch
        
        defcode("ZBRANCH", 7, 0, zbranch)
        ld hl, 0
        call cpHLBC
        jp z, zbranch_

        ;; The top of the stack wasn't zero.  Skip the offset and resume execution.
        inc de
        inc de

        pop bc ;; New top of stack
        
        NEXT

        defcode("?DUP",4,0,qdup)
        ld hl, 0
        call cpHLBC
        jp nz, dup
        NEXT

        defcode("=", 1,0,eql)
        pop hl
        call cpHLBC
        jp z, tru
        jp fal

        defcode("<>", 2,0, neql)
        pop hl
        call cpHLBC
        jp z, fal
        jp tru

        defcode(">=", 2,0,greater)
        pop hl
        call cpHLBC
        jp nc, tru
        jp fal

        defcode("<",1,0,less_than)
        pop hl
        call cpHLBC
        jp c, tru
        jp fal

        defcode("0=",2,0,zeql)
        ld hl,0
        call cpHLBC
        jp c, tru
        jp fal

        defcode("RAND",4,0,rand)
        push bc
        ld a, r
        ld c, a
        ld a, r
        ld b, a
        NEXT

;; Place a truth value on the top of the stack.
tru:
        ld bc, 1
        NEXT
        
;; Place a false value on the top of the stack.
fal:
        ld bc,0
        NEXT

;; Refer to https://github.com/KnightOS/kernel/blob/830f4ac87c50fa42faf634ab3ee476f0ab85b741/src/00/strings.asm
;; for string routines.

printhl_safe:
        push hl
        push af
        push de
        b_call _DispHL
        pop de
        pop af
        pop hl
        ret
        
ask_msg: .db "Press a key...",0
        defcode("ASK", 3, 0, ask)
        ld hl, ask_msg
        call str_println
        NEXT
        
str_println:
        b_call _PutS
        b_call _Newline
        ret
        
str_print:
        b_call _PutS
        ret

        defcode("UPRESS", 6, 0, you_pressed)
        ld hl, you_pressed_msg
        call str_println
        NEXT

        ;; key emit should output the same character that was input,
        ;; by the way
        defcode("KEY", 3, 0, key)
        push bc
        push de
        b_call _GetKey
        pop de
        ld c, a
        ld b, 0
        NEXT

        defcode("EMIT",4,0,emit)
        ld a, c
        b_call _PutC
        pop bc
        NEXT


        defcode("U.", 2, 0, print_tos)
        BC_TO_HL
        call printhl_safe
        pop bc
        NEXT

        ;; Convert a key code into an ASCII character by way of a
        ;; lookup table.
        defcode("KEY_ASCII",9,0,key_ascii)
        ;; First portion is copied from key.
        push bc
        push de
        b_call _GetKey
        ;; a contains the byte received.
        ld h, 0
        ld l , a
        ld de, key_table
        ;; Add the offset
        add hl, de
        ld a, (hl)
        ld c, a
        ld b, 0
        pop de
        NEXT

        defcode("TO_ASCII",8,0,to_ascii)
        ;; First portion is copied from key.
        push bc
        push de
        ld h, 0
        ld l , c
        ld de, key_table
        ;; Add the offset
        add hl, de
        ld a, (hl)
        ld c, a
        ld b, 0
        pop de
        NEXT


        


key_table:
.db "     ",$00,"  " ;; 0-7
.db "        " ;; 8-15
.db "        " ;; 16-23
.db "        " ;; 24-31
.db "        " ;; 32-39
.db "        " ;; 40-47
.db "        " ;; 48-55
.db "        " ;; 56-63
.db "        " ;; 64-71
.db "        " ;; 72-79
.db "        " ;; 80-87
.db "        " ;; 88-95
.db "        " ;; 96-103
.db "        " ;; 104-111
.db "        " ;; 112-119
.db "        " ;; 120-127
.db "+-*/^()",193 ;; 128-135
.db "]  , .  " ;; 136-143
.db "        " ;; 144-151
.db "  ABCDEF";; 152-159
.db "GHIJKLMN";; 160-167
.db "OPQRSTUV" ;; 168-175
.db "WXYZ    " ;; 176-183
.db "        " ;; 184-191
.db "       :" ;; 192-199
.db "  ?\"    " ;; 200-207
.db "        " ;; 208-215
.db "        " ;; 216-223
.db "        " ;; 224-231
.db "    {}  " ;; 232-239
.db "        " ;; 240-247
.db "        " ;; 248-255


;; Inputs:
;;  DE: Multiplier
;;  BC: Multiplicand
;; Outputs:
;;  DEHL: Product of DE and BC.
#macro mul16By16Iter
        add hl, hl
        rl e
        rl d
        jr nc, $ + 6
        add hl, bc
        jr nc, $ + 3
        inc de
#endmacro

        defcode("*",1,0,mult)
        PUSH_DE_RS
        pop de ;; get the second element from the stack
        ld hl, 0
        sla e
        rl d
        jr nc, $ + 4
        BC_TO_HL
    
        mul16By16Iter
        mul16By16Iter
        mul16By16Iter
        mul16By16Iter
        mul16By16Iter
        mul16By16Iter
        mul16By16Iter
        mul16By16Iter
        mul16By16Iter
        mul16By16Iter
        mul16By16Iter
        mul16By16Iter
        mul16By16Iter
        mul16By16Iter
        mul16By16Iter
        
        HL_TO_BC
        POP_DE_RS
        NEXT
        
        defword("TESTA", 5, 9, test_arith)
        .dw dup, four_plus, print_tos
        .dw dup, four_minus, print_tos
        .dw dup, one_plus, print_tos
        .dw dup, one_minus, print_tos
        .dw print_tos, exit

        defword("DOUBLE",6,0,times_two)
        .dw dup, add, exit



        defword("STARS",5,0,stars)
        .dw star, one_minus, zbranch, 6, branch, -10, exit

        defword("SPACE",5,0,space)
        .dw lit, 32, emit, exit

        defcode("CR",2,0,cr)
        b_call _Newline
        NEXT

        defcode("AT-XY",5,0,atxy)
        ld a, c
        ld (curRow), a
        pop bc
        ld a, c
        ld (curCol), a
        NEXT

        ;; Display a null-terminated string starting at the address
        ;; given to by the TOS.
        
        defcode("PUTS",4,0,putstr)
        BC_TO_HL
        b_call _PutS
        pop bc
        NEXT

        defcode("PUTLN",5,0,putstrln)
        BC_TO_HL
        b_call _PutS
        b_call _NewLine
        pop bc
        NEXT


;; Skipping spaces, get the next word from the user.
;; We store the string we got into str_buf.
;; ( -- )
#define BUFSIZE  31
buffer    .EQU TextShadow
        defcode("WORD",4,0,word)
        ;; Save IP and TOS.
        push de
        push bc
        
        ;; We can trash the value of BC now.
skip_space:
        b_call _GetKey        ;; Destroys BC DE HL, loads keycode to A.
        ld h, 0
        ld l, a
        ld c, a
        ld de, key_table
        add hl, de            ;; Add the offset.
        ld a, (hl)            ;; Load the actual ASCII character code into the accumulator.
        cp ' '
        jp z, skip_space
        
        ;; First non-space character hit.
        ;; Push the address of where to write to.
        ld hl, buffer
        
        push hl

        ld a, c

word_read_loop:
        ld h, 0
        ld l, a
        ld de, key_table
        add hl, de            ;; Add the offset.
        ld a, (hl)            ;; Load the actual ASCII character code into the accumulator.
        b_call _PutC          ;; Give user feedback.
        pop hl                ;; Pop the address to write to.
        ld (hl), a
        
        inc hl                ;; Increment address.

        push hl               ;; Save it.
after_word_del:        
        b_call _GetKey        ;; Read another character.
        cp kEnter
        jp z, word_done
        cp kSpace
        jp z, word_done
        cp kDel
        jp z, word_del
        jp word_read_loop
word_del:
        pop hl
        dec hl       ;; Decrement the storage pointer.
        push hl
        ld c, a
        ld a, 207
        b_call _PutC ;; Indicate that backspace was performed.
        ld a, c
        jp after_word_del  ;; We want to return to the main loop, but the user may enter more [DEL] keys.
        
word_done:
        ;; We still have the address to be written to on the stack.
        pop hl
        ld (hl), 0 ;; NUL-terminate the string
        pop bc     ;; restore IP and TOS
        pop de
        NEXT

        ;; That was the longest defcode yet!

;; strcmp [Strings]
;;  Determines if two strings are equal, and checks alphabetical sort order.
;; Inputs:
;;  HL: String pointer
;;  DE: String pointer
;; Outputs:
;;  Z: Set if equal, reset if not equal
;;  C: Set if string HL is alphabetically earlier than string DE
_strcmp:
        push hl
        push de
_strcmp_loop:
        ld a, (de)
        or a
        jr z, _strcmp_end
        cp (hl)
        jr nz, _strcmp_exit
        inc hl
        inc de
        jr _strcmp_loop
_strcmp_end:
        ld a, (hl)
        or a
_strcmp_exit:
        ccf
        pop de
        pop hl
        POP_DE_RS
        jp z, tru
        jp nz, fal


        ;; Convert a pointer returned by FIND to the start of the name
        ;; field address (something I made up).
        defcode(">NFA",5,0,to_nfa)
        inc bc
        inc bc
        inc bc
        NEXT
        
        ;; Convert a pointer returned by FIND to the start of the code
        ;; field address.
        defcode(">CFA",4,0,to_cfa)
        inc bc
        inc bc ;; Skip the link pointer.
        ld a, (bc) ;; Get the length of the word.
        ld h, 0
        ld l, a
        inc bc
        add hl, bc
        HL_TO_BC
        inc bc
        NEXT

        ;; Are two strings equal?
        ;; ( s1 s2 -- b )
        defcode("STREQ",4,0,streq)
        PUSH_DE_RS
        pop de
        BC_TO_HL
        pop bc
        jp _strcmp ;; we're offhanding this to the strcmp routine


        ;; ( pointer to string -- pointer to word )
        defcode("FIND",4,0,find)
        push de
        BC_TO_HL
        ld de, (var_latest)
        inc de
        inc de
        inc de

find_loop:
        call strcmp
        jp z, find_succeed
        jp nz, find_retry

find_succeed:
        dec de
        dec de
        dec de
        pop hl
        ex de,hl
        HL_TO_BC
        NEXT
find_retry:
        dec de
        dec de
        dec de
        push hl
        ld a, (de)
        ld l, a
        inc de
        ld a, (de)
        ld h, a
        dec de
        ld a, l
        cp 0
        jp z, find_maybe_fail

find_retry_cont:        
        inc hl
        inc hl
        inc hl
        ex de,hl
        pop hl
        jp find_loop
find_maybe_fail:
        ld a, h
        cp 0
        jp z, find_fail
        jp nz, find_retry_cont
find_fail:
        pop hl
        pop de
        ld bc, 0
        NEXT

;; strcmp [Strings]
;;  Determines if two strings are equal, and checks alphabetical sort order.
;; Inputs:
;;  HL: String pointer
;;  DE: String pointer
;; Outputs:
;;  Z: Set if equal, reset if not equal
;;  C: Set if string HL is alphabetically earlier than string DE
strcmp:
        push hl
        push de
strcmp_loop:
        ld a, (de)
        or a
        jr z, strcmp_end
        cp (hl)
        jr nz, strcmp_exit
        inc hl
        inc de
        jr strcmp_loop
strcmp_end:
        ld a, (hl)
        or a
strcmp_exit:
        ccf
        pop de
        pop hl
        ret


        defword(">DFA",4,0,to_dfa)
        .dw to_cfa, four_plus, exit
        

	;; We're doing this so that we can initialize LATEST to point to it.
	defword("STAR", 4, 0, star)
	.dw lit, 42, emit, exit

    
you_pressed_msg: .db "You pressed:", 0


quadruple_prog:
        .dw times_two, times_two, print_tos, done
key_count_prog:
        .dw ask, key, you_pressed, print_tos, done

dummy_byte: .db 0



write_prog:
        .dw lit, dummy_byte, print_tos
        .dw lit, 10, lit, dummy_byte, store_byte, lit, dummy_byte, fetch_byte, print_tos
        .dw done
stars_prog:
        .dw lit, 10, stars
        .dw done


;; Type out a message (doesn't store it in data, though, just echoes
;; to the screen).
        
type_prog:
        .dw key_ascii, dup, emit
        .dw lit, 0, eql, zbranch, -14, done

;; Prints out the keycode entered, the character with that code.
;; Then converts the keycode into an ASCII character with the to_ascii word
;; Then prints the ASCII code and the character.

;; In summary:

;; <KEYCODE> <EMITTED KEYCODE> <TO_ASCII CONVERTED CHARACTER> <EMITTED CONVERTED NUMBER>

key_prog:
        .dw key, dup, dup, dup, print_tos, space, emit, space, to_ascii, dup
        .dw print_tos, space, emit, cr
        .dw lit, kEnter, eql
        .dw zbranch, -36
        .dw done

;; Need this because the string wraps in an ugly way.
what_name_str: .db "What is your",0
what_name_str2: .dw "name?",0
luck_num_str: .db "Your lucky",0
luck_num_str2: .db "numbers are",0
hi_str: .db "Hello, ",0
fun_prog:
        .dw lit, what_name_str, putstrln
        .dw lit, what_name_str2, putstrln
        .dw word, cr
        .dw lit, hi_str, putstr, space
        .dw __buffer, putstrln
        .dw lit, luck_num_str, putstrln
        .dw lit, luck_num_str2, putstrln
        .dw rand, print_tos, space, rand, print_tos
        .dw done

msg1: .db "SECRET",0
guess: .db "Guess my secret:",0
secret_prog:
        .dw lit, guess, putstrln, lit, msg1
        .dw word, lit, buffer, streq, print_tos
        .dw done
cfa_prog:
        .dw latest, fetch, dup, lit, 3, add, putstrln
        .dw dup, print_tos, cr
        .dw to_cfa, print_tos,cr
        .dw done

;; Demonstrate that we can perform searches of the words.
search_msg: .db "Search: ",0
prog:
        .dw lit, search_msg, putstrln
        .dw word, lit, buffer, cr, find, dup, print_tos, dup
        .dw zbranch, 14, to_nfa, space, putstrln, cr, branch, 2, drop
        .dw done
        
;; We're going to see if the dictionary was constructed correctly.
words_prog:
        .dw latest, fetch, dup, lit, 3, add, putstr, space
        .dw fetch
        .dw zbranch, 22
        .dw dup, lit, 3, add, putstr, space     ;; loop
        .dw key, drop
        .dw branch, -24
        .dw done

setup_data_segment:
        ld de, AppBackUpScreen
        ld hl, var_here
        ld (save_sp), sp
        ld (hl), e
        inc hl
        ld (hl), d
        ld ix, return_stack_top

        ret
        
return_stack_top  .EQU    AppBackUpScreen+764
prog_exit: .dw 0
save_sp:
        .dw 0

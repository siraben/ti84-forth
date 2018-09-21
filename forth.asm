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
        ld sp, (save_sp)
        ret



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


        defcode("2DROP",5,0,two_drop)
        pop bc
        pop bc
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
        cell_alloc(var_latest,0)
        defcode("LATEST",6,0,latest)
        PSP_PUSH(var_latest)
        NEXT

        ;; Top (i.e. bottom) of the stack.
        cell_alloc(var_sz,0)
        defcode("S0",2,0,sz)
        PSP_PUSH(save_sp)
        NEXT

        defcode("[",1,F_IMMED,lbrac)
        xor a
        ld (var_state), a
        NEXT

        defcode("]",1,F_IMMED,rbrac)
        ld a, 1
        ld (var_state), a
        NEXT
        
save_sp:
        .dw 0

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

        defcode("R0", 5, 0, rz)
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



        defcode("ZBRANCH", 7, 0, zbranch)
        ld hl, 0
        call cpHLBC
        jp z, branch


        ld a, (de)
        ld l, a
        inc de
        ld a, (de)
        ld h, a
        inc de
        
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

        defcode(">=", 1,0,greater)
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
.db " -*     " ;; 128-135
.db "   ,    " ;; 136-143
.db "        " ;; 144-151
.db "  ABCDEF";; 152-159
.db "GHIJKLMN";; 160-167
.db "OPQRSTUV" ;; 168-175
.db "WXYZ    " ;; 176-183
.db "        " ;; 184-191
.db "        " ;; 192-199
.db "        " ;; 200-207
.db "        " ;; 208-215
.db "        " ;; 216-223
.db "        " ;; 224-231
.db "        " ;; 232-239
.db "        " ;; 240-247
.db "        " ;; 248-255

;; User input
#define BUFSIZE  48
buffer   .EQU TextShadow
buf_ptr  .EQU buffer + BUFSIZE + 1



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

        defword("STAR", 4, 0, star)
        .dw lit, 42, emit, exit

        defword("STARS",5,0,stars)
        .dw star, one_minus, zbranch, 6, branch, -10, exit

        defword("SPACE",5,0,space)
        .dw lit, 32, emit, exit

        defcode("CR",2,0,cr)
        b_call _Newline
        NEXT

        defcode("AT-XY",4,0,atxy)
        ld a, c
        ld (curRow), a
        pop bc
        ld a, c
        ld (curCol), a
        NEXT
        

you_pressed_msg: .db "You pressed:", 0


quadruple_prog:
        .dw times_two, times_two, print_tos, done
key_count_prog:
        .dw ask, key, you_pressed, print_tos, done
        defword("COUNT", 5, 0, count)
        .dw dup, print_tos, one_minus, zbranch, 6, branch, -12, exit

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

prog:
        .dw key, dup, dup, print_tos, space, emit, cr
        .dw lit, kEnter, eql
        .dw zbranch, -22
        .dw done
        
return_stack_top  .EQU    AppBackUpScreen+760

setup_data_segment:
        ld de, AppBackUpScreen
        ld hl, var_here
        ld (save_sp), sp
        ld (hl), e
        inc hl
        ld (hl), d
        ld ix, return_stack_top

        ret

prog_exit: .dw 0

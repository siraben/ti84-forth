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

        ;; For some weird reason, either the spasm assembler or TI-84
        ;; can't store the string "[", so we have to fix it by changing the value of lbrac's string.

        ld hl, name_lbrac
        inc hl
        inc hl
        inc hl
        ld (hl), 193
        call setup_data_segment
        ld bc, 9999
        ld de, prog
        NEXT
docol:
        PUSH_DE_RS
        pop de
        NEXT
        

done:
        ;; We reach here at the end of the program.
        ld hl, 9999
        call cpHLBC
        jp nz, print_stack_error

done_cont:

        b_call _GetKey
        b_call _ClrScrnFull

        ;; Even if we blew up the stack during execution, we can try to restore it and exit cleanly.
        ld sp, (save_sp)
        ld hl, (prog_exit)
        jp (hl)
print_stack_error:
        ld hl, possible_error_msg
        b_call _PutS
        jp done_cont
possible_error_msg: .db "Warning: Stack not empty or underflowed.",0


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

#define F_IMMED 128
#define F_HIDDEN 64
#define F_LEN $20
#define F_LENMASK 31

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

        defcode("EXIT",4,0,exit)
        POP_DE_RS
        NEXT

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

        defcode("+",1,0,add)
        pop hl
        add hl, bc
        HL_TO_BC
        NEXT

        defcode("-",1,0,sub)
        xor a
        pop hl
        sbc hl, bc
        HL_TO_BC
        NEXT

        ;; To be implemented.
        defcode("AND",3,0,and)
        pop bc
        
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
        ex de, hl
        HL_TO_BC
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

        defcode("2DUP",4,0,two_dup)
        pop hl
        push hl
        push bc
        push hl
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
        inc bc
        inc bc
        inc bc
        inc bc
        NEXT
        
        defcode("4-", 2, 0, four_minus)
        dec bc
        dec bc
        dec bc
        dec bc
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
        pop bc
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

        defcode("C@C!",4,0,byte_copy)
        pop hl
        ld a, (bc)
        ld (hl), a
        inc hl
        inc bc
        push hl
        NEXT

        defcode("CMOVE",5,0,cmove)
        PUSH_DE_RS
        pop hl
        pop de
        ldir
        push de
        HL_TO_BC
        POP_DE_RS
        NEXT

        ;; BC points to a codeword, execute it!
        defcode("EXECUTE",7,0,execute)
        BC_TO_HL
        pop bc
        jp (hl)
        NEXT


;; Make some variables!

;; Well since there's only like 6 variables defined in Jonesforth it's
;; not forth the effort trying to debug a macro (which took up a lot
;; of time for defword and defcode!)

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
        push bc
        ld bc, var_base
        NEXT

        ;; Are we compiling or are we interpreting?
        cell_alloc(var_state,1)
        defcode("STATE",5,0,state)
        push bc
        ld bc, var_state
        NEXT

        ;; Address of the most recently defined word.
        ;; We have to do a very hacky thing.
var_latest:
        .dw name_star
        defcode("LATEST",6,0,latest)
        push bc
        ld bc, var_latest
        NEXT

        ;; Top (i.e. bottom) of the stack.
        cell_alloc(var_sz,0)
        defcode("S0",2,0,sz)
        push bc
        ld bc, var_sz
        NEXT


        defcode("x",1,0,lbrac)
        ld hl, var_state
        ld (hl), 0
        inc hl
        ld (hl), 0
        NEXT

        defcode("]",1,0,rbrac)
        ld hl, var_state
        ld (hl), 1
        inc hl
        ld (hl), 0
        NEXT
        

        cell_alloc(var_stack_empty,1)
        defcode("?SE", 3, 0, stack_emptyq)
        push bc
        ld hl, (var_stack_empty)
        ld b, h
        ld c, l
        NEXT

        cell_alloc(var_here,AppBackUpScreen)
        defcode("HERE",4,0,here)
        push bc
        ld bc, var_here
        NEXT


        ;; Here are some constants.
        defcode("DOCOL", 5, 0, __docol)
        PSP_PUSH(docol)
        NEXT

        defcode("BUF", 3, 0, __buffer)
        PSP_PUSH(str_buf)
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
        ld a, c
        cp 0
        jp z, zbranch_maybe
        jp nz, zbranch_fail

zbranch_maybe:
        ld a, b
        cp 0
        jp nz,zbranch_fail
        pop bc
        jp branch
zbranch_fail:
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
        call key_asm
        push bc
        ld c, a
        ld b, 0
        NEXT

key_asm:
        push bc
        push de
        b_call _GetKey
        pop de
        pop bc
        ret
        
        defcode("EMIT",4,0,emit)
        ld a, c
        b_call _PutC
        pop bc
        NEXT


        defcode(".", 1, 0, print_tos)
        BC_TO_HL
        call printhl_safe
        pop bc
        NEXT

        defword("?", 1, 0, peek_addr)
        .dw fetch, print_tos, exit

        ;; Convert a key code into an ASCII character by way of a
        ;; lookup table.

        ;; We need both because sometimes we might want to check for a
        ;; specific key that doesn't correspond to something
        ;; printable.
        defcode("AKEY",4,0,akey)
        push de
        call akey_asm
        pop de
        
        push bc
        ld b, 0
        ld c, a
        NEXT

;; Trashes hl, de, returns the ASCII character in register A If the
;; user pressed enter, this is recorded as $00.  This routine keeps
;; asking for input until a character that is in the table is input.
akey_asm:
        ;; First portion is copied from key.
        b_call _GetKey
        ;; a contains the byte received.
        ld h, 0
        ld l , a
        cp kSpace
        jp z, akey_return_space
        ld de, key_table
        ;; Add the offset
        add hl, de
        ld a, (hl)
        cp ' '
        jp z, akey_asm
        
        ret

akey_return_space:
        ld a, ' '
        ret

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
.db "       !" ;; 40-47
.db "   =  ' " ;; 48-55
.db "_@>     " ;; 56-63
.db " <      " ;; 64-71
.db "        " ;; 72-79
.db "        " ;; 80-87
.db "        " ;; 88-95
.db "        " ;; 96-103
.db "        " ;; 104-111
.db "        " ;; 112-119
.db "        " ;; 120-127
.db "+-*/^()",$C1 ;; 128-135
.db "]  , .01" ;; 136-143
.db "23456789" ;; 144-151
.db "  ABCDEF";; 152-159
.db "GHIJKLMN";; 160-167
.db "OPQRSTUV" ;; 168-175
.db "WXYZ    " ;; 176-183
.db "        " ;; 184-191
.db "      : " ;; 192-199
.db "  ?\"    " ;; 200-207
.db "        " ;; 208-215
.db "        " ;; 216-223
.db "        " ;; 224-231
.db "    {};\\" ;; 232-239
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

        defword("0",1,0,zero)
        .dw lit, 0, exit

        defword("1",1,0,one)
        .dw lit, 1, exit

        defword("2",1,0,two)
        .dw lit, 2, exit

        defword("3",1,0,three)
        .dw lit, 3, exit

        defword("4",1,0,four)
        .dw lit, 4, exit

        defword("5",1,0,five)
        .dw lit, 5, exit

        defword("6",1,0,six)
        .dw lit, 6, exit

        defword("7",1,0,seven)
        .dw lit, 7, exit

        defword("8",1,0,eight)
        .dw lit, 8, exit

        defword("9",1,0,nine)
        .dw lit, 9, exit

        defword("10",2,0,ten)
        .dw lit, 10, exit

        defword("TS",2,0,top_stack)
        .dw dup, print_tos, exit

        defword("STARS",5,0,stars)
        .dw star, dup, one_minus, zbranch, 6, branch, -12, exit

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


;; Get a full string of input (i.e. buffer user input for WORD and number etc.)
;; Input: none
;; Output: none
;; Side effect:
;; str_buf contains the user input.
;; gets_ptr points to the start of the buffer

;; We also want immediate feedback to the user.
;; 64 spaces
str_buf: .db "                                                                ",0
gets_ptr: .dw 0

        defcode("GETS",4,0,get_str_forth)
        push de
        push bc
        call get_str
        pop bc
        pop de
        NEXT
        
get_str:
        ld hl, gets_ptr
        ld de, str_buf

        ld (hl), e
        inc hl
        ld (hl), d
        dec hl

        xor a
        ld b, a ;; B will store how many characters we have read.
        ld (CurCol), a

;; GetKey destroys BC DE HL
key_loop:
        push hl
        push de
        push bc
        b_call _GetKey
        pop bc     
        pop de
        pop hl
        
        cp kEnter
        jp nz, not_enter

        ;; Got [ENTER].  Finish up.  Maybe the user hit [ENTER]
        ;; without entering anything, we need to check for that too.
        ld a, b
        or a
        jp z, no_chars
        
        xor a
        ld (de), a

        ret

no_chars:
        xor a
        ld (de), a
        inc de
        ld (de), a
        ret
not_enter:
        ;; Maybe it's the delete key?
        cp kDel
        jp nz, not_del

        ;; Yep.  Let's give some feedback.
        ld a, b
        or a
        ;; We're still at 0 characters!  Restart.
        jp z, key_loop

        ld hl, CurCol
        ld a, (hl)
        dec (hl)

        ;; Check if 0th column.
        or a
        jr nz, not_backup_line
        ;; Original column was 0, so we need to back up to the
        ;; previous line.
        ld (hl), 15
        dec hl
        dec (hl)

not_backup_line:
        dec de
        dec b
        ld a, ' '
        b_call _PutMap
        jp key_loop
        
not_del:
        cp kClear
        jr nz, not_clear

        ;; We have to clear everything!
        ld c, b
        ;; Divide C by 16.
        sra c
        sra c
        sra c
        sra c

        ld hl, CurRow
        ld a,b
        or a
        ;; No characters left.
        jr z, key_loop

        ld a, (hl)
        sub c
        ld c, a
        ld (hl), a

        inc hl
        ld (hl), 0
        ld a, ' '

clear_loop:
        b_call _PutC
        djnz clear_loop
        
        ld (hl), b
        dec hl
        ld (hl), c
        ld de, str_buf
        jp key_loop
        
not_clear:
        ld c, a
        ld a, b
        cp BUFSIZE
        jr z, key_loop
        ld a, c

        cp kSpace
        jp z, write_space

        ;; Convert to ASCII.
        ld h, 0
        ld l, a

        push de
        ld de, key_table
        add hl, de
        pop de
        
        ld a, (hl)
        ;; We got a space back, so it's not printable.  Try again.
        cp ' '
        jp z, key_loop
        jp write_char
write_space:
        ld a, ' '
write_char:
        b_call _PutC
        ld (de), a
        inc de
        inc b
        jp key_loop

;; Get the next character from the buffer.
;; A contains the next character from the buffer.
        defcode("GETC", 4, 0, get_char_forth)

        call get_char_asm
        push bc
        ld b, 0
        ld c, a
        NEXT
get_char_asm:
        push hl
        push de
        ld hl, gets_ptr
        ld de, (gets_ptr)
        
        ld a, (de)
        or a
        jp z, get_char_end
        inc de
        ld (hl), e
        inc hl
        ld (hl), d
get_char_end:
        pop de
        pop hl
        ret

unget_char:
        push hl
        push de
        ld hl, (gets_ptr)
        ld de, str_buf
        b_call _CpHLDE
        scf
        jr z, unget_char_done
        dec hl
        ld (gets_ptr), hl
        or a

unget_char_done:
        pop de
        pop hl
        ret

;; Skipping spaces, get the next word from the user.  Remember that
;; this needs to be flexible enough to be called from Forth as well.
;; We expect it to return a pointer to the next word following
;; gets_ptr.

;; ( -- base_addr len )
#define BUFSIZE  64
word_buf: .db "                                ",0
word_buf_ptr: .dw 0
        defcode("WORD",4,0,word)
        ;; Save IP and TOS.
        push bc
        push de
word_restart:
        ld hl, word_buf_ptr
        ld de, word_buf
        ;; Initialize word_buf_ptr to point at the actual start.
        ld (hl), e
        inc hl
        ld (hl), d
        dec hl
        jp skip_space
word_retry:
        push hl
        push de
        push bc
        call get_str
        pop bc
        pop de
        pop hl
skip_space:
        call get_char_asm
        or a
        jp z, empty_word  ;; get_char_asm returned nothing, so we need to retry with get_str
        cp ' '
        jp z, skip_space
        cp '\\'
        jp z, skip_comment
        jp actual_word
;; We really need a word.  Ask again!
empty_word:
        jp word_retry
skip_comment:
        ;; Since we know get_str reads one line of input, we can just
        ;; invoke WORD again to actually get the next word.
        push hl
        push de
        push bc
        call get_str
        pop bc
        pop de
        pop hl
        jp word_restart

actual_word:
        ld c, 1
        ;; A contains the character.        
        ld hl, (word_buf_ptr)
actual_word_write:        
        ld (hl), a
actual_word_loop:
        inc hl
        call get_char_asm
        or a
        jp z, word_done
        cp ' '
        jp z, word_done

        ;; A is another non-space, printable character.
        inc c
        jp actual_word_write
        
word_done:
        ;; Either read NUL or a space.
        xor a
        ld (hl), a
        pop de
        ld hl, word_buf
        ld b, 0  ;; c should contain the number of characters.
        push hl
        NEXT


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


        ;; Is this word immediate? (assuming it's a pointer returned by FIND)
        defcode("IMMED?",6,0, immedq)
        inc bc
        inc bc
        ld a, (bc)
        bit 7, a
        jp z, fal
        jp tru

        ;; Make the last word immediate
        defcode("IMMED",5,0, immediate)
        ld hl, (var_latest)
        inc hl
        inc hl
        ld a, 128
        xor (hl)
        ld (hl), a
        NEXT

        
        ;; Convert a pointer returned by FIND to the start of the name
        ;; field address (something I made up).
        defcode(">NFA",4,0,to_nfa)
        inc bc
        inc bc
        inc bc
        NEXT
        
        ;; Convert a pointer returned by FIND to the start of the code
        ;; field address.
        defcode(">CFA",4,0,to_cfa)
        inc bc
        inc bc ;; Skip the link pointer.
        ld a, (bc) ;; Get the length and flags of the word.
        and F_LENMASK ;; Remove flags except for length.
        ld h, 0
        ld l, a
        inc bc
        add hl, bc
        inc hl
        HL_TO_BC
        NEXT

        ;; Are two strings equal?
        ;; ( s1 s2 -- b )
        defcode("STR=",4,0,streql)
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
        inc de  ;; Skip link pointer and length
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
        cp 0 ;; or a
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
        cp 0 ;; or a
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
        .dw to_cfa, four_plus, one_minus, exit

        defcode("CREATE",6,0,create) ;; ( name length -- )
        ;; Create link pointer and update var_latest to point to it.
        ld hl, (var_here)
        PUSH_DE_RS
        ld de, (var_latest)
        ld (hl), e
        inc hl
        ld (hl), d
        dec hl

        ;; HL points to the new link pointer, so we should write its value into var_latest
        ld de, var_latest
        ld a, l
        ld (de), a
        inc de
        ld a, h
        ld (de), a

        inc hl
        inc hl
        ;; Now we have to write the length of the new word.
        ld a, c
        ld (hl), a
        inc hl

        ;; LDIR loads the value at (HL) to (DE), increments both,
        ;; decrements BC, loops until BC = 0.
        ex de, hl
        pop hl
        ld b, 0 ;; sanitize input, maybe?
        ldir

        xor a
        ld (de), a
        inc de

        ld hl, var_here
        ld (hl), e
        inc hl
        ld (hl), d

        POP_DE_RS
        pop bc
        NEXT

        ;; Recall that Forth words start with a call to docol.  The
        ;; opcode of call is CD <LOW> <HIGH> B6 9D seems to be the
        ;; address of DOCOL right now, but we shouldn't hardcode it so
        ;; we'll let the assembler do its job.
        defcode("DOCOL_H",7,0,docol_header)
        push de
        ld de, (var_here)
        ld a, $CD
        ld (de), a
        inc de
        ld hl, docol
        ;; Yes, this is correct.  We are writing a call docol instruction manually.
        ld a, l
        ld (de), a
        inc de
        ld a, h
        ld (de), a
        inc de
        ld hl, var_here
        ld (hl), e
        inc hl
        ld (hl), d
        pop de
        NEXT

        defword(":",1,0,colon)
        .dw space, word, create, docol_header
        .dw lbrac, exit

        defword(";",1,128, semicolon)
        .dw lit, exit, comma
        .dw rbrac, exit


        defcode("PAGE",4,0,page)
        push bc
        push de
        ld a, 0
        ld (currow), a
        ld (curcol), a
        b_call _ClrScrnFull
        pop de
        pop bc
        NEXT


        defcode("BYE",3,0,bye)
        jp done

	;; MAKE SURE THIS IS THE LAST WORD TO BE DEFINED!
	defword("STAR", 4, 0, star)
	.dw lit, 42, emit, exit

    
you_pressed_msg: .db "You pressed:", 0

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
        .dw akey, dup, emit
        .dw lit, 0, eql, zbranch, -14, done

;; Prints out the keycode entered, the character with that code.
;; Then converts the keycode into an ASCII character with the to_ascii word
;; Then prints the ASCII code and the character.

;; ...
;; <KEYCODE> <EMITTED KEYCODE> <TO_ASCII CONVERTED CHARACTER> <EMITTED CONVERTED NUMBER>
;; <KEYCODE> <EMITTED KEYCODE> <TO_ASCII CONVERTED CHARACTER> <EMITTED CONVERTED NUMBER>
;; ...

;; Handy for exploring table lookups and the to_ascii word
key_prog:
        .dw key, dup, dup, dup, print_tos, space, emit, space, to_ascii, dup
        .dw print_tos, space, emit, cr
        .dw lit, kEnter, eql
        .dw zbranch, -36
        .dw done

;; Demonstrate user input and random (?) number generation.
what_name_str: .db "What is your",0
what_name_str2: .dw "name?",0
luck_num_str: .db "Your lucky",0
luck_num_str2: .db "numbers are",0
hi_str: .db "Hello, ",0
fun_prog:
        .dw lit, what_name_str, putstrln
        .dw lit, what_name_str2, putstrln
        .dw word, drop, cr
        .dw lit, hi_str, putstr, space
        .dw putstrln
        .dw lit, luck_num_str, putstrln
        .dw lit, luck_num_str2, putstrln
        .dw rand, print_tos, space, rand, print_tos
        .dw done

;; Demonstrate word input and string comparsion.
;; msg1: .db "SECRET",0
;; guess: .db "Guess my secret:",0
;; secret_prog:
;;         .dw lit, guess, putstrln, lit, msg1
;;         .dw word, drop, lit, buffer, streql, print_tos
;;         .dw done
        
cfa_prog:
        .dw latest, fetch, dup, lit, 3, add, putstrln
        .dw dup, print_tos, cr
        .dw to_cfa, print_tos,cr
        .dw done

;; Test if CREATE works properly.
create_prog:
        .dw here, fetch, print_tos, cr
        .dw word, cr, create, here, fetch, print_tos, cr
        .dw lit, 1234, comma
        .dw here, fetch, print_tos, cr
        .dw here, fetch, one_minus, one_minus, fetch, print_tos, cr
        .dw done
        
;; Demonstrate that we can perform searches of the words.
search_msg: .db "Search: ",0
is_defined_msg: .db " is defined at memory location ",0
is_not_defined_msg: .db " is not defined.",0
the_word_msg: .db "The word ",0
repeat_msg: .db "Repeat?(Y/N) ",0
any_key_exit_msg: .db "Press any key to exit...",0
;; 46 emit = putchar('.')
;; search_prog:
;;         .dw cr
;;         .dw lit, search_msg, putstrln
;;         .dw word, drop, cr, find, dup
;;         .dw zbranch, 32, lit, the_word_msg, putstr, dup, to_nfa
;;         .dw putstr, lit, is_defined_msg, putstrln, print_tos, lit, 46, emit
;;         .dw branch, 22, drop, lit, the_word_msg, putstr, lit, buffer, putstr
;;         .dw lit, is_not_defined_msg, putstr, cr
;;         .dw lit, repeat_msg, putstr, key, to_ascii, dup, emit, lit, 89, eql, zbranch, 6, branch, -100
;;         .dw cr, lit, any_key_exit_msg, putstrln, done
        
;; ;; We're going to see if the dictionary was constructed correctly.
;; ;; Hold the right arrow key to fast forward through this.
;; words_prog:
;;         .dw latest, fetch, dup, lit, 3, add, putstr, space
;;         .dw fetch, dup
;;         .dw zbranch, 22
;;         .dw dup, lit, 3, add, putstr, space
;;         .dw key, drop
;;         .dw branch, -26
;;         .dw drop, done

setup_data_segment:
        ld de, AppBackUpScreen
        ld hl, var_here
        ld (save_sp), sp
        ld (hl), e
        inc hl
        ld (hl), d
        ld ix, return_stack_top
        ret

;; A REPL!  Only works with immediate words (i.e. no definitions yet),
;; but nevertheless it's amazing!
ok_msg: .db "ok",0
undef_msg: .db " ?",0
repl_prog:
        .dw word, drop, find, dup, zbranch, 20, to_cfa, space ;; drop the length before finding it (FIXME)
        .dw execute, space, lit, ok_msg, putstrln, branch, -28, drop, lit, undef_msg, putstrln, branch, -40, done
return_stack_top  .EQU    AppBackUpScreen+764
prog_exit: .dw 0
save_sp:   .dw 0


;; We should be able to define an interpreter in Forth that supports compiled code if we try.
prog:
        .dw get_str_forth
        .dw word, drop, find, dup, zbranch, 48
        .dw lit, var_state, fetch, zbranch, 18, to_cfa, execute, space
        .dw lit, ok_msg, putstrln, branch, -36, dup, immedq, zbranch, 6
        .dw branch, -26
        .dw to_cfa, comma, branch, -30, drop, lit, undef_msg, putstrln, branch, -68
        .dw done

getstr_prog:
        .dw get_str_forth, cr
        .dw word, print_tos, cr, lit, word_buf, putstrln
        .dw word, print_tos, cr, lit, word_buf, putstrln
        .dw done

.NOLIST

;;; Macros to make life easier.

;; NEXT, the basis of many Forth CODE words.

#define NEXT ld a, (de)
#defcont   \ ld l, a
#defcont   \ inc de
#defcont   \ ld a, (de)
#defcont   \ ld h, a
#defcont   \ inc de
#defcont   \ jp (hl)

;; Push BC to the return stack.
#define PUSH_BC_RS dec ix
#defcont         \ ld (ix + 0), b
#defcont         \ dec ix
#defcont         \ ld (ix + 0), c

;; Pop the top entry of the return stack to BC.
#define POP_BC_RS  ld c, (ix + 0)
#defcont         \ inc ix
#defcont         \ ld b, (ix + 0)
#defcont         \ inc ix

;; Push HL to the return stack.
#define PUSH_HL_RS dec ix
#defcont         \ ld (ix + 0), h
#defcont         \ dec ix
#defcont         \ ld (ix + 0), l

#define POP_HL_RS  ld l, (ix + 0)
#defcont         \ inc ix
#defcont         \ ld h, (ix + 0)
#defcont         \ inc ix

;; Push DE to the return stack.
#define PUSH_DE_RS dec ix
#defcont         \ ld (ix + 0), d
#defcont         \ dec ix
#defcont         \ ld (ix + 0), e

;; Pop the top entry of the return stack to DE.
#define POP_DE_RS  ld e, (ix + 0)
#defcont         \ inc ix
#defcont         \ ld d, (ix + 0)
#defcont         \ inc ix


;; Convience macros to transfer 16-bit register contents.

#define BC_TO_HL ld h, b
#defcont       \ ld l, c

#define HL_TO_BC ld b, h
#defcont       \ ld c, l

#define HL_TO_DE ld d, h
#defcont       \ ld e, l

#define BC_TO_DE ld d, b
#defcont       \ ld e, c

;; Our register allocations
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

        ;; This b_call pushes to the floating point stack, at memory
        ;; location $9824, below AppBackupScreen at $9872.
        b_call _PushRealO1


        pop bc ;; Save the place where this program needs to go.
        ld hl, prog_exit
        ld (hl), c
        inc hl
        ld (hl), b
        push bc

        ;; For some weird reason, either the spasm-ng assembler or TI-84
        ;; can't store the string "[", so we have to fix it by changing the value of rbrac's string.

        ld hl, name_rbrac
        inc hl
        inc hl
        inc hl
        ld (hl), 193

        ;; Same goes for S" and ."
        ld hl, name_s_quote
        inc hl
        inc hl
        inc hl
        inc hl
        ld (hl), 34

        ld hl, name_dot_quote
        inc hl
        inc hl
        inc hl
        inc hl
        ld (hl), 34

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
        b_call _PopRealO1
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

bit_cache: .dw 0
        ;; 16-bit AND operator, yay!
        defcode("AND",3,0,and)
        pop hl
        ld (bit_cache), hl

        ld hl, bit_cache
        ;; AND lowest byte first.
        ld a, c
        and (hl)
        ld (hl), a
        inc hl

        ld a, b
        and (hl)
        ld (hl), a
        dec hl

        ld a, (hl)
        ld c, a
        inc hl
        ld a, (hl)
        ld b, a
        NEXT

        defcode("OR",2,0,or)
        pop hl
        ld (bit_cache), hl

        ld hl, bit_cache

        ld a, c
        or (hl)
        ld (hl), a
        inc hl

        ld a, b
        or (hl)
        ld (hl), a
        dec hl

        ld a, (hl)
        ld c, a
        inc hl
        ld a, (hl)
        ld b, a
        NEXT

        defcode("XOR",3,0,xor)
        pop hl
        ld (bit_cache), hl

        ld hl, bit_cache

        ld a, c
        xor (hl)
        ld (hl), a
        inc hl

        ld a, b
        xor (hl)
        ld (hl), a
        dec hl

        ld a, (hl)
        ld c, a
        inc hl
        ld a, (hl)
        ld b, a
        NEXT

        defcode("<<",2, 0, left_shift)
        BC_TO_HL
        add hl, hl
        HL_TO_BC
        NEXT

        defcode(">>",2,0, right_shift)
        push de
        ld d, b
        ld e, c
        srl d
        rr e
        ld b, d
        ld c, e
        pop de
        NEXT
        
        ;; Bitwise NOT.
        defcode("INVERT",6,0,invert)
        ld a, c
        cpl
        ld c, a

        ld a, b
        cpl
        ld b, a
        
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
        HL_TO_BC
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

        defcode("2+", 2, 0, two_plus)
        inc bc
        inc bc
        NEXT
        
        defcode("2-", 2, 0, two_minus)
        dec bc
        dec bc
        NEXT        

        defcode(">R", 2, 0, to_r)
        PUSH_BC_RS
        pop bc
        NEXT

        defcode("R>", 2, 0, from_r)
        push bc
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

        defcode("LITSTR",6,0,litstring)
        ;; String length
        ld a, (de)
        ld l, a
        inc de
        ld a, (de)
        ld h, a
        inc de

        push bc ;; old stack top
        push de ;; push address of string
        HL_TO_BC ;; BC now contains the string length
        
        ;; Skip the string.
        add hl, de
        ;; Skip null pointer.  (Even though we have the length, because
        ;; we don't have a bcall Linux that can print out a string with
        ;; a certain length).
        
        inc hl
        ex de, hl
        NEXT

        defword("SQ",2,128,s_quote)
        .dw state, fetch, zbranch, 54, here, fetch, get_char_forth, dup, lit, 34
        .dw neql, zbranch, 12, over, store_byte, one_plus, branch, 65514, drop
        .dw over, zero, store_byte, here, fetch, sub, here, fetch, swap, branch, 60
        .dw tick, litstring, comma, here, fetch, zero, comma, get_char_forth, dup
        .dw lit, 34, neql, zbranch, 8, c_comma, branch, 65518, drop, zero, c_comma
        .dw dup, here, fetch, swap, sub, three, sub, swap, store, exit
        
        .dw state, fetch, not, zbranch, 64, tick, litstring, comma, here, fetch
        .dw zero, comma, get_char_forth, dup, lit, 34, neql, zbranch, eight, c_comma
        .dw branch, 65518, drop, zero, c_comma, dup, here, fetch, swap, sub, three, sub
        .dw swap, store, branch, 44, here, fetch, get_char_forth, dup, lit, 34, neql, zbranch
        .dw 12, over, store_byte, one_plus, branch, 65514, drop, here, fetch, sub, here
        .dw fetch, swap, exit

        defword(".Q",2,128,dot_quote)
        .dw state, fetch, zbranch, 30, get_char_forth, dup, lit, 34, eql
        .dw zbranch, 6, drop, exit, emit, branch, 65514, branch, 10, s_quote
        .dw tick, tell, comma, exit

        defcode("TELL",4,0,tell)
        pop bc
        BC_TO_HL
        b_call _PutS
        pop bc
        NEXT

        defcode("STRLEN",6,0,strlen)
        BC_TO_HL
        ;; Taken from the KnightOS kernel.
        push af
        push hl
        xor a
        ld b, a
        ld c, a
        cpir
        ; bc = -bc
        ld a,c \ cpl \ ld c, a \ ld a,b \ cpl \ ld b, a
        pop hl
        pop af
        NEXT

        ;; Return a pointer to the first occurrence of a character in a string
        ;; Taken from the KnightOS kernel.
        defcode("STRCHR",6,0,strchr) ; ( char str -- addr )
        BC_TO_HL
        pop bc
        ld b, c

strchr_loop:
        ld a, (hl)
        or a
        jr z, strchar_fail
        cp b
        jp z, tru
        inc hl
        jr strchr_loop
strchar_fail:
        jp fal

        
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
        pop bc
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
        ;; ( source destination amount -- )
        PUSH_DE_RS
        pop de
        pop hl
        ldir
        POP_DE_RS
        pop bc
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

        ;; Base of the parameter stack.
        cell_alloc(var_sz, 0)
        defcode("SP0",3,0,sz)
        push bc
        ld bc, var_sz
        NEXT

        ;; The "x" gets replaced with "[" at program start, see "start:"
        defcode("x",1,128,rbrac)
        ld hl, var_state
        ld (hl), 0
        inc hl
        ld (hl), 0
        NEXT

        defcode("]",1,0,lbrac)
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

        cell_alloc(var_here,0)
        defcode("HERE",4,0,here)
        push bc
        ld bc, var_here
        NEXT


        ;; Here are some constants.
        defcode("DOCOL", 5, 0, __docol)
        PSP_PUSH(docol)
        NEXT

        defcode("BUF", 3, 0, __string_buffer)
        PSP_PUSH(string_buffer)
        NEXT

        defcode("BUFSZ", 5, 0, __string_buffer_size)
        PSP_PUSH(STRING_BUFFER_SIZE)
        NEXT

        defcode("WBUF", 4, 0, __word_buffer)
        PSP_PUSH(word_buffer)
        NEXT

        defcode("WBUFSZ", 6, 0, __word_buffer_size)
        PSP_PUSH(word_buffer)
        NEXT

        defcode("RP0", 3, 0, rz)
        PSP_PUSH(return_stack_top)
        NEXT

        defcode("H0", 2, 0, hz)
        PSP_PUSH(here_start)
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

        defcode("SCR",3,0,__scratch)
        PSP_PUSH(scratch)
        NEXT

        defcode("PLOTSS",6,0,__plot_s_screen)
        PSP_PUSH(plotSScreen)
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

        defcode("C,",2,0,c_comma)
        call _c_comma
        pop bc
        NEXT
_c_comma
        push de
        ld hl, (var_here)
        ld (hl), c
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

        ;; Since we can't do ld hl, sp
        push bc
        ld (var_sp), sp
        ld hl, (var_sp)
        HL_TO_BC
        NEXT

var_sp:
        .dw 0

        defcode("SP!", 3, 0, sp_store)
        BC_TO_HL
        ld sp, hl
        pop bc ;; new top of stack
        NEXT


        defcode("RP@",3,0,rp_fetch)
        push ix
        NEXT

        defcode("RP!",3,0,rp_store)
        pop ix
        NEXT
        
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

        defcode("0BRANCH", 7, 0, zbranch)
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

        defcode(">=", 2,0,greater_eq)
        pop hl
        call cpHLBC
        jp nc, tru
        jp fal

        defcode("<=", 2,0,less_eq)
        BC_TO_HL
        pop bc
        call cpHLBC
        jp nc, tru
        jp fal

        defcode("<",1,0,less_than)
        pop hl
        call cpHLBC
        jp c, tru
        jp fal

        defcode(">",1,0,greater_than)
        pop hl
        push hl
        push bc
        call cpHLBC
        pop bc
        pop hl        
        jp nc, gt_check_neq
        jp fal
gt_check_neq:
        call cpHLBC
        jp z, fal
        jp tru        

        defcode("0=",2,0,zeql)
        ld hl, 0
        call cpHLBC
        jp c, fal
        jp tru

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
        ld bc, 0
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

        
str_println:
        b_call _PutS
        b_call _Newline
        ret
        
str_print:
        b_call _PutS
        ret

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

        ;; Get a key but non-blocking.
        defcode("KEYC", 4, 0, keyc)
        push bc
        b_call _GetCSC
        ld c, a
        ld b, 0
        NEXT
        
        defcode("EMIT",4,0,emit)
        ld a, c
        b_call _PutC
        pop bc
        NEXT


        ;; Print the top of the stack.
        defcode("T.", 1, 0, t_dot)
        BC_TO_HL
        call printhl_safe
        ld a, ' '
        b_call _PutC
        pop bc
        NEXT

        defcode("?", 1, 0, peek_addr)
        ld a, (bc)
        ld l, a
        inc bc
        ld a, (bc)
        ld h, a
        call printhl_safe
        pop bc
        NEXT

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



;; mul16By16 [Maths]
;;  Performs an unsigned multiplication of DE and BC.
;; Inputs:
;;  DE: Multiplier
;;  BC: Multiplicand
;; Outputs:
;;  DEHL: Product of DE and BC.
mul16By16:

        defcode("*",1,0,mult)
        PUSH_DE_RS
        pop de ;; get the second element from the stack
        
    push bc
        push af
            ld hl, 0
            ld a, b
            ld b, h
            or a
                        rla \ jr nc, $+5 \ ld h, d \ ld l, e
            add hl, hl \ rla \ jr nc, $+4 \ add hl, de \ adc a, b
            add hl, hl \ rla \ jr nc, $+4 \ add hl, de \ adc a, b
            add hl, hl \ rla \ jr nc, $+4 \ add hl, de \ adc a, b
            add hl, hl \ rla \ jr nc, $+4 \ add hl, de \ adc a, b
            add hl, hl \ rla \ jr nc, $+4 \ add hl, de \ adc a, b
            add hl, hl \ rla \ jr nc, $+4 \ add hl, de \ adc a, b
            add hl, hl \ rla \ jr nc, $+4 \ add hl, de \ adc a, b
            ld b, a
            push hl
            ld hl, 0
            ld a, c
            ld c, h
            or a
                        rla \ jr nc, $+5 \ ld h, d \ ld l, e
            add hl, hl \ rla \ jr nc, $+4 \ add hl, de \ adc a, c
            add hl, hl \ rla \ jr nc, $+4 \ add hl, de \ adc a, c
            add hl, hl \ rla \ jr nc, $+4 \ add hl, de \ adc a, c
            add hl, hl \ rla \ jr nc, $+4 \ add hl, de \ adc a, c
            add hl, hl \ rla \ jr nc, $+4 \ add hl, de \ adc a, c
            add hl, hl \ rla \ jr nc, $+4 \ add hl, de \ adc a, c
            add hl, hl \ rla \ jr nc, $+4 \ add hl, de \ adc a, c
            ld d, b
            pop bc
            ld e, a
            ld a, c
            add a, h
            ld h, a
            ld a, e
            adc a, b
            ld e, a
            jr nc, $ + 3
            inc d
        pop af
    pop bc

       
        HL_TO_BC
        POP_DE_RS
        NEXT



        ;; ( a b -- remainder quotient )
        defcode("/MOD", 4, 0, divmod)
        PUSH_DE_RS
        ld d, b
        ld e, c
        pop bc
        ld a, b
divACbyDE:
        ld hl, 0
        .db $CB, $31 ; sll c
        rla
        adc hl, hl
        sbc hl, de
        jr nc, $+4
        add hl, de
        dec c
        .db $CB, $31 ; sll c
        rla
        adc hl, hl
        sbc hl, de
        jr nc, $+4
        add hl, de
        dec c
        .db $CB, $31 ; sll c
        rla
        adc hl, hl
        sbc hl, de
        jr nc, $+4
        add hl, de
        dec c
        .db $CB, $31 ; sll c
        rla
        adc hl, hl
        sbc hl, de
        jr nc, $+4
        add hl, de
        dec c
        .db $CB, $31 ; sll c
        rla
        adc hl, hl
        sbc hl, de
        jr nc, $+4
        add hl, de
        dec c
        .db $CB, $31 ; sll c
        rla
        adc hl, hl
        sbc hl, de
        jr nc, $+4
        add hl, de
        dec c
        .db $CB, $31 ; sll c
        rla
        adc hl, hl
        sbc hl, de
        jr nc, $+4
        add hl, de
        dec c
        .db $CB, $31 ; sll c
        rla
        adc hl, hl
        sbc hl, de
        jr nc, $+4
        add hl, de
        dec c
        .db $CB, $31 ; sll c
        rla
        adc hl, hl
        sbc hl, de
        jr nc, $+4
        add hl, de
        dec c
        .db $CB, $31 ; sll c
        rla
        adc hl, hl
        sbc hl, de
        jr nc, $+4
        add hl, de
        dec c
        .db $CB, $31 ; sll c
        rla
        adc hl, hl
        sbc hl, de
        jr nc, $+4
        add hl, de
        dec c
        .db $CB, $31 ; sll c
        rla
        adc hl, hl
        sbc hl, de
        jr nc, $+4
        add hl, de
        dec c
        .db $CB, $31 ; sll c
        rla
        adc hl, hl
        sbc hl, de
        jr nc, $+4
        add hl, de
        dec c
        .db $CB, $31 ; sll c
        rla
        adc hl, hl
        sbc hl, de
        jr nc, $+4
        add hl, de
        dec c
        .db $CB, $31 ; sll c
        rla
        adc hl, hl
        sbc hl, de
        jr nc, $+4
        add hl, de
        dec c
        .db $CB, $31 ; sll c
        rla
        adc hl, hl
        sbc hl, de
        jr nc, $+4
        add hl, de
        dec c

        ld b, a
        ;; Remainder, then quotient.
        push hl
        POP_DE_RS
        NEXT
     
        ;; Ad-hoc solution to read a number.

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


        defcode("SPACE",5,0,space)
        ld a, ' '
        b_call _PutC
        NEXT

        defcode("CR",2,0,cr)
        b_call _Newline
        NEXT

        defcode("AT-XY",5,0,atxy)
        ld a, c
        ld (curRow), a
        pop bc
        ld a, c
        ld (curCol), a
        pop bc
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
;; string_buffer contains the user input.
;; gets_ptr points to the start of the buffer

;; We also want immediate feedback to the user.
#define STRING_BUFFER_SIZE 128

;; We can have a default program!
string_buffer: .fill 128,0
gets_ptr: .dw string_buffer

        defcode("GETS",4,0,get_str_forth)
        push de
        push bc
        call get_str
        pop bc
        pop de
        NEXT
        
get_str:
        ld hl, gets_ptr
        ld de, string_buffer

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

        ;; We should echo enter.
        b_call _NewLine
        
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
        jp z, is_del

        ;; For convenience, the left arrow key in alpha mode also
        ;; works as a DEL.

        cp kLeft
        jp nz, not_del

is_del:
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
        ld de, string_buffer
        jp key_loop
        
not_clear:
        ld c, a
        ld a, b
        cp STRING_BUFFER_SIZE
        jr z, key_loop
        ld a, c

        ;; Special keys that actually write a space.
        cp kSpace
        jp z, write_space

        cp kRight ;; right arrow, alpha mode
        jp z, write_space

        ;; Convert to ASCII.
        ld h, 0
        ld l, a

        push de
        ld de, key_table
        add hl, de
        pop de
        
        ld a, (hl)
        ;; We got a space back as per the table, so it's not printable.  Try again.
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


        defcode("UNGETC", 6, 0, unget_char_forth)

        call unget_char

        NEXT
unget_char:
        push hl
        push de
        ld hl, (gets_ptr)
        ld de, string_buffer
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
word_buffer:     .fill 64, 0
word_buffer_ptr: .dw 0
        defcode("WORD",4,0,word)
        ;; Save IP and TOS.
        push bc
        push de
word_restart:
        ld hl, word_buffer_ptr
        ld de, word_buffer
        ;; Initialize word_buffer_ptr to point at the actual start.
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
        ld hl, (word_buffer_ptr)
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
        ld hl, word_buffer
        ld b, 0  ;; c should contain the number of characters.
        push hl
        NEXT


        ;; Is this word immediate? (assuming it's a pointer returned by FIND)
        defcode("?IMMED",6,0, qimmed)
        inc bc
        inc bc
        ld a, (bc)
        bit 7, a
        jp z, fal
        jp tru

        ;; Make the last word immediate
        defcode("IMMED",5,128, immediate)
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
        ;; ( s1 s2 -- bool )
        defcode("STR=",4,0,streql)
        PUSH_DE_RS
        pop de
        BC_TO_HL
        call strcmp
        jp z, tru
        jp fal

        ;; ( string-ptr length-- pointer to word )
        defcode("FIND",4,0,find)
        pop bc ;; FIXME: Check length.
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
        ;; We found the word.  But is it hidden?
        dec de
        ;; Now we're at the length/flags pointer.
        ld a, (de)
        bit 6, a
        jp nz, find_succ_hidden
        dec de
        dec de
        pop hl
        ex de,hl
        HL_TO_BC
        NEXT
find_retry:
        dec de
find_succ_hidden:        
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
        or a
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

        defcode("WB",2,0,writeback)
        push bc
        push de
        b_call _PopRealO1 ;; from the floating point stack
        b_call _PushRealO1
        b_call _ChkFindSym
    
        ld    hl, data_start - $9D95 + 4    ; have to add 4 because of tasmcmp token
                                            ; (2 bytes) and for size bytes (2 bytes)
        add    hl, de        ;hl now points to data location in original program.
        ex    de, hl         ;write back.
        ld    hl, data_start
        ld    bc, data_end - data_start
        ldir
        
        pop de
        pop bc
        NEXT


        ;; How many bytes have we used?
        defword("USED",4,0,used)
        .dw here, fetch, hz, sub, exit

        ;; Save the current state into the scratch buffer.
        ;; Assuming the current state is the data after the buffer 
        defword("SIMG",4,0,save_image)
        .dw here, fetch, lit, save_here, store
        .dw latest, fetch, lit, save_latest, store
        .dw hz, lit, scratch, used, cmove, writeback, exit

        ;; Load the scratch buffer back into the current state.
        defword("LIMG",4,0,load_image)
        .dw lit, save_here, fetch, here, store
        .dw lit, save_latest, fetch, latest, store
        .dw lit, scratch, hz, lit, save_here, fetch, hz, sub, cmove, exit

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
        ;; Opcode of CALL
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
        .dw word, create, docol_header
        .dw latest, fetch, hidden
        .dw rbrac, exit

        defword(";",1,128, semicolon)
        .dw lit, exit, comma
        .dw latest, fetch, hidden
        .dw lbrac, exit

        ;; Compile a call in the new word where call DOCOL used to be.
        defcode("(DOES>)",7,0,does_brac)
        push bc
        ld bc, (var_latest)
        inc bc
        inc bc ;; Skip the link pointer.
        ld a, (bc) ;; Get the length and flags of the word.
        and F_LENMASK ;; Remove flags except for length.
        ld h, 0
        ld l, a
        inc bc
        add hl, bc
        inc hl
        inc hl
        ;; Now we need to overwrite the destination of call docol with DE
        ld (hl), e
        inc hl
        ld (hl), d
        pop bc
        
        ;; Mimic exit
        POP_DE_RS
        NEXT
        
        ;; DE contains the address of the next instruction to go to
        ;; Which is the "action" part of the word being defined with DOES>.

        defcode("DOES>",5,128,does_start)
        push de
        ld de, (var_here)
        ld hl, does_brac
        ld a, l
        ld (de), a
        inc de
        ld a, h
        ld (de), a
        inc de
        ld a, $CD
        ld (de), a
        inc de
        
        ld hl, dodoes
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

dodoes:
        PUSH_DE_RS
        ;; Get return address of the DOES> body
        pop de
        BC_TO_HL
        ;; The top of the stack contains the address of the data folloinwg
        ;; call dodoes.  We want this on the top of the stack.
        pop bc
        push hl
        NEXT
        


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


        defcode("HIDDEN",6,0,hidden)
        BC_TO_HL
        inc hl
        inc hl
        ld a, 64
        xor (hl)
        ld (hl), a
        pop bc
        NEXT

        defcode("?HIDDEN",7,0,qhidden)
        BC_TO_HL
        inc hl
        inc hl
        ld a, 64
        and (hl)
        ld b, 0
        ld c, a
        NEXT

        defword("MOD",3,0, mod)
        .dw divmod, drop, exit

        defword("/",1,0,div)
        .dw divmod, swap, drop, exit

        defword("NEGATE",6,0,negate)
        .dw lit, 0, swap, sub, exit

        defword("TRUE",4,0,true_val)
        .dw lit, 1, exit

        defword("FALSE",5,0,false_val)
        .dw lit, 0, exit

        defword("NOT",3,0,not)
        .dw zeql, exit

        defword("LITERAL",7,128,literal)
        .dw tick, lit, comma, comma, exit

        defcode("NIP",3,0,nip)
        pop hl
        NEXT
        
        defcode("TUCK",4,0,tuck)
        pop hl
        push bc
        push hl
        NEXT

        defword("ID.",3,0,id_dot)
        .dw lit, 3, add, putstr, exit

        defword("HIDE",4,0,hide)
        .dw word, find, hidden, exit

        defword("IF",2,128,if)
        .dw tick, zbranch, comma, here, fetch, lit, 0, comma, exit

        defword("THEN",4,128,then)
        .dw dup, here, fetch, swap, sub, swap, store, exit

        defword("ELSE",4,128,else)
        .dw tick, branch, comma, here, fetch, lit, 0, comma, swap, dup, here
        .dw fetch, swap, sub, swap, store, exit

        defword("BEGIN",5,128,begin)
        .dw here, fetch, exit

        defword("UNTIL",5,128,until)
        .dw tick, zbranch, comma, here, fetch, sub, comma, exit

        defword("AGAIN",5,128,again)
        .dw tick, branch, comma, here, fetch, sub, comma, exit

        defword("WHILE",5,128,while)
        .dw tick, zbranch, comma, here, fetch, lit, 0, comma, exit

        defword("REPEAT",6,128,repeat)
        .dw tick, branch, comma, swap, here, fetch, sub, comma
        .dw dup, here, fetch, swap, sub, swap, store, exit
        
        defword("CHAR",4,0,char)
        .dw word, two_drop, lit, word_buffer, fetch_byte, exit

        defword("(COMP)",6,128,compile)
        .dw word, find, to_cfa, comma, exit

        defword("CONST",5,0,constant)
        .dw word, create, docol_header, lit, lit, comma, comma
        .dw lit, exit, comma, exit

        defword("ALLOT",5,0,allot)
        .dw here, fetch, swap, here, add_store, exit

        defword("CELLS",4,0,cells)
        .dw lit, 2, mult, exit

        defword("RECURSE",7,128,recurse)
        .dw latest, fetch, to_cfa, comma, exit

        defword("VAR",3,0,variable)
        .dw lit, 1, cells, allot, word, create, docol_header, lit, lit, comma, comma
        .dw lit, exit, comma, exit
 
        defword("DO",2,128, do)
        .dw here, fetch, tick, to_r, comma, tick, to_r, comma, exit

        defword("LOOP",4,128, loop)
        .dw tick, from_r, comma, tick, from_r, comma, tick, one_plus, comma, tick, two_dup, comma
        .dw tick, eql, comma, tick, zbranch, comma, here, fetch,  sub, comma, tick, two_drop, comma, exit


        defword("+LOOP",5,128, add_loop)
        .dw tick, from_r, comma, tick, from_r, comma, tick, rot, comma, tick, add, comma, tick, two_dup, comma
        .dw tick, eql, comma, tick, zbranch, comma, here, fetch,  sub, comma, tick, two_drop, comma, exit

        defword("FORGET",6,0,forget)
        .dw word, find, dup, fetch, latest, store, here, store, exit

        defcode("'0'",3,0,zeroc)
        push bc
        ld bc, 48
        NEXT

        defcode("'9'",3,0,ninec)
        push bc
        ld bc, 57
        NEXT

        defword("WITHIN",6,0,within)
        .dw over, sub, to_r, sub, from_r, less_eq, exit

        defword("NUM?",4,0,numq)
        .dw zeroc, ninec, within, exit

        defword("NUM",3,128,num)
        .dw zero, get_char_forth, dup, numq, not, zbranch, 30, drop, state, fetch, zbranch, 6
        .dw branch, 10, tick, lit, comma, comma, exit, branch, 14, zeroc, sub
        .dw swap, ten, mult, add, branch, -54, exit

        defword("CFA>",4,0, cfa_to)
        .dw latest, fetch, qdup, zbranch, 22, two_dup, swap
        .dw less_than, zbranch, 6, nip, exit, fetch, branch, -24, drop, zero, exit

        defword("PICK",4,0,pick)
        .dw one_plus, left_shift, sp_fetch, add, fetch, exit

        defword("U.",2,0,u_dot_)
        .dw base, fetch, divmod, qdup, zbranch, 4, u_dot_, dup, ten, less_than, zbranch, 10
        .dw lit, 48, branch, 10, ten, sub, lit, 65, add, emit, exit

        defword("UWIDTH",6,0,u_width)
        .dw base, fetch, div, qdup, zbranch, 10, u_width, one_plus, branch, 4, one, exit

        defword("SPACES",6,0,spaces)
        .dw zero, to_r, to_r, space, from_r, from_r, one_plus, two_dup, eql, zbranch, 65518
        .dw two_drop, exit

        defword("U.R",3,0,u_dot_r)
        .dw swap, dup, u_width, rot, swap, sub, spaces, u_dot_, exit

        defword("U.",2,0,u_dot)
        .dw u_dot_, space, exit
        

        defword(".",1,0,print_tos)
        .dw u_dot, exit

        defword(".S",2,0,print_stack)
        .dw sp_fetch, dup, sz, fetch, less_than, zbranch, 16, dup, fetch, u_dot
        .dw two, add, branch, 65512, drop, exit
        
        ;; This word was bootstrapped from an interpreted definition.
        defword("SEE",3,0,see)
        .dw word, find, here, fetch, latest, fetch, lit, 2, pick, over, neql, zbranch, 12
        .dw nip, dup, fetch, branch, 65514, drop, swap, lit, 58, emit, space, dup, id_dot
        .dw space, dup, qimmed, zbranch, 10, lit, 73, emit, space, to_dfa, key, drop
        .dw two_dup, greater_than, zbranch, 196, dup, fetch, tick, lit, over, eql, zbranch, 16
        .dw drop, two_plus, dup, fetch, u_dot, branch, 160, tick, zbranch, over, eql, zbranch, 30
        .dw drop, lit, 48, emit, lit, 66, emit, two_plus, dup, fetch, space, u_dot, branch, 120
        .dw tick, branch, over, eql, zbranch, 24, drop, lit, 66, emit, space, two_plus, dup, fetch
        .dw u_dot, branch, 86, tick, tick, over, eql, zbranch, 28, drop, lit, 39, emit, space, two_plus
        .dw dup, fetch, cfa_to, id_dot, space, branch, 48, tick, exit, over, eql, zbranch, 26, drop
        .dw two_dup, two_plus, neql, zbranch, 10, lit, 69, emit, space, branch, 12, dup, cfa_to
        .dw id_dot, space, drop, two_plus, branch, 65332, lit, 59, emit, cr, two_drop, exit

        defword("WORDS",5,0,words)
        .dw latest, fetch, key, drop, qdup, zbranch, 24, dup, qhidden, not, zbranch, 8
        .dw dup, id_dot, space, fetch, branch, 65506, cr, exit



        defword("CASE",4,128,case)
        .dw zero, exit

        defword("OF", 2, 128, of)
        .dw tick, over, comma, tick, eql, comma, if, tick, drop, comma, exit

        defword("ENDOF", 5, 128, endof)
        .dw else, exit

        defword("ENDCASE", 7, 128, endcase)
        .dw tick, drop, comma, qdup, zbranch, 8, then, branch, -10, exit


        defcode("I",1,0,curr_loop_index)
        push bc
        ld c, (ix + 2)
        ld b, (ix + 3)
        NEXT


;; Taken from http://z80-heaven.wikidot.com/sound

        ;; ( frequency duration -- )
        defcode("SMIT",4,0,sound_emit)
        BC_TO_HL
        pop bc
        push de
        call p_FreqOut
        pop de
        pop bc
        NEXT
p_FreqOut:
;Inputs:
;     HL is the duration of the note
;     BC is the frequency
    xor    a
FreqOutLoop1:
    push    bc
    xor     3    ;this will toggle the lower two bits (the data being sent to the link port)
    ld    e,a
FreqOutLoop2:
    ld    a,h
    or    l
    jr    z,FreqOutDone
    cpd
    jp    pe,FreqOutLoop2
    ld    a,e
    scf
FreqOutDone:
    pop    bc
    out    (0),a
    jr    c,FreqOutLoop1
    xor b
    nop
    nop
    out (0),a       ;reset the port, else the user will be really annoyed.
    ret

        defcode("IN0",3,0,in_zero)
        push bc
        push de
        ld a, c
        call read_z
        pop de
        pop bc
        NEXT

        defcode("W0",2,0,write_zero)
        push bc
        push de
        call write_z
        pop de
        ld b,0
        ld c,a
        NEXT
        
;; Read data from the link port.
read_z:
	ld b,0			; reset variables (b = byte,
	ld d,1			; d = bitmask, e = clockstate)
	in a,(0)		; Get byte and check tip
	bit 2,a
	call z,State_zero
	call nz,State_one
Read_go:
	in a,(0)		; Is clockstate changed?
	bit 2,a
	ld a,e
	jr z,Clock_is_low
	or a
	jr z,Clockchanged	; Yes (High)
	jr Read_go		; No
Clock_is_low:
	or a
	jr z,Read_go		; No  (Low)
Clockchanged:			; Yes
	in a,(0)		; Get value from port
	bit 2,a
	call z,State_zero	; Store new clockstate
	call nz,State_one
	bit 3,a
	call nz,Or_byte	; Or them, depending on state of ring
	rlc d
	ld a,d
	cp 1
	jr z,Stop_read		; Yes: quit
	jr Read_go		; No, next bit
Or_byte:
	ld a,b
	or d
	ld b,a
	ret
State_zero:
	ld e,0
	ret
State_one:
	ld e,1
	ret
Stop_read:
	ld a,b
	ret

Write_z:
	ld c,a			; Store byte
	ld d,1			; Create bitmask
	ld e,$D1		; Init linkport value
Write_go:
	ld a,c			; Retrieve byte
	and d			; and with bitmask
	or a
	call z,Set_ring_low
	call nz,Set_ring_high	; Set data line (ring) according to bit
	rlc d			; rotate bitmask
	ld a,e			; retrieve linkport value
	out (0),a		; Set linkport
	xor 1			; invert clockstate
	ld e,a			; store linkport value
	ld b,6
Delay_loop:
	djnz Delay_loop	; Short delay
	ld a,d			; bitmask back to original?
	cp 1
	jr nz,Write_go		; No: Next bit
	ret			; Yes: Done
Set_ring_high:
	res 1,e
	ret
Set_ring_low:
	set 1,e
	ret

        
;; FrequencyLUT:
;;  .dw 2100,1990,1870,1770,1670,1580,1490,1400,1320,1250,1180,1110
;;  .dw 1050, 996, 940, 887, 837, 790, 746, 704, 665, 627, 592, 559
;;  .dw  527, 498, 470, 444, 419, 395, 373, 352, 332, 314, 296, 279
;;  .dw  264, 249, 235, 222, 209, 198, 186, 176, 166, 157, 148, 140
;;  .dw  132, 124, 117, 111, 105,  99,  93,  88,  83,  78,  74,  70
;;  .dw   66,  62,  59,  55,  52,  49,  47,  44,  42,  39,  37,  35
;;  .dw   33,  31,  29,  28,  26,  25,  23,  22,  21,  20,  19,  18
;;  .dw   17,  16,  15,  14,  13,  12,  11,  10,  10,   9,   9,   8
;;  .dw    8,   7,   7,   7,   7,   6,   6,   5,   5,   5,   5,   4
 
;; ;; Play note A for duration D
;; ;; Taken from http://z80-heaven.wikidot.com/sound
;; ;; Work in progress.  Somewhat buggy.

;;         ;; (  note duration -- )
;;         defcode("PLAY",4,0,play_note)
;;         PUSH_DE_RS
;;         ld d, c
;;         pop bc
;;         ld a, c
;; PlayNote:
;;         ld hl,FrequencyLUT
;;         add a,l
;;         ld l,a
;;         jr nc, PlayNote_cont
;;         inc h
;;         ld c,(hl)
;;         inc hl
;; PlayNote_cont:        
;;         ld b,(hl)
;; ;now BC is the frequency for the note
;; NoteLoop:
;;         push bc
;;         ld hl,4096
;;         call p_FreqOut
;;         pop bc
;;         dec bc
;;         ld a,b
;;         or c
;;         jr nz,NoteLoop
;;         POP_DE_RS
;;         pop bc
;;         NEXT


        defcode("PLOT",4,0,plot)
        push bc
        push de
        b_call _GrBufCpy
        pop de
        pop bc
        NEXT



        ;; ( x y -- addr bitmask )
        defcode("GETP", 4, 0, get_pixel_forth)
        ld l, c
        pop bc
        ld a, c
        push de

        call get_pixel
        pop de
        push hl
        ld b, 0
        ld c, a
        NEXT

;; Given the x-coordinate in A and the y-coordinate in L, return in HL
;; the address and a bitmask in A.
get_pixel:
        ld     h, 0
        ld     d, h
        ld     e, l
        add    hl, hl
        add    hl, de
        add    hl, hl
        add    hl, hl
    
        ld     e, a
        srl    e
        srl    e
        srl    e
        add    hl, de
    
        ld     de, plotSScreen
        add    hl, de
        and 7
        ld a, $80
        ret z
        ld b, a
        
get_pixel_loop:
        rrca
        djnz get_pixel_loop
        ret


        ;; Darken a pixel
        defcode("DARKP",5,0,darken_pixel)
        ld l, c
        pop bc
        ld a, c
        push de
        call   get_pixel
        or     (hl)
        ld     (hl), a
        pop de        
        pop bc
        NEXT
        
        
        defcode("TOGP",4,0,toggle_pixel)
        ld l, c
        pop bc
        ld a, c
        push de
        call   get_pixel
        xor    (hl)
        ld     (hl), a
        pop de                
        pop bc
        NEXT

        defcode("LITP",4,0,lighten_pixel)
        ld l, c
        pop bc
        ld a, c
        push de
        call   get_pixel
        cpl
        and    (hl)
        ld     (hl), a
        pop de                      
        pop bc
        NEXT


;; Creating an editor.  Rough idea: We want to have a block-editing
;; system to be able to save and read programs.  We use the small
;; variable width font instead of the large one, so that we may place
;; it on the screen.

        defword("WR", 2, 0, write)
        ;; The second get_str_forth is necessary as we don't want the
        ;; interpreter to read the entered text
        .dw cr, get_str_forth, cr, lit, word_buffer, swap, __string_buffer_size,  cmove, get_str_forth, exit
        
        defcode("PN", 2, 0, print_nice)
        ld hl, 0
        ld (PenCol), hl
        BC_TO_HL
        b_call _VPutS
        b_call _NewLine
        pop bc        
        NEXT
        
        defcode("BYE",3,128,bye)
        jp done

	;; MAKE SURE THIS IS THE LAST WORD TO BE DEFINED!
	defword("STAR", 4, 0, star)
	.dw lit, 42, emit, exit

setup_data_segment:
        ld de, here_start
        ld hl, var_here
        ld (save_sp), sp
        ld (var_sz), sp
        ld (hl), e
        inc hl
        ld (hl), d
        ld ix, return_stack_top
        ret

ok_msg: .db "ok",0
undef_msg: .db " ?",0
return_stack_top  .EQU    AppBackUpScreen+764
prog_exit: .dw 0
save_sp:   .dw 0

;; We should be able to define an interpreter in Forth that supports compiled code if we try.
;; Coming later:  A interpreter defined in Forth and meta-compiled.
prog:
;;      .dw get_str_forth
        .dw word, find, dup, zbranch, 48
        .dw lit, var_state, fetch, zbranch, 18, to_cfa, execute, space
        .dw lit, ok_msg, putstrln, branch, -34, dup, qimmed, zbranch, 6
        .dw branch, -26
        .dw to_cfa, comma, branch, -30, drop, lit, undef_msg, putstrln, branch, -66
        .dw done

here_start:     .fill 512, 0

data_start:
scratch:
                .fill 512, 0
save_latest: .dw star
save_here:   .dw scratch
data_end:

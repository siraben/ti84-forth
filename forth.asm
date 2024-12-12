.NOLIST

;;; Macros to make life easier.


;; Push BC to the return stack.
;; 10 + 19 + 10 + 19 = 58
#define PUSH_BC_RS dec ix
#defcont         \ ld (ix + 0), b
#defcont         \ dec ix
#defcont         \ ld (ix + 0), c

;; Pop the top entry of the return stack to BC.
;; 19 + 10 + 19 + 10 = 58
#define POP_BC_RS  ld c, (ix + 0)
#defcont         \ inc ix
#defcont         \ ld b, (ix + 0)
#defcont         \ inc ix

;; Push HL to the return stack.
;; 10 + 19 + 10 + 19 = 58
#define PUSH_HL_RS dec ix
#defcont         \ ld (ix + 0), h
#defcont         \ dec ix
#defcont         \ ld (ix + 0), l

;; Pop the top entry of the return stack to HL.
;; 19 + 10 + 19 + 10 = 58
#define POP_HL_RS  ld l, (ix + 0)
#defcont         \ inc ix
#defcont         \ ld h, (ix + 0)
#defcont         \ inc ix

;; Push DE to the return stack.
;; 10 + 19 + 10 + 19 = 58
#define PUSH_DE_RS dec ix
#defcont         \ ld (ix + 0), d
#defcont         \ dec ix
#defcont         \ ld (ix + 0), e

;; Pop the top entry of the return stack to DE.
;; 19 + 10 + 19 + 10 = 58
#define POP_DE_RS  ld e, (ix + 0)
#defcont         \ inc ix
#defcont         \ ld d, (ix + 0)
#defcont         \ inc ix


;; Convience macros to transfer 16-bit register contents.

#define BC_TO_HL ld h, b
#defcont       \ ld l, c

;; 4 + 4 = 8
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

;; NEXT, the basis of many Forth CODE words.
;; Defined as a jump to reduce code size.
#define NEXT jp next_sub

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

        ;; For some weird reason, either the spasm-ng assembler can't
        ;; store the string "[", so we have to fix it by changing the
        ;; value of rbrac's string.

        ld hl, name_lbrac+3
        ld (hl), 193

        ;; Same goes for S" and ."
        ld a, 34
        ld (name_s_quote+4), a
        ld (name_dot_quote+4), a

        ;; Setup data segment
        ld de, here_start
        ld hl, var_here
        ld (save_sp), sp
        ld (var_sz), sp
        ld (hl), e
        inc hl
        ld (hl), d
        ld ix, return_stack_top

        ld bc, 9999
        ld de, interpret_loop
        NEXT

docol:
        PUSH_DE_RS
        pop de
        NEXT


next_sub:               ;; Cycle count (total 38)
        ld a, (de)      ;; 7
        ld l, a         ;; 4
        inc de          ;; 6
        ld a, (de)      ;; 7
        ld h, a         ;; 4
        inc de          ;; 6
        jp (hl)         ;; 4


done:
        ;; We reach here at the end of the program.
        ld hl, 9999
        call cpHLBC
        jr nz, print_stack_error

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
        jr done_cont
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

        ;; 16-bit AND operator, yay!
        defcode("AND",3,0,and)
        pop hl

        ;; AND lowest byte first.
        ld a, c
        and l
        ld c, a

        ld a, b
        and h
        ld b, a
        NEXT

        defcode("OR",2,0,or)
        pop hl

        ld a, c
        or l
        ld c, a

        ld a, b
        or h
        ld b, a
        NEXT

        defcode("XOR",3,0,xor)
        pop hl
        ld a, c
        xor l
        ld c, a

        ld a, b
        xor h
        ld b, a
        NEXT

        defcode("<<",2, 0, left_shift)
        sla c
        rl b
        NEXT


        defcode(">>",2,0, right_shift)
        srl b
        rr c
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
        ;; 46 + 10 + 10 + 14 + 14 + 4 + 8 + 58 = 164
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

        defcode("2OVER",5,0,two_over)
        push bc
        pop bc
        pop hl
        pop bc
        pop hl
        push hl
        push bc
        dec sp
        dec sp
        dec sp
        dec sp
        push hl
        NEXT
;; TODO: 2NIP, 2TUCK, 2ROT

        defcode("1+", 2, 0, one_plus)
        inc bc
        NEXT


        defcode("1-", 2, 0, one_minus)
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
        ;; 46 + 14 = 60
        PUSH_BC_RS
        pop bc
        NEXT

        defcode("R>", 2, 0, from_r)
        push bc
        POP_BC_RS
        NEXT

        defcode("R@", 2, 0, r_fetch)
        push bc
        ld c, (ix + 0)
        ld b, (ix + 1)
        NEXT

        defcode("2>R", 3, 0, two_to_r)
        pop hl
        PUSH_HL_RS
        PUSH_BC_RS
        pop bc
        NEXT

        defcode("2R>", 3, 0, two_r_from)
        push bc
        POP_BC_RS
        POP_HL_RS
        push hl
        NEXT

        defcode("RDROP",5,0,rdrop)
        POP_HL_RS
        NEXT

        defcode("2RDROP",6,0,two_rdrop)
        POP_HL_RS
        POP_HL_RS
        NEXT

        defcode("LIT",3,0,lit)
        push bc
        ld a, (de)
        ld c, a
        inc de
        ld a, (de)
        ld b, a
        inc de
        NEXT

        defcode("LITSTR",6,0,litstring)
        ;; String length
        push bc ;; old stack top
        ex de, hl
        ld c, (hl)
        inc hl
        ld b, (hl)
        ;; BC now contains the string length
        inc hl
        push hl ;; push address of string
        ;; Skip the string.
        add hl, bc
        ;; Skip null pointer.  (Even though we have the length, because
        ;; we don't have a bcall Linux that can print out a string with
        ;; a certain length).
        inc hl
        ex de, hl
        NEXT

        defword("SQ",2,128,s_quote)
        .dw state, fetch, zbranch, 66, tick, litstring, comma, here, lit, 0
        .dw comma, getc, dup, lit, 34, neql, zbranch, 8, c_comma, branch, 65518, drop
        .dw lit, 0, c_comma, dup, here, swap, sub, lit, 3, sub, swap, store, branch, 38
        .dw here, getc, dup, lit, 34, zbranch, 12, over, store_byte, one_plus, branch, 65514
        .dw drop, here, sub, here, swap, exit


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
        jp z, fal
        cp b
        jr z, strchr_succ
        inc hl
        jr strchr_loop

strchr_succ:
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
        ld b, a
        ld c, l
        NEXT

        ;; ( n addr -- )
        defcode("+!",2,0,add_store)
        pop hl
        ld a, (bc)
        add a, l
        ld (bc), a
        inc bc
        ld a, (bc)
        adc a, h
        ld (bc), a
        pop bc
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
        pop bc
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

        cell_alloc(var_base,10)
        defcode("BASE",4,0,base)
        push bc
        ld bc, var_base
        NEXT

        ;; Floating point precision.
        cell_alloc(var_precision,12)
        defcode("PREC",4,0,precision)
        push bc
        ld bc, var_precision
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
        defcode("x",1,128,lbrac)
        ld hl, var_state
        ld (hl), 1
        inc hl
        ld (hl), 0
        NEXT

        defcode("]",1,0,rbrac)
        ld hl, var_state
        ld (hl), 0
        inc hl
        ld (hl), 0
        NEXT

        cell_alloc(var_stack_empty,1)
        defcode("?SE", 3, 0, stack_emptyq)
        push bc
        ld bc, (var_stack_empty)
        NEXT

        cell_alloc(var_here,0)
        defcode("HERE",4,0,here)
        push bc
        ld bc, var_here
        NEXT

        cell_alloc(var_num_status,0)
        defcode("NUMST", 5, 0, num_status)
        push bc
        ld bc, var_num_status
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

        defcode("WBUFP", 5, 0, __word_buffer_ptr)
        PSP_PUSH(word_buffer_ptr)
        NEXT

        defcode("WBUF", 4, 0, __word_buffer)
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

        defcode("ABS",3,0,__abs)
        PSP_PUSH(AppBackUpScreen)
        NEXT

        defword("UALT",4,0, use_alt)
        .dw lit, AppBackUpScreen, here, store, exit

        defcode("PLOTSS",6,0,__plot_s_screen)
        PSP_PUSH(plotSScreen)
        NEXT

        defcode("'",1,0,tick)
        push bc
        ex de, hl
        ld c, (hl)
        inc hl
        ld b, (hl)
        inc hl
        ex de, hl
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

        defcode("SP@", 3, 0, sp_fetch)
        push bc
        ld hl, 0
        add hl, sp
        HL_TO_BC
        NEXT

        defcode("SP!", 3, 0, sp_store)
        BC_TO_HL
        ld sp, hl
        pop bc ;; new top of stack
        NEXT

        defcode("RP@",3,0,rp_fetch)
        push bc
        push ix
        pop bc
        NEXT

        defcode("RP!",3,0,rp_store)
        push bc
        pop ix
        pop bc
        NEXT

        defcode("BRANCH", 6, 0, branch)
        ex de, hl
        ld e,(hl)
        inc hl
        ld d,(hl)
        dec hl
        add hl, de
        ex de, hl
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

        defcode("JUMP", 4, 0, jump)
        ex de, hl
        ld e, (hl)
        inc hl
        ld d, (hl)
        NEXT

        defcode("0JUMP", 5, 0, zjump)
        ld a, b
        or c
        pop bc
        jp z, jump
        inc de
        inc de
        NEXT

        defcode("0BRANCH", 7, 0, zbranch)
        ld a, b
        or c
        pop bc
        jp z, branch
        ;; The top of the stack wasn't zero.  Skip the offset and resume execution.
        inc de
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
        call cpHLBC
        jp c, fal
        jp nz, tru
        jp fal
        
        defcode("0=",2,0,zeql)
        ld hl, 0
        call cpHLBC
        jp c, fal
        jp tru


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

        defcode("EMITS", 5, 0, emit_small)
        ld a, c
        push de
        b_call _VPutMap
        pop de
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
        jr z, akey_return_space
        ld de, key_table
        ;; Add the offset
        add hl, de
        ld a, (hl)
        cp ' '
        jr z, akey_asm
        ret

akey_return_space:
        ld a, ' '
        ret

        defcode("TO_ASCII",8,0,to_ascii)
        ;; First portion is copied from key.
        push bc
        ld b, 0
        ld hl, key_table
        ;; Add the offset
        add hl, bc
        ld c, (hl)
        NEXT

key_table:
.db "     ",$00,"  " ;; 0   - 7
.db "        "       ;; 8   - 15
.db "        "       ;; 16  - 23
.db "        "       ;; 24  - 31
.db "        "       ;; 32  - 39
.db "       !"       ;; 40  - 47
.db "   =  ' "       ;; 48  - 55
.db "_@>     "       ;; 56  - 63
.db " <      "       ;; 64  - 71
.db "        "       ;; 72  - 79
.db "        "       ;; 80  - 87
.db "        "       ;; 88  - 95
.db "        "       ;; 96  - 103
.db "        "       ;; 104 - 111
.db "        "       ;; 112 - 119
.db "        "       ;; 120 - 127
.db "+-*/^()",$C1    ;; 128 - 135
.db "]  , .01"       ;; 136 - 143
.db "23456789"       ;; 144 - 151
.db "  ABCDEF"       ;; 152 - 159
.db "GHIJKLMN"       ;; 160 - 167
.db "OPQRSTUV"       ;; 168 - 175
.db "WXYZ    "       ;; 176 - 183
.db "        "       ;; 184 - 191
.db "      : "       ;; 192 - 199
.db "  ?\"    "      ;; 200 - 207
.db "        "       ;; 208 - 215
.db "        "       ;; 216 - 223
.db "        "       ;; 224 - 231
.db "    {};\\"      ;; 232 - 239
.db "        "       ;; 240 - 247
.db "        "       ;; 248 - 255

;; mul16By16 [Maths]
;;  Performs an unsigned multiplication of DE and BC.
;; Inputs:
;;  DE: Multiplier
;;  BC: Multiplicand
;; Outputs:
;;  DEHL: Product of DE and BC.

mul16By16:
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
        ret

        defcode("*",1,0,mult)
        PUSH_DE_RS
        pop de ;; get the second element from the stack
        call mul16by16
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

        ;; ( n^2 -- n )
        defcode("SQRT",4,0,sqrt)
        ;; Input: LA
        ;; Output: D
        push de
        ld a, c
        ld l, b
sqrt_la:
        ld de, 0040h	; 40h appends "01" to D
        ld h, d

        ld b, 7

        ; need to clear the carry beforehand
        or a

sqrt_loop:
        sbc hl, de
        jr nc, $+3
        add hl, de
        ccf
        rl d
        rla
        adc hl, hl
        rla
        adc hl, hl

        djnz sqrt_loop

        sbc hl, de		; optimised last iteration
        ccf
        rl d
        ld b, 0
        ld c, d
        pop de
        NEXT


        ;; defcode("FRAND",5,0,f_rand)
        ;; push bc
        ;; push de
        ;; b_call _Random
        ;; b_call _PushRealO1
        ;; pop de
        ;; pop bc
        ;; NEXT

        ;; defcode("F.",2,0,f_dot)
        ;; push bc
        ;; push de
        ;; b_call _PopRealO1
        ;; ld a, (var_precision)
        ;; b_call _FormReal
        ;; pop de
        ;; ld hl, $848e
        ;; b_call _PutS
        ;; pop bc
        ;; NEXT

        ;; ;; Read a floating point number.
        ;; ;; Parameter stack :     ( addr len -- )
        ;; ;; Floating point stack: ( -- f )
        ;; defcode("FREAD",5,0,f_read)
        ;; NEXT

        ;; defcode("F*",2,0,f_mult)
        ;; push bc
        ;; push de
        ;; b_call _PopRealO1
        ;; b_call _PopRealO2
        ;; b_call _FPMult
        ;; b_call _PushOP1
        ;; pop de
        ;; pop bc
        ;; NEXT

        ;; defcode("FSQUARE",7,0,f_square)
        ;; push bc
        ;; push de
        ;; b_call _PopRealO1
        ;; b_call _FPSquare
        ;; b_call _PushOP1
        ;; pop de
        ;; pop bc
        ;; NEXT

        ;; defcode("F=",2,0,f_eql)
        ;; push bc
        ;; push de
        ;; b_call _PopRealO1
        ;; b_call _PopRealO2
        ;; b_call _CpOP1OP2
        ;; pop de
        ;; pop bc
        ;; jp z, tru
        ;; jp fal


        ;; defcode("FDUP",4,0,f_dup)
        ;; push bc
        ;; push de
        ;; b_call _PopRealO1
        ;; b_call _PushOP1
        ;; b_call _PushOP1
        ;; pop de
        ;; pop bc
        ;; NEXT

        ;; defcode("FDROP",5,0,f_drop)
        ;; push bc
        ;; push de
        ;; b_call _PopRealO1
        ;; pop de
        ;; pop bc
        ;; NEXT

        ;; defcode("FSWAP",5,0,f_swap)
        ;; push bc
        ;; push de
        ;; b_call _PopRealO1
        ;; b_call _PopRealO2
        ;; b_call _PushRealO1
        ;; b_call _PushRealO2
        ;; pop de
        ;; pop bc
        ;; NEXT

        ;; defcode("F+",2,0,f_add)
        ;; push bc
        ;; push de
        ;; b_call _PopRealO1
        ;; b_call _PopRealO2
        ;; b_call _FPAdd
        ;; b_call _PushOP1
        ;; pop de
        ;; pop bc
        ;; NEXT

        ;; defcode("F/",2,0,f_div)
        ;; push bc
        ;; push de
        ;; b_call _PopRealO2
        ;; b_call _PopRealO1
        ;; b_call _FPDiv
        ;; b_call _PushOP1
        ;; pop de
        ;; pop bc
        ;; NEXT


        ;; defcode("FRCI",4,0,f_recip)
        ;; push bc
        ;; push de
        ;; b_call _PopRealO1
        ;; b_call _FPRecip
        ;; b_call _PushOP1
        ;; pop de
        ;; pop bc
        ;; NEXT

        ;; defcode("F-",2,0,f_sub)
        ;; push bc
        ;; push de
        ;; b_call _PopRealO2
        ;; b_call _PopRealO1
        ;; b_call _FPSub
        ;; b_call _PushOP1
        ;; pop de
        ;; pop bc
        ;; NEXT

        ;; defcode("FSQRT",5,0,f_sqrt)
        ;; push bc
        ;; push de
        ;; b_call _PopRealO1
        ;; b_call _SqRoot
        ;; b_call _PushOP1
        ;; pop de
        ;; pop bc
        ;; NEXT

        ;; ;; ( addr len -- hash_addr )
        ;; defcode("MD5",3,0,md_five)
        ;; b_call $808d  ;; md5init
        ;; pop hl
        ;; push de
        ;; push ix
        ;; b_call $8090 ;; md5update
        ;; b_call $8090 ;; md5update
        ;; b_call $8090 ;; md5update
        ;; b_call $8090 ;; md5update
        ;; b_call $8018 ;; md5final
        ;; ld bc, $8292
        ;; pop ix
        ;; pop de
        ;; NEXT

        ;; Double length number routines.
        ;; Convention for this Forth:  ( high low -- )
        ;; ( high low divisor -- remainder quotient_high quotient_low )
        defcode("D/MOD",5,0,double_divmod)
        PUSH_DE_RS
        ld (save_ix), ix
        BC_TO_DE ;; get the divisor
        pop ix ;; get the low part
        pop bc ;; get the high part
        ld a, b
        call div32By16
        ld b, a

        ;; Push remainder
        push hl
        ;; BC contains the high quotient
        push bc

        ld b, ixh
        ld c, ixl
        ;; Now it contains the low quotient

        ld ix, (save_ix)
        POP_DE_RS
        NEXT

;; From KnightOS kernel
;; div32By16 [Maths]
;;  Performs `ACIX = ACIX / DE`
;; Outputs:
;;  ACIX: ACIX / DE
;;  HL: Remainder
;;  B: 0
div32By16:
        ld hl, 0
        ld b, 32
dd_loop:
        add ix, ix
        rl c
        rla
        adc hl, hl
        jr  c, dd_overflow
        sbc hl, de
        jr  nc, dd_setBit
        add hl, de
        djnz dd_loop
        ret
dd_overflow:
        or a
        sbc hl, de
dd_setBit:
        inc ixl
        djnz dd_loop
        ret

        ;; ( n1 n2 -- high_mult low_mult )
        defcode("UM*",3,0,um_star)
        PUSH_DE_RS
        pop de ;; get the second element from the stack
        call mul16by16
        HL_TO_BC
        push de ;; push high part onto stack.
        POP_DE_RS
        NEXT

        defcode("D+",2,0,double_add)
        BC_TO_HL
        PUSH_DE_RS
        pop bc
        PUSH_BC_RS
        pop de
        add hl, de
        HL_TO_BC

        POP_DE_RS
        pop hl
        adc hl,de

        push hl
        POP_DE_RS
        NEXT


;; add16To32 [Maths]
;;  Performs `ACIX = ACIX + DE`
        defcode("M+",2,0,m_plus)
        ld (save_ix), ix
        PUSH_DE_RS
        BC_TO_DE
        pop bc
        ld a, b
add16to32:
        add ix, de
        jr nc, add16to32_done
        or a
        inc c
        jr z, add16to32_done
        add a, 1
add16to32_done:
        ld b, a
        push bc
        push ix
        pop bc
        ld ix,(save_ix)
        POP_DE_RS
        NEXT

;; mul32By8 [Maths]
;;  Performs an unsigned multiplication of DEHL and A.
;; Outputs:
;;  DEHL: product of DEHL and A
        ;; ( 32bit_high 32bit_low 8bit -- 32*8high 32*8 low )
        defcode("DS", 2, 0, double_scale)
mul32By8:
        PUSH_DE_RS
        ld a, c
        pop hl
        pop de
        push bc \ push ix
        ld ixl, 8
        push de
        push hl
        ld hl, 0
        ld d, h
        ld e, l
mul32by8_loop:
        add hl, hl
        rl e
        rl d
        rla
        jr nc, mul32by8_noAdd
        pop bc
        add hl, bc
        ex (sp), hl
        push hl
        adc hl, de
        pop de
        ex de, hl
        ex (sp), hl
        push bc
mul32by8_noAdd:
        dec ixl
        jr nz, mul32by8_loop
        pop bc
        pop bc
        pop ix \ pop bc
        push de
        HL_TO_BC
        POP_DE_RS
        NEXT

        defcode("SPACE",5,0,space)
        ld a, ' '
        b_call _PutC
        NEXT

        defcode("CR",2,0,cr)
        b_call _Newline
        NEXT

        defcode("AT-XY",5,0,at_xy)
        ld a, c
        ld (curCol), a
        pop bc
        ld a, c
        ld (curRow), a
        pop bc
        NEXT

        defcode("ATS-XY",6,0,ats_xy)
        ld a, c
        ld (PenCol), a
        pop bc
        ld a, c
        ld (PenRow), a
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
#define STRING_BUFFER_SIZE 64

string_buffer: .fill STRING_BUFFER_SIZE,0
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

        ;; Reinitialize the gets_ptr to point to the buffered input.
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
        jr nz, not_enter

        ;; Got [ENTER].  Finish up.  Maybe the user hit [ENTER]
        ;; without entering anything, we need to check for that too.

        ;; We should echo enter.
        ld a, ' '
        b_call _PutC

        xor a
        cp b
        ld (de), a
        ret nz
        inc de
        ld (de), a
        ret
not_enter:
        ;; Maybe it's the delete key?
        cp kDel
        jr z, is_del

        ;; For convenience, the left arrow key in alpha mode also
        ;; works as a DEL.

        cp kLeft
        jr nz, not_del

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
        jr z, write_space

        cp kRight ;; right arrow, alpha mode
        jr z, write_space

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
        jr z, get_char_end
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
;;  The current gets_ptr may be at a different string buffer, so we can't compare it.
unget_char:
        push hl
        push de
        ld hl, (gets_ptr)
        ;; b_call _CpHLDE
        ;; scf
        ;; jr z, unget_char_done
        dec hl
        ld (gets_ptr), hl
unget_char_done:
        pop hl
        ret

;; Skipping spaces, get the next word from the user.  Remember that
;; this needs to be flexible enough to be called from Forth as well.
;; We expect it to return a pointer to the next word following
;; gets_ptr.

;; ( -- base_addr len )
#define BUFSIZE  16
word_buffer:     .fill BUFSIZE, 0
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
        jr z, empty_word  ;; get_char_asm returned nothing, so we need to retry
                          ;; with get_str
        cp ' '
        jr z, skip_space
        cp '\n' ;; if we're reading from a converted text file.
        jr z, skip_space
        cp '\t'
        jr z, skip_space
        cp '\\'
        jr z, skip_comment
        jr actual_word
;; We really need a word.  Ask again!
empty_word:
        push hl
        ld hl, ok_msg
        b_call _PutS
        b_call _NewLine
        pop hl
        jr word_retry
skip_comment:
        ;; We could be reading from a text file.
        call get_char_asm

        ;; Ran out of input.
        or a
        jr z, empty_word

        ;; Newline found.  Then go to start.
        cp '\n'
        jr z, skip_space

        ;; Some other character.
        jr skip_comment

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
        jr z, word_done
        cp ' '
        jr z, word_done
        cp '\n'
        jr z, word_done
        cp '\t'
        jr z, word_done

        ;; A is another non-space, printable character.
        inc c
        jr actual_word_write

word_done:
        ;; Either read NUL or a space.
        xor a
        ld (hl), a
        pop de
        ld hl, word_buffer
        ld b, a  ;; c should contain the number of characters.
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
        jr nz, find_retry

find_succeed:
        ;; We found the word.  But is it hidden?
        dec de
        ;; Now we're at the length/flags pointer.
        ld a, (de)
        bit 6, a
        jr nz, find_succ_hidden
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
        or h
        jr z, find_fail
        inc hl
        inc hl
        inc hl
        ex de,hl
        pop hl
        jr find_loop
find_fail:
        pop hl
        pop de
        jp fal

;; From KnightOS kernel.
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

        defword("SIMG",4,0,save_image)
        .dw here, fetch, lit, save_here, store
        .dw latest, fetch, lit, save_latest, store
        .dw __scratch, hz, used, cmove, writeback, exit

        ;; Load the scratch buffer back into the current state.
        defword("LIMG",4,0,load_image)
        .dw lit, save_here, fetch, here, store
        .dw lit, save_latest, fetch, latest, store
        .dw hz, lit, scratch, lit, save_here, fetch, hz, sub, cmove, exit

        defword(">DFA",4,0,to_dfa)
        .dw to_cfa, lit, 3, add, exit

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
        ld hl, (var_here)
        ld de, does_brac
        
        ld (hl), e
        inc hl
        
        ld (hl), d
        inc hl
        
        ld (hl), $CD
        inc hl

        ld de, dodoes
        
        ld (hl), e
        inc hl
        
        ld (hl), d
        inc hl

        ld de, var_here
        ex de, hl
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
        xor a
        ld (currow), a
        ld (curcol), a
        b_call _ClrScrnFull
        pop de
        pop bc
        NEXT

        defcode("TOG-SCRL",8,0,toggle_scroll)
        ld a, (IY + AppFlags)
        ld l, %00000100
        xor l
        ld (IY + AppFlags), a
        NEXT

        defcode("INVTXT",6,0,inverse_text)
        ld a, (IY + TextFlags)
        ld l, %00001000
        xor l
        ld (IY + TextFlags), a
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
        .dw word, drop, fetch_byte, exit

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
        .dw lit, 2,  allot, word, create, docol_header, tick, lit, comma, comma
        .dw tick, exit, comma, exit

        defword("DO",2,128, do)
        .dw here, fetch, tick, to_r, comma, tick, to_r, comma, exit

        defword("LOOP",4,128, loop)
        .dw tick, from_r, comma, tick, from_r, comma, tick, one_plus, comma, tick, two_dup, comma
        .dw tick, eql, comma, tick, zbranch, comma, here, fetch, sub, comma, tick, two_drop, comma, exit


        defword("+LOOP",5,128, add_loop)
        .dw tick, from_r, comma, tick, from_r, comma, tick, rot, comma, tick, add, comma, tick, two_dup, comma
        .dw tick, eql, comma, tick, zbranch, comma, here, fetch, sub, comma, tick, two_drop, comma, exit

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

        defword("PARSE-NUM", 9, 0, parse_num)
        .dw dup, fetch_byte, dup, numq, zjump, parse_num_fail
        .dw zeroc, sub

parse_num_continue:
        .dw swap, one_plus, swap

parse_num_loop:
        .dw over, fetch_byte, zjump, parse_num_done
        .dw over, fetch_byte, numq, zjump, parse_num_fail
        .dw over, fetch_byte, zeroc, sub, swap, lit, 10, mult, add
        .dw jump, parse_num_continue

parse_num_done:
        .dw swap, drop
        .dw lit, 1, num_status, store, exit

parse_num_fail:
        .dw two_drop, lit, 0, num_status, store, exit

        defword("CFA>",4,0, cfa_to)
        .dw latest, fetch, qdup, zbranch, 22, two_dup, swap
        .dw less_than, zbranch, 6, nip, exit, fetch, branch, -24, drop, lit, 0, exit

        defword("PICK",4,0,pick)
        .dw one_plus, left_shift, sp_fetch, add, fetch, exit

        defword("U.",2,0,u_dot_)
        .dw base, fetch, divmod, qdup, zbranch, 4, u_dot_, dup, lit, 10, less_than
        .dw zbranch, 10, lit, 48, branch, 12, lit, 10, sub, lit, 65, add, emit, exit

        defword("UWIDTH",6,0,u_width)
        .dw base, fetch, div, qdup, zbranch, 10, u_width, one_plus, branch, 6, lit, 1, exit

        defword("SPACES",6,0,spaces)
        .dw lit, 0, to_r, to_r, space, from_r, from_r, one_plus, two_dup, eql, zbranch, 65518
        .dw two_drop, exit

        defword("U.R",3,0,u_dot_r)
        .dw swap, dup, u_width, rot, swap, sub, spaces, u_dot_, exit

        defword("U.",2,0,u_dot)
        .dw u_dot_, space, exit

        defword(".",1,0,print_tos)
        .dw u_dot, exit

        defword("DEPTH",5,0,depth)
        .dw sz, fetch, sp_fetch, sub, two_minus, right_shift, exit

        defword(".S",2,0,print_stack)
        .dw lit, '<', emit, depth, u_dot_, lit, '>', emit, space
        .dw sp_fetch, dup, sz, fetch, less_than, zbranch, 18, dup, fetch, u_dot
        .dw lit, 2, add, branch, 65510, drop, exit

        defword("HEX",3,0,hex)
        .dw lit, 16, base, store, exit

        defword("DEC",3,0,decimal)
        .dw lit, 10, base, store, exit

        ;; This word was bootstrapped from an interpreted definition.
        defword("SEE",3,0,see)
        ;; New version in progress, doesn't seem to work well.
        ;; .dw word, find, here, fetch, latest, fetch, lit, 2, pick, over, neql, zbranch, 12
        ;; .dw nip, dup, fetch, branch, 65514, drop, swap, lit, 58, emit, space, dup, id_dot
        ;; .dw space, dup, qimmed, zbranch, 19, litstring, 10
        ;; .db "IMMEDIATE "
        ;; .dw to_dfa, key, drop, two_dup, greater_than, zbranch, 293, dup, fetch, tick, lit
        ;; .dw over, eql, zbranch, 16, drop, two_plus, dup, fetch, u_dot, branch, 257, tick
        ;; .dw litstring, over, eql, zbranch, 54, drop, lit, 83, emit, two_plus, dup, fetch
        ;; .dw dup, u_dot, lit, 34, emit, space, swap
        ;; .dw two_plus, swap, two_dup, tell
        ;; .dw lit, 34, emit, space, add, one_plus, branch, 193, tick, zbranch, over, eql
        ;; .dw zbranch, 42, drop
        ;; .dw litstring, 10
        ;; .db "0BRANCH ( "
        ;; .dw two_plus, dup, fetch, u_dot, litstring, 2
        ;; .db ") "
        ;; .dw branch, 141, tick, branch, over, eql, zbranch, 41, drop, litstring, 9
        ;; .db "BRANCH ( "
        ;; .dw two_plus, dup, fetch, u_dot, litstring, 2
        ;; .db ") "
        ;; .dw branch, 90, tick, tick, over, eql, zbranch, 28, drop, lit, 39, emit, space
        ;; .dw two_plus, dup, fetch, cfa_to, id_dot, space, branch, 52, tick, exit, over
        ;; .dw eql, zbranch, 30, drop, two_dup, two_plus, neql, zbranch, 14, litstring, 5
        ;; .db "EXIT "
        ;; .dw branch, 12, dup, cfa_to, id_dot, space, drop, two_plus, branch, 65235
        ;; .dw lit, 59, emit, cr, two_drop, exit

        ;; Old version of SEE
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
        .dw latest, fetch, key, lit, 5, neql, zbranch, 32, dup, zbranch, 24, dup, qhidden, not, zbranch, 8
        .dw dup, id_dot, space, fetch, branch, -38, cr, drop, exit

        defword("CASE",4,128,case)
        .dw lit, 0, exit

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

        defcode("J",1,0,curr_loop_index2)
        push bc
        ld c, (ix + 6)
        ld b, (ix + 7)
        NEXT

zero_blk_name_buffer:
        ld hl, blk_name_buffer
        xor a
        ld (hl), a
        inc hl
        ld (hl), a
        inc hl
        ld (hl), a
        inc hl
        ld (hl), a
        inc hl
        ld (hl), a
        inc hl
        ld (hl), a
        inc hl
        ld (hl), a
        inc hl
        ld (hl), a
        inc hl
        ld (hl), a

        ret

scr_name: .db "SCRATCH",0
        ;; ( -- )
        defcode("CSCR",4,0,create_scratch)
        call zero_blk_name_buffer
        push bc
        ld bc, scr_name
        push bc
        ld bc, 7

        ;; First make a variable name in OP1.
        pop hl
        push de
        ld de, blk_name_buffer
        ;; Indicate that this variable is a program.
        ld a, ProgObj
        ld (de), a
        inc de
        ;; Copy the 8-character name.
        ldir

        push ix
        ld hl, blk_name_buffer
        b_call _Mov9ToOP1
        ;; Allocate 255 bytes (default block size, change later if needed)
        ld hl, 1024
        b_call _CreateProg
        pop ix
        ;; DE contains the start of the memory location.
        ld b, d
        ld c, e
        ex de, hl
        pop de

        ld (hl), 0
        inc hl
        ld (hl), 4
        inc bc
        inc bc
        NEXT


blk_name_buffer: .fill 9, 0
        ;; ( name_string name_len -- block_start )
        defcode("CBLK",4,0,create_block)
        call zero_blk_name_buffer
        ;; First make a variable name in OP1.
        pop hl
        push de
        ld de, blk_name_buffer
        ;; Indicate that this variable is a program.
        ld a, ProgObj
        ld (de), a
        inc de
        ;; Copy the 8-character name.
        ldir

        push ix
        ld hl, blk_name_buffer
        b_call _Mov9ToOP1
        ;; Allocate 255 bytes (default block size, change later if needed)
        ld hl, 255
        b_call _CreateProg
        pop ix
        ;; DE contains the start of the memory location.
        ld b, d
        ld c, e
        ex de, hl
        pop de

        ld (hl), 255
        inc hl
        ld (hl), 0
        inc bc
        inc bc
        NEXT

        ;; ( name_string name_len -- data_start )
        ;; Return 0 if not found.
        defcode("FBLK",4,0,find_block)
        call zero_blk_name_buffer
        pop hl
        push de
        ld de, blk_name_buffer ;; skip the tag byte.
        ld a, ProgObj
        ld (de), a
        inc de
        ldir

        push ix
        ld hl, blk_name_buffer
        b_call _Mov9ToOP1
        b_call _ChkFindSym
        pop ix
        jp c, fblk_fail
        ld b, d
        ld c, e
        pop de
        inc bc
        inc bc
        NEXT
fblk_fail:
        pop de
        jp fal

        ;; Switch the input stream to the pointer on the stack.
        ;; ( prog_start_ptr --  )
        ;; When WORD runs out of input it calls GETS, which resets gets_ptr
        defcode("RUN",3,0,run)
        BC_TO_HL
        ld (gets_ptr), hl
        pop bc
        ld de, interp
        NEXT


load_not_found_msg1: .db "File ",0
load_not_found_msg2: .db " not found. ",0
load_long_name_msg: .db "Name must be 8 characters or shorter. ", 0
        defword("LOAD",4,0,load_file)
        .dw word, dup, lit, 8, greater_than, zbranch, 12
        .dw lit, load_long_name_msg, putstrln, two_drop, exit
        .dw find_block, dup, zbranch, 8, run, branch, 22
        .dw lit, load_not_found_msg1, putstr, lit, word_buffer, putstr
        .dw lit, load_not_found_msg2, putstrln, drop, exit


        defcode("REFILL", 6, 0, refill)
        call get_str
        NEXT

        defword("QUIT", 4, 0, quit)
        .dw sz, fetch, sp_store, refill, jump, interpret_loop


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


        defcode("PLOT",4,0,plot)
        push bc
        push de
        b_call _GrBufCpy
        pop de
        pop bc
        NEXT

;; Creating an editor.  Rough idea: We want to have a block-editing
;; system to be able to save and read programs.  We use the small
;; variable width font instead of the large one, so that we may place
;; it on the screen.

        ;; ( address_to_write_to -- )
        defword("WR", 2, 0, write)
        ;; The second get_str_forth is necessary as we don't want the
        ;; interpreter to read the entered text
        .dw cr, get_str_forth, cr, __string_buffer, swap, __string_buffer_size, cmove, get_str_forth, exit

        defcode("TELLS", 5, 0, tell_small)
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

ok_msg: .db " ok",0
undef_msg: .db " ?",0
prog_exit: .dw 0
save_sp:   .dw 0
save_ix:   .dw 0

return_stack_top  .EQU    $91DC + 294

        ;; 
interp:
        .dw get_str_forth, interpret, done
        
        ;; The interpreter
        defword("INTERPRET", 9, 0, interpret)
interpret_loop:
        .dw word, find, qdup, zjump, maybe_num
        .dw lit, var_state, fetch, not, zjump, interpret_word

compiling_word:
        .dw dup, qimmed, zjump, compile_word

interpret_word:
        .dw to_cfa, execute, jump, interpret_loop

compile_word:
        .dw to_cfa, comma, jump, interpret_loop

maybe_num:
        .dw lit, word_buffer, parse_num
        .dw num_status, fetch, zjump, undef
        .dw lit, var_state, fetch, not, zjump, interpret_loop

compile_num:
        .dw lit, lit, comma, comma, jump, interpret_loop
undef:
        ;; Go to interpret mode and clear the stack.
        .dw lbrac, sz, fetch, sp_store
        .dw lit, undef_msg, putstrln, jump, interpret_loop

        .dw done


data_start:
here_start:
scratch:
             .fill 350, 0
save_latest: .dw star
save_here:   .dw scratch
data_end:

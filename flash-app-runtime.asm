;; TI-84 Forth Flash App lifecycle and persistence support.

persist_size       .EQU persist_header + 0
persist_magic      .EQU persist_header + 2
persist_format     .EQU persist_header + 6
persist_generation .EQU persist_header + 8
persist_used       .EQU persist_header + 10
persist_latest     .EQU persist_header + 12
persist_crc        .EQU persist_header + 14
persist_commit     .EQU persist_header + 16
persist_slot_temp  .EQU persist_header + 18
persist_crc_temp   .EQU persist_header + 20
persist_used_temp  .EQU persist_header + 22
APP_ARCHIVED_SLOT_DATA_OFFSET .EQU 17

;; Check whether HL more bytes fit at HERE.  Carry means failure.
;; BC and DE are preserved.
app_require_room:
        push bc
        push de
        ld de, (var_here)
        add hl, de
        jr c, app_require_room_fail
        ld de, (var_arena_end)
        or a
        sbc hl, de
        jr z, app_require_room_ok
        jr c, app_require_room_ok
app_require_room_fail:
        pop de
        pop bc
        scf
        ret
app_require_room_ok:
        pop de
        pop bc
        or a
        ret

;; Write one of the two fixed AppVar names directly into OP1.  This avoids
;; passing a Flash pointer to an OS routine after bank A has been remapped.
;; Input: A = 0 for FTHSAVA, nonzero for FTHSAVB.
app_set_slot_name:
        push af
        ld a, AppVarObj
        ld (OP1), a
        ld a, 'F'
        ld (OP1 + 1), a
        ld a, 'T'
        ld (OP1 + 2), a
        ld a, 'H'
        ld (OP1 + 3), a
        ld a, 'S'
        ld (OP1 + 4), a
        ld a, 'A'
        ld (OP1 + 5), a
        ld a, 'V'
        ld (OP1 + 6), a
        pop af
        or a
        ld a, 'A'
        jr z, app_set_slot_suffix
        ld a, 'B'
app_set_slot_suffix:
        ld (OP1 + 7), a
        xor a
        ld (OP1 + 8), a
        ret

;; Resolve a persistence slot to its AppVar size word.
;;
;; _ChkFindSym returns a RAM AppVar's size-word address directly.  For an
;; archived named variable it instead returns the archive record's status
;; byte.  The fixed seven-byte FTHSAVx name puts the AppVar size word 17 bytes
;; later: one status byte, a two-byte record length, six symbol-metadata bytes,
;; and the eight-byte length/name field.  Output: A = source page (zero for
;; RAM), HL = size word; carry means the slot is missing.  Archived results are
;; normalized across the 8000h window boundary.
app_find_slot_data:
        call app_set_slot_name
        b_call _ChkFindSym
        ret c
        ld a, b
        ex de, hl
        or a
        ret z
        ld de, APP_ARCHIVED_SLOT_DATA_OFFSET
        add hl, de
        jp app_normalize_flash_source

;; Normalize A:HL after adding to an archived source pointer.
app_normalize_flash_source:
        or a
        ret z
        bit 7, h
        ret z
        ld de, $4000
        or a
        sbc hl, de
        inc a
        jr app_normalize_flash_source

;; Copy BC bytes from A:HL to DE.  Page zero denotes an ordinary RAM source;
;; nonzero pages use TI-OS's cross-page Flash copier.
app_copy_source_to_ram:
        or a
        jr nz, app_copy_source_from_flash
        ldir
        ret
app_copy_source_from_flash:
        b_call _FlashToRam
        ret

;; Copy and structurally validate a persistence header.
;; Input: A = slot. Output: HL = generation, carry on failure.
app_probe_slot:
        push af
        call app_find_slot_data
        jr c, app_probe_slot_missing
        ld de, persist_header
        ld bc, APP_PERSIST_HEADER_SIZE + 2
        call app_copy_source_to_ram

        ld hl, (persist_size)
        ld de, APP_PERSIST_HEADER_SIZE
        or a
        sbc hl, de
        jr c, app_probe_slot_invalid
        ld hl, (persist_magic)
        ld de, $4954             ;; "TI"
        or a
        sbc hl, de
        jr nz, app_probe_slot_invalid
        ld hl, (persist_magic + 2)
        ld de, $3446             ;; "F4"
        or a
        sbc hl, de
        jr nz, app_probe_slot_invalid
        ld hl, (persist_format)
        ld de, APP_PERSIST_FORMAT
        or a
        sbc hl, de
        jr nz, app_probe_slot_invalid
        ld hl, (persist_commit)
        ld de, APP_PERSIST_COMMIT
        or a
        sbc hl, de
        jr nz, app_probe_slot_invalid

        ld hl, (persist_used)
        ld de, (var_arena_size)
        or a
        sbc hl, de
        jr c, app_probe_slot_size_ok
        jr nz, app_probe_slot_invalid
app_probe_slot_size_ok:
        ld hl, (persist_used)
        ld de, APP_PERSIST_HEADER_SIZE
        add hl, de
        ld de, (persist_size)
        or a
        sbc hl, de
        jr nz, app_probe_slot_invalid
        ld hl, (persist_generation)
        pop af
        or a
        ret
app_probe_slot_missing:
        pop af
        scf
        ret
app_probe_slot_invalid:
        pop af
        scf
        ret

;; Load and checksum one slot.  Input: A = slot. Carry means invalid.
app_try_load_slot:
        ld (persist_slot_temp), a
        call app_probe_slot
        ret c
        ld a, (persist_slot_temp)
        call app_find_slot_data
        ret c
        ld de, APP_PERSIST_HEADER_SIZE + 2
        add hl, de
        call app_normalize_flash_source
app_load_source_ready:
        ld de, here_start
        ld bc, (persist_used)
        push af
        ld a, b
        or c
        jr z, app_load_copy_empty
        pop af
        call app_copy_source_to_ram
        jr app_load_copy_done
app_load_copy_empty:
        pop af
app_load_copy_done:

        ld hl, here_start
        ld bc, (persist_used)
        call app_crc16_ccitt
        ld hl, (persist_crc)
        or a
        sbc hl, de
        jr nz, app_try_load_invalid

        ld hl, here_start
        ld de, (persist_used)
        add hl, de
        ld (var_here), hl
        ld de, (persist_latest)
        ld hl, name_star
        or a
        sbc hl, de
        jr z, app_try_load_latest_ok
        ex de, hl
        ld de, here_start
        or a
        sbc hl, de
        jr c, app_try_load_invalid
        add hl, de
        ld de, (var_here)
        or a
        sbc hl, de
        jr nc, app_try_load_invalid
app_try_load_latest_ok:
        ld hl, (persist_latest)
        ld (var_latest), hl
        ld hl, (persist_generation)
        ld (var_generation), hl
        ld a, (persist_slot_temp)
        ld (var_active_slot), a
        xor a
        ld (var_definition_open), a
        or a
        ret
app_try_load_invalid:
        scf
        ret

app_load_dictionary:
        xor a
        ld (var_valid_slots), a
        call app_probe_slot
        jr c, app_load_probe_b
        ld (var_gen_a), hl
        ld a, 1
        ld (var_valid_slots), a
app_load_probe_b:
        ld a, 1
        call app_probe_slot
        jr c, app_load_select
        ld (var_gen_b), hl
        ld a, (var_valid_slots)
        or 2
        ld (var_valid_slots), a
app_load_select:
        ld a, (var_valid_slots)
        or a
        jr z, app_cold_dictionary
        cp 1
        jr z, app_load_a
        cp 2
        jr z, app_load_b
        ld hl, (var_gen_b)
        ld de, (var_gen_a)
        or a
        sbc hl, de
        bit 7, h
        jr nz, app_load_a_then_b
app_load_b_then_a:
        ld a, 1
        call app_try_load_slot
        ret nc
app_load_a:
        xor a
        call app_try_load_slot
        ret nc
        jr app_cold_dictionary
app_load_a_then_b:
        xor a
        call app_try_load_slot
        ret nc
app_load_b:
        ld a, 1
        call app_try_load_slot
        ret nc
app_cold_dictionary:
        ld hl, name_star
        ld (var_latest), hl
        ld hl, here_start
        ld (var_here), hl
        xor a
        ld (var_generation), a
        ld (var_generation + 1), a
        ld (var_definition_open), a
        ld a, $FF
        ld (var_active_slot), a
        ret

;; Persist the live dictionary to the inactive slot, then archive it.  The
;; active archived slot is not touched until the replacement is complete.
app_save_dictionary_copy:
        call app_rollback_definition
        ld hl, (var_here)
        ld de, here_start
        or a
        sbc hl, de
        jp c, app_save_failed
        ld de, (var_arena_size)
        push hl
        or a
        sbc hl, de
        pop hl
        jr c, app_save_used_ok
        jp nz, app_save_failed
app_save_used_ok:
        ld (persist_used_temp), hl
        ld b, h
        ld c, l
        ld hl, here_start
        call app_crc16_ccitt
        ld (persist_crc_temp), de

        ld a, (var_active_slot)
        cp $FF
        jr z, app_save_slot_a
        xor 1
        jr app_save_slot_ready
app_save_slot_a:
        xor a
app_save_slot_ready:
        ld (persist_slot_temp), a
        call app_set_slot_name
        b_call _ChkFindSym
        jr c, app_save_create
        b_call _DelVarArc
app_save_create:
        ld hl, (persist_used_temp)
        ld de, APP_PERSIST_HEADER_SIZE + 32
        add hl, de
        push hl
        b_call _EnoughMem
        pop hl
        jp c, app_save_failed
        ld de, 32
        or a
        sbc hl, de
        ld a, (persist_slot_temp)
        call app_set_slot_name
        b_call _CreateAppVar
        inc de
        inc de
        ld a, 'T'
        ld (de), a
        inc de
        ld a, 'I'
        ld (de), a
        inc de
        ld a, 'F'
        ld (de), a
        inc de
        ld a, '4'
        ld (de), a
        inc de
        ld hl, APP_PERSIST_FORMAT
        ld a, l
        ld (de), a
        inc de
        ld a, h
        ld (de), a
        inc de
        ld hl, (var_generation)
        inc hl
        ld a, l
        ld (de), a
        inc de
        ld a, h
        ld (de), a
        inc de
        push hl
        ld hl, (persist_used_temp)
        ld a, l
        ld (de), a
        inc de
        ld a, h
        ld (de), a
        inc de
        ld hl, (var_latest)
        ld a, l
        ld (de), a
        inc de
        ld a, h
        ld (de), a
        inc de
        ld hl, (persist_crc_temp)
        ld a, l
        ld (de), a
        inc de
        ld a, h
        ld (de), a
        inc de
        ld hl, APP_PERSIST_COMMIT
        ld a, l
        ld (de), a
        inc de
        ld a, h
        ld (de), a
        inc de
        ld bc, (persist_used_temp)
        ld a, b
        or c
        jr z, app_save_copy_done
        ld hl, here_start
        ldir
app_save_copy_done:
        pop hl
        ld (var_generation), hl

        ld a, (persist_slot_temp)
        call app_set_slot_name
        b_call _ChkFindSym
        jr c, app_save_failed
        ld a, b
        or a
        jr nz, app_save_committed
        b_call _Arc_Unarc
app_save_committed:
        ld a, (persist_slot_temp)
        ld (var_active_slot), a
        or a
        ret
app_save_failed:
        ld hl, app_save_error_msg
        call flash_puts
        scf
        ret

app_save_dictionary:
        ld a, (var_arena_live)
        or a
        ret z
        jp app_save_dictionary_copy

app_release_arena:
        ld a, (var_arena_live)
        or a
        ret z
        ld hl, (var_arena_size)
        ld de, APP_WORKSPACE_SIZE
        add hl, de
        ex de, hl
        xor a
        ld (var_arena_live), a
        ld hl, userMem
        b_call _DelMem
        ret

dictionary_full:
        call app_rollback_definition
        ld hl, app_dictionary_full_msg
        call flash_puts
        jr app_reset_terminal
dictionary_corrupt:
        ld sp, (save_sp)
        call app_cold_dictionary
        ld hl, app_dictionary_corrupt_msg
        call flash_puts
app_reset_terminal:
        ld sp, (save_sp)
        ld ix, return_stack_top
        ld hl, 1
        ld (var_state), hl
        ld (var_stack_empty), hl
        ld (var_sz), sp
        ld bc, 9999
        ld de, interpret_loop
        NEXT

;; Discard a definition that failed before its final delimiter.  The marker is
;; set before any header or body bytes are emitted, so both HERE and LATEST can
;; be restored without walking a potentially incomplete record.
app_rollback_definition:
        ld a, (var_definition_open)
        or a
        ret z
        xor a
        ld (var_definition_open), a
        ld hl, (var_definition_start)
        ld (var_here), hl
        ld hl, (var_definition_prev)
        ld (var_latest), hl
        ret

app_system_error:
        ld sp, (save_sp)
        call app_release_arena
        ld hl, app_system_error_msg
        call flash_puts
        b_call _GetKey
        jp app_restore_and_exit

app_putaway:
        ld sp, (save_sp)
        call app_save_dictionary
        call app_release_arena
        b_call _ReloadAppEntryVecs
        bjump(_PutAway)

app_restore_and_exit:
        b_call _ReloadAppEntryVecs
        set appAutoScroll, (iy + appFlags)
        ld (iy + textFlags), 0
        b_call _ClrLCDFull
        b_call _HomeUp
        bit monAbandon, (iy + monFlags)
        jr nz, app_exit_putaway
        bjump(_JForceCmdNoChar)
app_exit_putaway:
        bjump(_PutAway)

app_vectors:
        .dw app_dummy_vector
        .dw app_dummy_vector
        .dw app_putaway
        .dw app_dummy_vector
        .dw app_system_error
        .dw app_dummy_vector
        .db appTextSaveF
app_dummy_vector:
        ret

app_memory_error_msg:      .db "Need more free RAM.",0
app_save_error_msg:        .db "Snapshot not saved.",0
app_dictionary_full_msg:   .db " Dictionary full.",0
app_dictionary_corrupt_msg:.db " Dictionary reset.",0
app_system_error_msg:      .db "TI-OS error; state not saved.",0

;; CRC16-CCITT (poly 1021h, initial value 1D0Fh).  Adapted from RPN83P's
;; MIT-licensed crc1.asm, copyright 2023 Brian T. Park.
;; Input: HL=data, BC=length. Output: DE=CRC. Destroys A, BC, DE, HL.
app_crc16_ccitt:
        ex de, hl
        ld hl, $1D0F
        jr app_crc_char_next
app_crc_char_loop:
        ld a, (de)
        inc de
        push bc
        ld b, 8
        xor h
        ld h, a
app_crc_bit_loop:
        add hl, hl
        jr nc, app_crc_bit_next
        ld a, l
        xor $21
        ld l, a
        ld a, h
        xor $10
        ld h, a
app_crc_bit_next:
        djnz app_crc_bit_loop
        pop bc
        dec bc
app_crc_char_next:
        ld a, b
        or c
        jr nz, app_crc_char_loop
        ex de, hl
        ret

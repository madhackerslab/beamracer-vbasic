;*****************************************************************************
;* VBASIC
;* Laubzega/MHL in 2019-2020
;*
;* Compatible with ca65 assembler (part of https://github.com/cc65).
;*****************************************************************************
        .export __HIGHMEM_SIZE

        .include "../../silverdr/64er/include/c64.i"
        .include "../../silverdr/64er/include/sd_macros.i"
        .include "vlib/vasyl.s"

COPIER_COUNT = 3    ; two hardware and one software (writing to main memory)
MAIN_RAM_COPIER = COPIER_COUNT - 1 ; the last one
src = FREEZP
dst = src + 2
JMPER = $54
KEYWORD_PTRS_ORIG = $a09e
KEYWORD_BYTES = $a19b - KEYWORD_PTRS_ORIG

token_id .set 0

.macro keyword Arg
        .repeat .strlen(Arg) - 1, I
        .byte   .strat(Arg, I)
        .endrep
        .byte   .strat(Arg, .strlen(Arg)-1) | $80
token_id .set token_id + 1
.endmacro

.macro patch_jmp Arg1, Arg2
        lda #$4c
        sta Arg1
        lda #<Arg2
        sta Arg1 + 1
        lda #>Arg2
        sta Arg1 + 2
.endmacro

        ; copy our resident section to HIGHMEM
        lda #<highmem_data
        sta src
        lda #>highmem_data
        sta src + 1
        lda #<highmem_data_start
        sta dst
        lda #>highmem_data_start
        sta dst + 1
        ldx #>(highmem_data_end - highmem_data_start) + 1
        jsr copy_pages

        ; copy BASIC ROM to RAM
        lda #<BASIC_BASE
        sta src
        sta dst
        lda #>BASIC_BASE
        sta src + 1
        sta dst + 1
        ldx #32
        jsr copy_pages

        c64_turn_basic_off

        lda #<KEYWORD_PTRS_ORIG
        sta src
        lda #>KEYWORD_PTRS_ORIG
        sta src + 1
        lda #<keyword_ptrs
        sta dst
        sta $A5BD
        sta $A600
        sta $A731
        sta $A739
        tax
        dex
        stx $A5FB
        lda #>keyword_ptrs
        sta dst + 1
        sta $A5BE
        sta $A601
        sta $A732
        sta $A73A
        sta $A5FC

        patch_jmp $a72a, uncrunch_tokens_patch
        patch_jmp $a5b6, crunch_tokens_patch
        patch_jmp $a5f9, no_carry2
        patch_jmp $a7f3, interpret_patch
        patch_jmp $afaa, function_patch
        patch_jmp $adb8, evaluate_expression_patch

        lda #<custom_convert_fac1_to_string
        sta $aabd
        lda #>custom_convert_fac1_to_string
        sta $aabe

        lda #<run_patch
        sta $a87b
        lda #>run_patch
        sta $a87c

        lda #<print_hook
        sta $ab1f
        lda #>print_hook
        sta $ab20

        lda $302
        sta warm_start_continuation + 1
        lda $303
        sta warm_start_continuation + 2

        ; patch error handler vector to reinitialize us after R/S-R
        lda #<warmstart_handler
        sta $302
        lda #>warmstart_handler
        sta $303

        ; we also need to evaluate $xxxx numbers
        lda #<evaluate_expression
        sta $030a
        lda #>evaluate_expression
        sta $030b

        ldx #<highmem_data_start     ; move MEMSIZ low to hide us above it
        ldy #>highmem_data_start
        clc
        jsr $ff99

        jsr prepare_register_names
        jsr knock_knock
        lda #$e3
        pha
        lda #$96
        pha
        jmp $a644       ; perform NEW and then near-COLD START

prepare_register_names:
        lda #<register_names
        sta src
        lda #>register_names
        sta src+1

        ldx #0
next_register:
        lda src
        sta register_table_lo,x
        lda src+1
        sta register_table_hi,x
        inx

loop_reg_name:
        ldy #0
        lda (src),y
        inc src
        bne @no_carry
        inc src+1
@no_carry:
        tay
        beq end_of_registers
        bpl loop_reg_name
        bmi next_register
end_of_registers:
        rts

copy_pages:
        ldy #0
copy_bytes:
        lda (src),y
        sta (dst),y
        dey
        bne copy_bytes
        inc src + 1
        inc dst + 1
        dex
        bne copy_bytes
        rts

missing_string:
        ;.byte "missing",0
        .byte "none ",0

highmem_data:
        .segment "HIGHCODE"
highmem_data_start:
warmstart_handler:
        lda $01
        and #$01
        beq skip_reinit ; BASIC ROM is already off
        jsr unconfig_copiers
        c64_turn_basic_off
        lda racer_mode
        cmp #2
        beq racer_after_rsr
        lda #0
        sta racer_mode
racer_after_rsr:
        jsr knock_knock
skip_reinit:

warm_start_continuation:
        bit $ffff       ; on first pass (after RUN) return normally
        lda #$4c        ; JMP opcode
        sta warm_start_continuation

        ; get original BASIC warm start vector
        lda $e449
        sta JMPER+1
        lda $e44a
        sta JMPER+2

        jmp (JMPER+1)

unconfig_copiers:
        ldx #3
        lda #$80        ; no "current" copier
clear_copier_state:     ; all copiers unconfigured
        sta current_copier,x
        dex
        bpl clear_copier_state
        rts

; attempt activation of the Beam Racer
knock_knock:
        lda racer_mode
        beq @end
        ldx #255
        cpx VREG_CONTROL
        bne @knocked
        lda #$42
        sta VREG_CONTROL
        lda #$52
        sta VREG_CONTROL
        cpx VREG_CONTROL
        bne @knocked

        lda #$ff
        sta racer_mode
        ldy #0
@loop:
        lda missing_string,y
        beq @end
        sta state_string,y
        iny
        bne @loop
        rts
@knocked:
        lda VREG_DLSTROBE
        lsr
        lsr
        lsr
        cmp #1
        beq @end

        sec
        sbc #1
        clc
        adc #'0'
        sta state_string+3
@end:
        rts

basic_string:
        .byte $93,$0d, " *** vbasic v1.0 by laubzega/mhl'20 ***", $0d, $0d
        .byte " "
beamracer_string:
        .byte "beamracer "
state_string:
        .byte "v1.0"
beamracer_string_end:
        .byte "  ",0

print_hook:
        cpy #$e4
        bne @not_interesting
        cmp #$73
        bne @not_interesting
        lda #<basic_string
        ldy #>basic_string
@not_interesting:
        jmp $b487   ; do the print


; this can potentially be patched in-place,
; jmp $a6ef would need to become a branch
uncrunch_tokens_patch:
        lda keyword_adr_lo - 1,x
        sta src
        lda keyword_adr_hi - 1,x
        sta src + 1
        ldy #0
next_char:
        lda (src),y
        bpl not_last
        jmp $A6EF
not_last:
        jsr $AB47
        iny
        bne next_char   ; branch always
        ; no need to handle the contingency
        ; of a token expanding to 256 chars
        ;jmp $E386      ; WARM START


crunch_tokens_patch:
        lda #<keyword_ptrs
        sta src
        lda #>keyword_ptrs
        sta src + 1
        ldy #0
        beq no_carry
compare_string:
        iny
        bne no_carry
        inc src + 1
no_carry:
        inx
compare_next:
        lda $0200,x
        sec
        sbc (src),y
        beq compare_string
        jmp $A5C1

crunch_tokens_patch2:
        iny
        bne no_carry2
        inc src + 1
no_carry2:
        lda (src),y
        bpl crunch_tokens_patch2
        iny
        bne no_carry3
        inc src + 1
no_carry3:
        lda (src),y
        bne compare_next
        jmp $A604


interpret_patch:
        cmp #$CC-$80
        bcs our_token
        cmp #$23
        bcs function_token
        jmp $A7F7
function_token:
        jmp $A80E

our_token:
        cmp #last_npf_token-$80    ; is it a function's token?
        bcc not_pure_function
        jmp $af08                  ; SYNTAX ERROR
not_pure_function:
        sec
        sbc #$CC-$80
        asl
        tay
        lda current_copier
        sta copier_saver
        lda function_flag
        pha
        lda #>(cmd_return-1)
        pha
        lda #<(cmd_return-1)
        pha
        lda vb_functions + 1,y
        pha
        lda vb_functions,y
        pha
        lda #0
        sta function_flag
        jmp $A801

cmd_return:
        pla
        sta function_flag
        lda copier_saver
        sta current_copier
        rts

function_patch:
        jsr $0073
        cpx #(npf_tokens_start-$80)*2
        bcs our_function
        jmp $afad       ; continue processing of ROM functions
our_function:
        pla
        txa
        sec
        sbc #($CC-$80)*2
        tay
        lda vb_functions,y
        sta $55
        lda vb_functions + 1,y
        sta $56
        lda function_flag
        pha
        lda #$80
        sta function_flag
        jsr $0054
        pla
        sta function_flag
        jmp $ad8d


get_signed_byte:
        jsr FRMNUM  ; $ad8a - evaluate expression and check is numeric, else do type mismatch
        jsr $b7fb   ; check if 16-bit value

        ; check if in <-255,255> range
        ; A holds HI, Y holds LO
        tax
        beq @ok
        inx
        bne illegal_quantity_error
        tya
        beq illegal_quantity_error
@ok:
        tya
        tax
        rts
        

; *****************************************************************************
; *** VCFG keyword implementation                                           ***
; *****************************************************************************
; current_copier and copier_state have to be next to each other
; as they are reset by a single loop
current_copier:
        .byte $80
copier_state:
        .byte $80, $80, $80
current_bank:
        .byte $00
copier_banks:
        .byte $00, $00, $00

VCFG_IMPL:
        jsr GETBYT ; $b79e - get byte parameter ; Parse A into uint8_t in X
        cpx #COPIER_COUNT
        bcc copier_ok
illegal_quantity_error:
        jmp $b248   ; "Illegal quantity"
copier_ok:
        stx dst

        jsr $aefd   ; scan for ",", else do syntax error then warm start
        jsr FRMNUM  ; $ad8a - evaluate expression and check is numeric, else do type mismatch
        jsr GETADR  ; $b7f7 - convert FAC_1 to uint16_t in LINNUM

        jsr $0079
        cmp #','
        beq @step_specified
        lda #1      ; default step is 1.
        bne step_ok
@step_specified:
        jsr $0073
        jsr FRMNUM  ; $ad8a - evaluate expression and check is numeric, else do type mismatch
        jsr AYINT   ; $b1bf - Convert a Floating Point Number to a Signed Integer in $64/$65 BE (sic!)
        lda $65
        ldx $64
        beq hi_byte_is_zero
        cpx #255    ; make sure we're in negative <-256,-1>
        bne illegal_quantity_error
        cmp #128
        bcc illegal_quantity_error ; step < -128
        bcs step_ok
hi_byte_is_zero:
        cmp #128                   ; step > 127
        bcs illegal_quantity_error ; step too large

step_ok:
        pha         ; preserve STEP value
        ldx current_bank
        jsr $0079
        cmp #','
        bne no_bank_specified
        jsr $0073
        jsr GETBYT ; $b79e - get byte parameter ; Parse A into uint8_t in X
        cpx #MEMBANK_COUNT
        bcs illegal_quantity_error
.if CONTROL_RAMBANK_BIT <> 0
        txa
        .repeat CONTROL_RAMBANK_BIT
        asl
        .endrep
        tax
.endif
no_bank_specified:
        stx selected_bank

        ldx dst
;        stx current_copier  ; set copier as current
        stx copier_saver    ; set copier as default
        lda #0              ; mark copier as configured
        sta copier_state,x

        lda selected_bank   ; set the bank to use by the copier
        sta copier_banks,x

        lda copier_addresses_lo,x
        sta src
        lda copier_addresses_hi,x
        sta src + 1

        ldy #0
        lda LINNUM   ; $14
        sta (src),y
        iny
        lda LINNUMHI ; $15
        sta (src),y
        pla          ; pull previously stored STEP value back
        iny
        sta (src),y  ; store into copier_step
        rts

copier_addresses_lo:
        .lobytes VREG_ADR0, VREG_ADR1, copier_adr
copier_addresses_hi:
        .hibytes VREG_ADR0, VREG_ADR1, copier_adr

preserve_copier_0:
        ldy #2
@copy_copier:
        lda VREG_ADR0,y
        sta copier_backup,y
        dey
        bpl @copy_copier
        rts

restore_copier_0:
        ldy #2
@copy_copier:
        lda copier_backup,y
        sta VREG_ADR0,y
        dey
        bpl @copy_copier
        rts

turn_on_read_mode:
        lda VREG_CONTROL   ; enable reading from copiers
        sta current_mode
        ora #1 << CONTROL_PORT_READ_ENABLE_BIT
        sta VREG_CONTROL
        rts

restore_mode:
        lda current_mode
        sta VREG_CONTROL
        rts

; *****************************************************************************
; *** VPOKE keyword implementation                                          ***
; *****************************************************************************
VPOKE_IMPL:
        jsr $B7EB   ; get 16- and 8-bit parameters

        jsr preserve_copier_0
        jsr set_bank
        lda $14
        sta VREG_ADR0
        lda $15
        sta VREG_ADR0 + 1
        stx VREG_PORT0
        jmp restore_copier_0

; *****************************************************************************
; *** VDPOKE keyword implementation                                         ***
; *****************************************************************************
VDPOKE_IMPL:
        jsr $ad8a   ; evaluate expression and check is numeric, else do
                    ; type mismatch
        jsr $b7f7   ; convert FAC_1 to integer in temporary integer
        lda $14
        sta src
        lda $15
        sta src + 1
        jsr $aefd   ; scan for ",", else do syntax error then warm start
        jsr $ad8a   ; evaluate expression and check is numeric, else do
                    ; type mismatch
        jsr $b7f7   ; convert FAC_1 to integer in temporary integer

        jsr preserve_copier_0
        jsr set_bank
        lda src
        sta VREG_ADR0
        lda src + 1
        sta VREG_ADR0 + 1
        lda #1
        sta VREG_STEP0
        lda $14
        sei         ; disable IRQs so that we don't get interfered between bytes
        sta VREG_PORT0
        lda $15
        sta VREG_PORT0
        cli
        jmp restore_copier_0

; *****************************************************************************
; *** DPOKE keyword implementation                                          ***
; *****************************************************************************
DPOKE_IMPL:
        jsr $ad8a   ; evaluate expression and check is numeric, else do
                    ; type mismatch
        jsr $b7f7   ; convert FAC_1 to integer in temporary integer
        lda $14
        sta src
        lda $15
        sta src + 1
        jsr $aefd   ; scan for ",", else do syntax error then warm start
        jsr $ad8a   ; evaluate expression and check is numeric, else do
                    ; type mismatch
        jsr $b7f7   ; convert FAC_1 to integer in temporary integer

        ldy #0
        lda $14
        sei
        sta (src),y
        iny
        lda $15
        sta (src),y
        cli
        rts

; *****************************************************************************
; *** DPEEK keyword implementation                                          ***
; *****************************************************************************
DPEEK_IMPL:
        jsr one_argument
        lda firstarg_lo
        sta src
        lda firstarg_hi
        sta src+1
        ldy #0
        sei         ; ensure atomicity
        lda (src),y
        pha
        iny
        lda (src),y
        cli
        tay
        pla
        jmp return_uint16

; *****************************************************************************
; *** VPEEK keyword implementation                                          ***
; *****************************************************************************
VPEEK_IMPL:
        jsr one_argument

        jsr preserve_copier_0
        jsr turn_on_read_mode
        lda firstarg_lo
        sta VREG_ADR0
        lda firstarg_hi
        sta VREG_ADR0 + 1
        lda VREG_PORT0
        pha
        jsr restore_mode
        jsr restore_copier_0
        pla
        ldy #0
        jmp return_uint16

; *****************************************************************************
; *** VDPEEK keyword implementation                                         ***
; *****************************************************************************
VDPEEK_IMPL:
        jsr one_argument

        jsr preserve_copier_0
        jsr turn_on_read_mode
        lda firstarg_lo
        sta VREG_ADR0
        lda firstarg_hi
        sta VREG_ADR0 + 1
        lda #1
        sta VREG_STEP0
        sei         ; disable IRQs so that we don't get interfered between bytes
        lda VREG_PORT0
        pha
        lda VREG_PORT0
        cli
        pha
        jsr restore_mode
        jsr restore_copier_0
        pla
        tay
        pla
        jmp return_uint16

; *****************************************************************************
; *** LO keyword implementation                                             ***
; *****************************************************************************
LO_IMPL:
        jsr one_argument
        ldy firstarg_lo
        jmp $b3a2   ; convert Y to byte in FAC_1 and return

; *****************************************************************************
; *** HI keyword implementation                                             ***
; *****************************************************************************
HI_IMPL:
        jsr one_argument
        ldy firstarg_hi
        jmp $b3a2   ; convert Y to byte in FAC_1 and return



opcodes:
mov_opcode:
        keyword "mov"
movi_opcode:
        keyword "movi"
wait_opcode:
        keyword "wait"
waitrel_opcode:
        keyword "delay"
setmask_opcode:
        keyword "mask"
end_opcode:
        keyword "end"
skip_opcode:
        keyword "skip"
badline_opcode:
        keyword "badline"
nop_opcode:
        keyword "nop"
waitforbad_opcode:
        keyword "waitbad"
waitrep_opcode:
        keyword "waitrep"
irq_opcode:
        keyword "irq"
seta_opcode:
        keyword "seta"
setb_opcode:
        keyword "setb"
deca_opcode:
        keyword "deca"
decb_opcode:
        keyword "decb"
xfer_opcode:
        keyword "xfer"
bra_opcode:
        keyword "bra"
unknown_opcode:
        keyword "  ???"

; Calculate shared MASK and VALUE for DELAY/MASK family of opcodes
MASKREL_ALL_MASK = VASYL_DELAYH_MASK & VASYL_DELAYV_MASK & VASYL_MASKH_MASK & VASYL_MASKV_MASK & VASYL_MASKPH_MASK & VASYL_MASKPV_MASK
MASKREL_ANDVALUE = VASYL_DELAYH_VALUE & VASYL_DELAYV_VALUE & VASYL_MASKH_VALUE & VASYL_MASKV_VALUE & VASYL_MASKPH_VALUE & VASYL_MASKPV_VALUE
MASKREL_NOTANDVALUE = ~VASYL_DELAYH_VALUE & ~VASYL_DELAYV_VALUE & ~VASYL_MASKH_VALUE & ~VASYL_MASKV_VALUE & ~VASYL_MASKPH_VALUE & ~VASYL_MASKPV_VALUE

MASKREL_MASK = MASKREL_ALL_MASK & (MASKREL_ANDVALUE | MASKREL_NOTANDVALUE)
MASKREL_VALUE = MASKREL_MASK & MASKREL_ANDVALUE


last_dlist_address:
        .word 0
line_counter:
        .byte 0
matching_table:
one_byte_ops:
        .byte VASYL_WAITBAD_MASK, VASYL_WAITBAD_VALUE
        .byte VASYL_VNOP_MASK, VASYL_VNOP_VALUE
        .byte VASYL_SKIP_MASK, VASYL_SKIP_VALUE
        .byte VASYL_IRQ_MASK, VASYL_IRQ_VALUE
        .byte VASYL_DECAB_MASK, VASYL_DECAB_VALUE
        .byte VASYL_WAITREP_MASK, VASYL_WAITREP_VALUE
        .byte VASYL_BADLINE_MASK, VASYL_BADLINE_VALUE
two_byte_ops:
        .byte VASYL_WAIT_MASK, VASYL_WAIT_VALUE
        .byte VASYL_MOV_MASK, VASYL_MOV_VALUE
        .byte VASYL_MOVI_MASK, VASYL_MOVI_VALUE
        .byte MASKREL_MASK, MASKREL_VALUE
        .byte VASYL_XFER_MASK, VASYL_XFER_VALUE
        .byte VASYL_BRA_MASK, VASYL_BRA_VALUE
matchers_end:
decoder_table:
        .dbyt WAITBAD_DECODE - 1
        .dbyt NOP_DECODE - 1
        .dbyt SKIP_DECODE - 1
        .dbyt IRQ_DECODE - 1
        .dbyt DECAB_DECODE - 1
        .dbyt WAITREP_DECODE - 1
        .dbyt BADLINE_DECODE - 1
        .dbyt WAIT_DECODE - 1
        .dbyt MOV_DECODE - 1
        .dbyt MOV_DECODE - 1    ; former MOVI
        .dbyt MASKREL_DECODE - 1
        .dbyt XFER_DECODE - 1
        .dbyt BRA_DECODE - 1
; Ordered indexes of opcodes that have no operands
; and thus can be easily decoded by a shared function
one_byte_ops_idx:
        .byte waitforbad_opcode - opcodes
        .byte nop_opcode - opcodes
        .byte skip_opcode - opcodes
        .byte irq_opcode - opcodes
; *****************************************************************************
; *** VLIST keyword implementation                                          ***
; *****************************************************************************
target_lo: .res 1
target_hi: .res 1
target_flag: .res 1
VLIST_IMPL:
        ldx #0
        stx target_flag
        bcc next_char_not_token
        tax         ; recreate CPU flags
        beq dlist_no_args
        cmp #'$'
        bne dlist_no_args

next_char_not_token:
        jsr $ad8a   ; evaluate expression and check is numeric, else do
                    ; type mismatch
        jsr $b7f7   ; convert FAC_1 to integer in temporary integer

        lda $14
        ldx $15
        sta last_dlist_address
        stx last_dlist_address+1
dlist_no_args:
        jsr $0079
        beq @skip
        cmp #','
        beq @no_error
        jmp $af08   ; SYNTAX ERROR
@no_error:
        jsr $0073
        jsr $ad8a   ; evaluate expression and check is numeric, else do
                    ; type mismatch
        jsr $b7f7   ; convert FAC_1 to integer in temporary integer

        lda $14
        ldx $15
        sta target_lo
        stx target_hi
        dec target_flag
@skip:
        lda #21
        sta line_counter

        jsr preserve_copier_0
        jsr turn_on_read_mode
        jsr set_bank
        lda last_dlist_address
        sta VREG_ADR0
        lda last_dlist_address + 1
        sta VREG_ADR0 + 1
        lda #1
        sta VREG_STEP0

opcode_loop:
        ldy VREG_ADR0
        lda VREG_ADR0 + 1
        jsr print_hex_word
        lda #':'
        jsr $ab47
        lda VREG_PORT0
        sta current_opcode

        tay
        jsr print_hex_byte

        ldx #0
try_next_match:
        tya
        and matching_table,x
        cmp matching_table+1,x
        beq match_found
        inx
        inx
        cpx #matchers_end - matching_table
        bne try_next_match

        ldx #unknown_opcode - opcodes
        jsr print_opcode
loop_decoder:
        jsr $ffe1       ; check CTRL-C
        beq @end
        jsr $aad7       ; print CR/LF

        bit target_flag ; are we listing until the end address?
        bpl @dec_line_ctr
        lda target_hi
        cmp VREG_ADR0+1
        bcc @end
        bne opcode_loop
        lda target_lo
        cmp VREG_ADR0
        bcs opcode_loop
        bcc @end

@dec_line_ctr:
        lda line_counter
        dec line_counter
        bne opcode_loop
@end:
        lda VREG_ADR0
        sta last_dlist_address
        lda VREG_ADR0 + 1
        sta last_dlist_address + 1
        jsr restore_copier_0
        jsr restore_mode
        rts

match_found:
        cpx #two_byte_ops - matching_table
        bcc @one_byte_op
        txa
        pha
        lda VREG_PORT0  ; get second byte
        sta second_byte
        jsr print_hex_byte
        pla
        tax
        jmp @dont_print_spaces
@one_byte_op:
        ; print two spaces to account for one fewer hex byte
        ; displayed for one-byte commands
        lda #' '
        jsr $ab47
        jsr $ab47
@dont_print_spaces:
        lda #>(loop_decoder - 1)
        pha
        lda #<(loop_decoder - 1)
        pha
        lda decoder_table,x
        pha
        lda decoder_table+1,x
        pha
        rts

SETAB_DECODE:
        ldx #seta_opcode - opcodes
        lda current_opcode
        and #$01
        beq @it_is_seta
        ldx #setb_opcode - opcodes
@it_is_seta:
        jsr print_opcode
        ;lda #12
        jsr print_tab
        lda second_byte
        jsr print_S_hex_byte
        jmp spacefill

MASKREL_DECODE:
        ldx #setmask_opcode - opcodes
        lda current_opcode
        and #VASYL_DELAYH_VALUE ^ VASYL_MASKH_VALUE
        bne @setmask_detected
        lda current_opcode
        and #VASYL_SETAB_MASK
        cmp #VASYL_SETAB_VALUE
        beq SETAB_DECODE
@not_waitrep:
        ldx #waitrel_opcode - opcodes
@setmask_detected:
        stx mask_or_delay
        jsr print_opcode
        lda current_opcode
        and #VASYL_MASKH_VALUE ^ VASYL_MASKPH_VALUE
        beq @not_persistent
        lda #'p'
        jsr $ab47
        inc char_counter    ; account for "P"
@not_persistent:
        ldx #'v'
        lda current_opcode
        and #VASYL_DELAYH_VALUE ^ VASYL_DELAYV_VALUE
        bne @is_vertical
        ldx #'h'
@is_vertical:
        txa
        jsr $ab47
        lda #9
        inc char_counter    ; account for either "v" or "h"
        jsr print_adj_tab

        lda current_opcode
        tax
        and #VASYL_DELAYH_VALUE ^ VASYL_DELAYV_VALUE
        bne @vertical_decode
        lda #' '
        jsr $ab47

        ; we're only interested in the second argument when processing DELAY*
        lda mask_or_delay
        cmp #waitrel_opcode - opcodes
        bne @no_v_in_delayh

        lda second_byte
        and #$c0
        beq @no_v_in_delayh
        clc
        rol
        rol
        rol
        jsr print_S_hex_byte
        lda #','
        jsr $ab47
@no_v_in_delayh:
        lda second_byte
        and #$3f
        jsr print_S_hex_byte
        jmp binary_decode_maybe

@vertical_decode:
        txa
        and #1
        ldy second_byte
        jsr print_S_hex_9bit

binary_decode_maybe:
        lda current_opcode
        and #VASYL_DELAYH_VALUE ^ VASYL_MASKH_VALUE
        beq @end

        lda current_opcode
        and #VASYL_DELAYH_VALUE ^ VASYL_DELAYV_VALUE
        bne @mask_vertical
        lda second_byte
        asl
        asl
        clc
        bcc @print_binary
@mask_vertical:
        lda current_opcode
        and #$01
        tax
        lda second_byte
        sec
@print_binary:
        jmp print_binary
@end:
spacefill:
        lda #39
        sec
        sbc $d3
        tay
        jmp print_y_spaces


WAIT_DECODE:
        lda current_opcode
        cmp #127
        bne @not_end_opcode
        lda second_byte
        cmp #255
        bne @not_end_opcode

        ldx #end_opcode - opcodes
        jsr print_opcode
        jmp spacefill

@not_end_opcode:
        ldx #wait_opcode - opcodes
        jsr print_opcode
        lda #9
        jsr print_adj_tab

        lda current_opcode
        pha
        and #1
        ldy second_byte
        jsr print_S_hex_9bit
        lda #','
        jsr $ab47
        pla
        lsr
        and #63
        jsr print_S_hex_byte
        jmp spacefill

MOV_DECODE:
        ldx #mov_opcode - opcodes
        jsr print_opcode
        jsr print_tab
        lda current_opcode
        tax
        and #%01000000
        bne plain_mov
        txa
        and #%00011111
        clc
        adc #$40
        bne mov_shared
plain_mov:
        txa
        and #%00111111
mov_shared:
        pha
        jsr print_S_hex_byte
        lda #','
        jsr $ab47
        lda second_byte
        jsr print_S_hex_byte
        pla
        jmp print_reg_name

XFER_DECODE:
        ldx #xfer_opcode - opcodes
        jsr print_opcode
        jsr print_tab
        lda second_byte
        and #$7f
        pha
        jsr print_S_hex_byte
        lda #','
        jsr $ab47
        lda #'('
        jsr $ab47
        lda second_byte
        and #$80
        clc
        rol
        rol
        jsr print_hex_digit
        lda #')'
        jsr $ab47
        pla
        jmp print_reg_name

BRA_DECODE:
        ldx #bra_opcode - opcodes
        jsr print_opcode
        lda #8
        jsr print_adj_tab
        lda #'$'
        jsr $ab47

        lda second_byte
        sta copier_step
        lda VREG_ADR0
        sta src
        lda VREG_ADR0 + 1
        sta src + 1
        jsr increment_copier

        ldy src
        lda src + 1
        jmp print_hex_word


WAITBAD_DECODE:
NOP_DECODE:
SKIP_DECODE:
IRQ_DECODE:
        txa
        sec
        sbc #one_byte_ops - matching_table
        lsr
        tay
        ldx one_byte_ops_idx,y
        jsr print_opcode
        lda #$1c
        jmp print_adj_tab

BADLINE_DECODE:
        ldx #badline_opcode - opcodes
        jsr print_opcode
        lda #12
        jsr print_adj_tab
        lda current_opcode
        and #$07
        jsr print_hex_digit
        jmp spacefill


WAITREP_DECODE:
        ldx #waitrep_opcode - opcodes
        jsr print_opcode
        lda #12
        jsr print_adj_tab
        lda current_opcode
        and #$01
        jsr print_hex_digit
        jmp spacefill

DECAB_DECODE:
        ldx #deca_opcode - opcodes
        lda current_opcode
        and #$01
        beq @it_is_deca
        ldx #decb_opcode - opcodes
@it_is_deca:
        jsr print_opcode
        jmp spacefill

print_tab:
        lda #10
print_adj_tab:
        sec
        sbc char_counter
        tay
print_y_spaces:
        lda #' '
tab_loop:
        jsr $ab47
        dey
        bne tab_loop
        rts

print_opcode:
        lda #' '
        jsr $ab47 ; print character
;        cpx #unknown_opcode - opcodes
;        beq @skip_v
;        lda #'v'
;@skip_v:
        jsr $ab47 ; print character
        lda #$00
        sta char_counter
print_loop:
        lda opcodes,x
        inc char_counter
        inx
        pha
        and #$7f
        jsr $ab47
        pla
        bpl print_loop
        rts

; parse two arguments of VMOV and VWAIT
two_arguments:
        lda function_flag
        pha
        lda LINNUMHI ; $15
        pha
        lda LINNUMLO ; $14
        pha
        bit function_flag
        bpl @not_function
        jsr CHKOPN   ; $aefa - scan for "(", else do syntax error then warm start
@not_function:
        jsr FRMNUM   ; $ad8a - evaluate expression and check is numeric, else do
                     ; type mismatch
        ;jsr GETADR   ; $b7f7 - Convert a Floating Point Number to
                     ; an Unsigned Two-Byte Integer in LINNUM ($14/$15)
        jsr $b7fb
        lda LINNUMLO ; $14
        pha
        lda LINNUMHI ; $15
        pha
        jsr CHKCOM   ; $aefd scan for ",", else do syntax error then warm start
        jsr get_signed_byte

        pla
        sta firstarg_hi
        pla
        sta firstarg_lo
        pla
        sta LINNUMLO ; $14
        pla
        sta LINNUMHI ; $15
        pla
        sta function_flag
        bit function_flag
        bpl @not_function2
        jsr CHKCLS   ; $aef7 - Check for and Skip Closing Parentheses ")"
@not_function2:
        rts

; parse one argument of VDELAY/MASK
one_argument:
        lda $15
        pha
        lda $14
        pha
        lda function_flag
        pha
        bpl @not_function
        jsr $aefa   ; scan for "(", else do syntax error then warm start
@not_function:
        lda function_flag
        and #1
        beq @not_offset
        jsr $0079
        cmp #$AC    ; token "*"
        bne @not_offset
        jsr $0073
; notify the caller that we're indeed processing an offset
        tsx
        dec $101,x
@not_offset:
        jsr $ad8a   ; evaluate expression and check is numeric, else do
                    ; type mismatch
        ;jsr $b7f7  ; check if positive 16-bit value
        jsr $b7fb   ; check if 16-bit value
        lda $14
        sta firstarg_lo
        lda $15
        sta firstarg_hi
        pla
        sta function_flag
        pla
        sta $14
        pla
        sta $15
        bit function_flag
        bpl @not_function2
        jsr $aef7   ; scan for ")"
@not_function2:
        rts


; *****************************************************************************
; *** Check if the next character is a '#', and if so, decode copier
; *** number that follows it. Otherwise current_copier remains unchanged.
; *** Check if current copier has been configured.
; *** Args:
; ***   A - current character to process
; ***   X - if negative, we are processing VEND and no comma should follow
; ***       the copier number if present.
; *****************************************************************************
get_copier_id:
        stx vend_flag
        cmp #'#'
        bne no_hash
        jsr $0073   ; get next character
        jsr $b79e   ; get byte parameter
        cpx #COPIER_COUNT
        bcc @copier_ok
        ldx #9      ; "Illegal device number"
        jmp $a437   ; ERROR
@copier_ok:
	stx current_copier
        bit vend_flag   ; are we processing VEND keyword?
        bmi no_comma
        jsr CHKCOM  ; $aefd - scan for ",", else do syntax error then warm start
no_comma:
no_hash:
        ldx current_copier
        bmi unconfigured
check_if_configured:
        lda copier_state,x
        bpl copier_configured
unconfigured:
        jmp $b248   ; "Illegal quantity" for now, but maybe change it later
copier_configured:
        rts


; *****************************************************************************
; *** VDATA keyword implementation                                          ***
; *****************************************************************************
VDATA_IMPL:
        ldx #$ff
        jsr get_copier_id
        jsr $0079
        beq @end
        cmp #','
        bne @skip_first_getchar
        jsr $0073
@skip_first_getchar:
        cmp #'"'
        bne @not_quote
@next_byte:
        jsr $0073
        cmp #'"'
        beq @nomore
        jsr $0079
        jsr binarize_hex_digit
        bcs @error
        asl
        asl
        asl
        asl
        sta src
        jsr $0073
        jsr binarize_hex_digit
        bcc @ok
@error: jmp $af08       ; SYNTAX error
@ok:
        ora src
        tay
        sec
        jsr store_or_output
        jmp @next_byte
@nomore:
        jsr $0073
        rts


@more_data:
        jsr $0073
@not_quote:
        jsr $b79e   ; get byte parameter
        txa
        tay
        sec
        jsr store_or_output
        jsr $0079
        cmp #','
        beq @more_data
@end:
        rts

binarize_hex_digit:
        bcc @decimal_digit
        cmp #'f' + 1
        bcs @not_hex_digit
        cmp #'a'
        bcc @not_hex_digit
        sec
        sbc #'a'-10
        bne @store_nibble
@decimal_digit:
        sec
        sbc #'0'
@store_nibble:
        clc
        rts
@not_hex_digit:
        sec
        rts

; *****************************************************************************
; *** LABEL keyword implementation                                          ***
; *****************************************************************************
LABEL_IMPL:
        ldx #$ff
        jsr get_copier_id
        jsr $0079
        beq @end
        cmp #','
        bne @skip_first_getchar
@more_data:
        jsr $0073
@skip_first_getchar:
        jsr $b08b   ; search for a variable
        sta $49
        sty $4a

        ldx current_copier
        lda copier_addresses_lo,x
        sta src
        lda copier_addresses_hi,x
        sta src+1

        ldy #0
        lda (src),y
        iny
        sta ($49),y
        lda (src),y
        dey
        sta ($49),y
@end:
        rts

; *****************************************************************************
; *** VMOV/VMOVI keyword implementation                                     ***
; *****************************************************************************
VMOV_IMPL:
VMOVI_IMPL:
        nop
        sty current_function_index
        bit function_flag
        bmi vmov_impl_function

        ldx #0 ; not a VEND
        jsr get_copier_id

vmov_impl_function:
        jsr two_arguments ; parsed arguments into firstarg and X respectively
        lda firstarg_hi
        bne hibyte_nonzero
        txa               ; second arg to A
        tay               ; and to Y (for store_or_output)
        lda firstarg_lo
        cmp #$50          ; MOV target register must be <= $4f
        bcc mov_reg_ok
hibyte_nonzero:
        jmp $b798         ; "Illegal quantity"
mov_reg_ok:
        cmp #$40          ; addresses lower than $d040 are "regular"
        bcc @regular_mov  ; branch if lower than $40
        sbc #$40          ; carry is already set
        ora #%10000000
        bne mov_common    ; uncondtitional
@regular_mov:
        ora #%11000000
mov_common:
        clc

store_or_output:    ; A - hibyte, Y - lobyte, C - one arg if set
        tax
        lda #255
        adc #0
        sta two_args_flag
        bne @two_args
        ldy #0
@two_args:
        txa
        bit function_flag
        bpl transfer_to_memory

return_uint16:
        ldx #0      ; "manual" int to string conversion
        stx $0d     ; due to values >32767 that would normally
        sty $62
        sta $63     ; present as negative
        stx $66
        stx $70
        ldx #$90
        stx $61
        jmp $bccc   ; INT() - rounding to avoid digits after decimal point

set_bank:
        lda VREG_CONTROL
        and #(~CONTROL_RAMBANK_MASK) + 256  ; A contains VREG_MODE
        ora current_bank
        sta VREG_CONTROL
        rts

preserve_ctrl_set_bank_from_x:
        pha
        lda VREG_CONTROL
        sta control_store
        and #(~CONTROL_RAMBANK_MASK) + 256
        sta temp_store
        txa
        ora temp_store
        sta VREG_CONTROL
        pla
        rts

restore_bank:
        pha
        lda control_store
        sta VREG_CONTROL
        pla
        rts

temp_store:
        .res 1
control_store:
        .res 1


transfer_to_memory:
        ldx current_copier
transfer_to_memory_copier_x:
        bpl current_copier_ok
        jmp $b798   ; "Illegal quantity" - maybe we can have better message here
current_copier_ok:
        cpx #MAIN_RAM_COPIER
        bcs copier2_vmov_to_main_ram
        dex
        beq @copier1

	    ldx copier_banks
        jsr preserve_ctrl_set_bank_from_x ; sets bank from X, preserves A
        sta VREG_PORT0
        bit two_args_flag
        bpl @no_second_arg
        sty VREG_PORT0
        bmi @no_second_arg ; unconditional

@copier1:
	    ldx copier_banks + 1
        jsr preserve_ctrl_set_bank_from_x ; sets bank from X, preserves A
        sta VREG_PORT1
        bit two_args_flag
        bpl @no_second_arg
        sty VREG_PORT1
@no_second_arg:
        jmp restore_bank

copier2_vmov_to_main_ram:
        ldx copier_adr
        stx src
        ldx copier_adr+1
        stx src+1
        pha
        tya
        tax
        pla
        ldy #0
        sta (src),y
        jsr increment_copier
        bit two_args_flag
        bpl skip_hi_byte
        txa
        sta (src),y
inc_and_store_copier:
        jsr increment_copier
skip_hi_byte:
        ldx src
        stx copier_adr
        ldx src + 1
        stx copier_adr + 1
        rts

transfer_from_memory:
        ldy current_copier
transfer_from_memory_copier_y:
        cpy #MAIN_RAM_COPIER
        bcs vmov_from_main_memory
        ldx copier_banks,y
        jsr preserve_ctrl_set_bank_from_x ; preserves X
        tya
        asl
        asl
        tax
        lda VREG_PORT0,x
        jmp restore_bank

vmov_from_main_memory:
        ldx copier_adr
        stx src
        ldx copier_adr+1
        stx src+1
        ldy #0
        lda (src),y
        pha
        jsr inc_and_store_copier
        pla
        rts


; *****************************************************************************
; *** BANK keyword implementation                                           ***
; *****************************************************************************
BANK_IMPL:
        nop
        bit function_flag
        bmi @bank_impl_function

        jsr get_arg_less_than_8
        .repeat CONTROL_RAMBANK_BIT
        asl
        .endrep
        sta current_bank
        rts
@bank_impl_function:
        jsr $aefa   ; scan for "(", else do syntax error then warm start
        jsr $aef7   ; scan for ")"
        lda current_bank
        .repeat CONTROL_RAMBANK_BIT
        lsr
        .endrep
        ldy #0
        jmp return_uint16


; *****************************************************************************
; *** RACER keyword implementation                                          ***
; *** 0 - turn off, 1 - turn on, 2 - turn on and survive RUN/STOP-RESTORE   ***
; *****************************************************************************
RACER_IMPL:
        nop
        bit function_flag
        bmi @racer_impl_function

        jsr $b79e      ; get byte parameter
        cpx #3         ; argument must be <= 3
        bcs illegal_quantity
        bit racer_mode ; negative means there is no BeamRacer installed
        bpl @set_racer_mode
        rts

@set_racer_mode:
        stx racer_mode
        cpx #0
        beq @end
        jmp knock_knock
@end:
        lda $d02e
        ora #$40    ; bit6 is used as un-knock trigger
        sta $d02e   ; un-knock BeamRacer
        rts

@racer_impl_function:
        jsr $aefa   ; scan for "(", else do syntax error then warm start
        jsr $aef7   ; scan for ")"
        lda racer_mode
        ldy #0
        jmp return_uint16


get_arg_less_than_8:
        jsr $b79e   ; get byte parameter
        cpx #8     ; argument "horizontal" must be <= 7
        bcs illegal_quantity
        txa
        rts


; *****************************************************************************
; *** DLON keyword implementation                                           ***
; *****************************************************************************
DLON_IMPL:
        lda VREG_CONTROL
        ora #1 << CONTROL_DLIST_ON_BIT
        bne store_control1  ; branch always

; *****************************************************************************
; *** DLOFF keyword implementation                                          ***
; *****************************************************************************
DLOFF_IMPL:
        lda VREG_CONTROL
        and #~(1 << CONTROL_DLIST_ON_BIT) + 256
store_control1:
        sta VREG_CONTROL
        rts

; *****************************************************************************
; *** VWAIT keyword implementation                                          ***
; *****************************************************************************
VWAIT_IMPL:
        nop
        bit function_flag
        bmi vwait_impl_function

        ldx #0 ; non-VEND flag
        jsr get_copier_id

vwait_impl_function:
        jsr two_arguments
        cpx #64     ; argument "horizontal" must be <= 63
        bcs illegal_quantity
        lda firstarg_hi
        cmp #2      ; argument "vertical" must be <= 511
        bcs illegal_quantity
vertical_ok:
        txa
        asl
        ora firstarg_hi
        ldy firstarg_lo
        clc
        jmp store_or_output
illegal_quantity:
        jmp $b798   ; "Illegal quantity"

; *****************************************************************************
; *** VEND keyword implementation                                           ***
; *****************************************************************************
VEND_IMPL:
        nop
        bit function_flag
        bmi vend_impl_function

        ldx #$ff ; VEND flag
        jsr get_copier_id
        jmp omit_bracket_scan

vend_impl_function:
        jsr $aefa   ; scan for "(", else do syntax error then warm start
        jsr $aef7   ; scan for ")"
omit_bracket_scan:
        lda #<511
        sta firstarg_lo
        lda #>511
        sta firstarg_hi
        ldx #63
        bne vertical_ok


; *****************************************************************************
; *** VDELAYH keyword implementation                                        ***
; *****************************************************************************
VDELAYH_IMPL:
        nop
        bit function_flag
        bmi vdelayh_impl_function

        ldx #0 ; non-VEND flag
        jsr get_copier_id

vdelayh_impl_function:
        jsr GETBYT ; $b79e - get byte parameter ; Parse A into uint8_t in X
        ldy #0
        jsr $0079
        cmp #','
        bne @no_second_arg

        txa
        pha
        jsr $0073
        jsr GETBYT
        pla
        tay

@no_second_arg:
        cpx #64
        bcs illegal_quantity
        stx firstarg_lo

        cpy #4
        bcs illegal_quantity
        tya
        clc
        ror
        ror
        ror
        ora firstarg_lo
        tay
        lda #%10110000
        clc
        jmp store_or_output


; *****************************************************************************
; *** VDELAYV keyword implementation                                        ***
; *****************************************************************************
VDELAYV_IMPL:
        nop
        bit function_flag
        bmi vdelayv_impl_function

        ldx #0
        jsr get_copier_id

vdelayv_impl_function:
        jsr one_argument
        lda firstarg_hi
        cmp #2
        bcs illegal_quantity
        ora #%10111000
        ldy firstarg_lo
        clc
        jmp store_or_output

; Add 8-bit signed step to the address
increment_copier:
        bit copier_step
        bpl @step_is_positive
        lda #0
        sec
        sbc src
        beq @step_larger    ; LSB is 0, so MSB will have to be decreased
        cmp copier_step
        beq @step_not_larger
        bcc @step_not_larger
@step_larger:
        dec src + 1
@step_is_positive:
@step_not_larger:
        lda src
        clc
        adc copier_step
        sta src
        bit copier_step
        bmi step_is_negative
        bcc @no_carry
        inc src + 1
@no_carry:
step_is_negative:
        rts

; *****************************************************************************
; *** Implementation of keywords with no operands                           ***
; *****************************************************************************
no_operand_opcodes:
        .byte %10100100 ; WAITBAD
        .byte %10100111 ; NOP
        .byte %10100110 ; SKIP
        .byte %10100010 ; IRQ
        .byte %10100000 ; DECA
        .byte %10100001 ; DECB

VWAITBAD_IMPL:
VNOP_IMPL:
VSKIP_IMPL:
VIRQ_IMPL:
VDECA_IMPL:
VDECB_IMPL:

        nop
        sty current_function_index
        bit function_flag
        bmi @vskip_impl_function

        ldx #$ff
        jsr get_copier_id
        jmp @omit_bracket_scan

@vskip_impl_function:
        jsr $aefa   ; scan for "(", else do syntax error then warm start
        jsr $aef7   ; scan for ")"
@omit_bracket_scan:
        lda current_function_index
        sec
        sbc #no_op_functions - vb_functions
        lsr
        tay
        lda no_operand_opcodes,y
        sec
        jmp store_or_output

; *****************************************************************************
; *** VBADLINE keyword implementation                                       ***
; *****************************************************************************
VBADLINE_IMPL:
        nop
        bit function_flag
        bmi @vbadoff_impl_function

        ldx #0
        jsr get_copier_id

@vbadoff_impl_function:
        jsr get_arg_less_than_8
        ora #%10101000
        sec
        jmp store_or_output

; *****************************************************************************
; *** VBRA keyword implementation                                           ***
; *****************************************************************************
VBRA_IMPL:
        nop
        bit function_flag
        bmi @vbra_impl_function

        ldx #0 ; Not a VEND
        jsr get_copier_id

@vbra_impl_function:
        inc function_flag
        jsr one_argument
        
        lda function_flag
        and #1
        beq @check_bounds
        dec function_flag
@not_offset:
        ; adjustment for the fact that BRA is in relation to the
        ; first address immediately following it.
        lda firstarg_lo
        sec
        sbc #2
        sta firstarg_lo
        bcs @no_carry
        dec firstarg_hi
@no_carry:

        ldx current_copier
        lda copier_addresses_lo,x
        sta src
        lda copier_addresses_hi,x
        sta src + 1

        ldy #0
        sec
        lda firstarg_lo
        sbc (src),y
        sta firstarg_lo
        iny
        lda firstarg_hi
        sbc (src),y
        sta firstarg_hi
@check_bounds:
        ;check bounds
        lda firstarg_hi
        beq @bra_positive
        cmp #255
        beq @bra_negative
@bra_out:
        jmp illegal_quantity3
@bra_positive:
        ldy firstarg_lo
        bmi @bra_out
        bpl @bra_ok

@bra_negative:
        ldy firstarg_lo
        bpl @bra_out
@bra_ok:
        lda #%10100011 ; BRA opcode
        clc
        jmp store_or_output


; *****************************************************************************
; *** VXFER keyword implementation                                          ***
; *****************************************************************************
VXFER_IMPL:
        nop
        bit function_flag
        bmi @vxfer_impl_function

        ldx #0
        jsr get_copier_id

@vxfer_impl_function:
        jsr two_arguments
        cpx #2
        bcs illegal_quantity2
        txa
        clc
        ror
        ror

        ldx firstarg_hi
        bne illegal_quantity2
        ldx firstarg_lo
        cpx #$50        ; highest register is $4f
        bcs illegal_quantity2

        ora firstarg_lo
        tay

        lda #%10100101
        clc
        jmp store_or_output

illegal_quantity2:
        jmp $b798   ; "Illegal quantity"

; *****************************************************************************
; *** VWAITREP keyword implementation                                       ***
; *****************************************************************************
VWAITREP_IMPL:
        nop
        bit function_flag
        bmi @vwaitrep_impl_function

        ldx #0 ; not a VEND
        jsr get_copier_id

@vwaitrep_impl_function:
        jsr one_argument
        lda firstarg_hi
        bne illegal_quantity2
        lda firstarg_lo
        cmp #2     ; argument "counter" must be <= 1
        bcs illegal_quantity2
        ora #%10111010 ; WAITREP opcode
        sec        ; one argument
        jmp store_or_output

; *****************************************************************************
; *** VSET keyword implementation                                           ***
; *****************************************************************************
VSETA_IMPL:
VSETB_IMPL:
        nop
        sty current_function_index
        bit function_flag
        bmi @vctrset_impl_function

        ldx #0 ; not a VEND
        jsr get_copier_id

@vctrset_impl_function:
        jsr one_argument

        lda firstarg_hi
        bne illegal_quantity2
        lda #%10110010
        ldy current_function_index
        cpy #vsetb_func - vb_functions
        adc #0
        ldy firstarg_lo
        clc
        jmp store_or_output

illegal_quantity3:
        jmp $b798   ; "Illegal quantity"

; *****************************************************************************
; *** VMASK keyword implementation                                          ***
; *** VMASKH, VMASKV, VMASKPH, VMASKPV combined                             ***
; *****************************************************************************
VMASK_IMPL:
        nop
        sty current_function_index
        bit function_flag
        bmi @vsetmask_impl_function

        ldx #0
        jsr get_copier_id

@vsetmask_impl_function:
        jsr one_argument
        lda current_function_index
        cmp #vsetmaskv_func - vb_functions
        beq @vertical
        cmp #vsetmaskpv_func - vb_functions
        bne @not_vertical
@vertical:
        lda firstarg_hi
        cmp #2
        bcs illegal_quantity3
        ora #%00001000      ; turn on V bit
        bne @vsetmask_common
@not_vertical:
        lda firstarg_lo
        cmp #64     ; argument "horizontal" must be <= 63
        bcs illegal_quantity3
        lda firstarg_hi
        bne illegal_quantity3

@vsetmask_common:
        ldx current_function_index
        cpx #vsetmaskph_func - vb_functions
        bcc @not_persistent
        ora #%00000010
@not_persistent:
        ora #%10110100
        tay
        ldy firstarg_lo
        clc
        jmp store_or_output


; *****************************************************************************
; *** ABOUT function implementation                                         ***
; *****************************************************************************
about_string:
        .include "about_string.i"
about_string_end:
ABOUT_IMPL:
        nop
        jsr $aefa   ; scan for "(", else do syntax error then warm start
        jsr $aef7   ; scan for ")"


        lda #about_string_end - about_string + beamracer_string_end - beamracer_string
        jsr $b47d   ; reserve space for A-long string

        lda $62
        sta dst
        lda $63
        sta dst + 1

        lda #<about_string
        ldx #>about_string
        ldy #about_string_end - about_string
        jsr copy_to_aboutstring

        lda #<beamracer_string
        ldx #>beamracer_string
        ldy #beamracer_string_end - beamracer_string
        jsr copy_to_aboutstring

        pla
        pla
        pla

        jmp $b4ca

copy_to_aboutstring:
        sta src
        stx src + 1
        tya
        pha
        dey
@loop:
        lda (src),y
        sta (dst),y
        dey
        bpl @loop

        pla
        clc
        adc dst
        sta dst
        bcc @nocarry
        inc dst + 1
@nocarry:
        rts



; *****************************************************************************
; *** COPY command implementation                                           ***
; *****************************************************************************
COPY_IMPL:
        jsr parse_copier
        stx copy_to_copier
        jsr CHKCOM   ; $aefd - scan for ",", else do syntax error then warm start
        jsr parse_copier
        stx copy_from_copier
        jsr CHKCOM   ; $aefd - scan for ",", else do syntax error then warm start
        jsr FRMNUM   ; $ad8a - evaluate expression and check is numeric, else do
                     ; type mismatch
        jsr GETADR   ; $b7f7 - convert FAC_1 to uint16_t in LINNUM ($14/$15)
        jsr turn_on_read_mode
        lda #0       ; copy byte by byte (one arg)
        sta two_args_flag
        beq @decrement  ; unconditional, decrement first to avoid off-by-one

@copy_next_byte:
        ldy copy_from_copier
        jsr transfer_from_memory_copier_y
        ldx copy_to_copier
        jsr transfer_to_memory_copier_x
@decrement:
        ldx #255
        dec LINNUM
        cpx LINNUM
        bne @copy_next_byte
        dec LINNUMHI
        cpx LINNUMHI
        bne @copy_next_byte

        jsr restore_mode
        rts

parse_copier:
        cmp #'#'
        bne @no_hash
        jsr $0073   ; get next character
        jsr GETBYT  ; $b79e - Parse A into uint8_t in X
        cpx #COPIER_COUNT
        bcc @copier_ok
@no_hash:
        ldx #9
        jmp $a437   ; "Illegal device number"
@copier_ok:
        jmp check_if_configured

evaluate_expression:
        lda #0
        sta $0d
        jsr $0073
        bcs not_digit
        jmp $ae8f
not_digit:
        cmp #'$'
        beq eval_dollar
        jmp $ae92
eval_dollar:
        ldx #0
        stx $63
        stx $64
        stx $65
        stx $66
decode_next_digit:
        jsr $0073
        jsr binarize_hex_digit
        bcs not_hex_digit
        sta value,x
        inx
        cpx #4
        bne decode_next_digit
        jsr $0073
not_hex_digit:
        lda #%01010101  ; we'll rotate this to get odd/even indicator
        sta digit_oddness
        dex
        bmi evaluate_error
        txa
        lsr
        tay
        lda exponent_LUT,y  ; if one or two digits the number is 8-bit.
        sta $61             ; 16-bit otherwise
convert_next_digit:
        lda value,x
        bit digit_oddness
        bmi even_digit
        sta $62,y
        jmp skip_odd
even_digit:
        asl
        asl
        asl
        asl
        ora $62,y
        sta $62,y
        dey
skip_odd:
        rol digit_oddness
        dex
        bpl convert_next_digit

        sec
        jmp $b8d2
evaluate_error:
        jmp $af08   ; SYNTAX ERROR

exponent_LUT:
        .byte $88, $90


print_S_hex_9bit:
        pha
        lda #'$'
        jsr $ab47
        pla
        jsr print_hex_digit
        tya
        jmp print_hex_byte

print_S_hex_byte:
        pha
        lda #'$'
        jsr $ab47
        pla
        jmp print_hex_byte

print_hex_word:
        jsr print_hex_byte
        tya
print_hex_byte:
        pha
        lsr
        lsr
        lsr
        lsr
        tax
        lda hexdigits,x
        jsr $ab47
        pla
print_hex_digit:
        and #$0f
        tax
        lda hexdigits,x
        jmp $ab47

hexdigits:
        .byte "0123456789abcdef"
digit_counter:
        .byte 0


custom_convert_fac1_to_string:
        lda ampersand_flag
        bne convert_to_hex
        jmp $bddd
convert_to_hex:
        lda #'$'
        jsr $ab47

        lda #<$100
        sta dst
        lda #>$100
        sta dst + 1
        jsr $bc9b   ; convert FAC1 floating to fixed
        ldy #0
        sty skip_lz_flag
        lda ampersand_flag

        ldx #0
        cmp #1
        beq bits_unknown
        cmp #'l'
        beq out_next_digit
        ldx #2
        cmp #'w'
        beq out_next_digit
        ;cmp #'b'
        bne out_last_digit ; must be "b"
bits_unknown:
        dec skip_lz_flag

out_next_digit:
        txa
        pha
        lda $62,x
        jsr out_hex_byte
        pla
        tax
        inx
        cpx #3
        bne out_next_digit
out_last_digit:
        lda $65     ; always output last digit
        jsr dont_skip_lz

        lda #0
        sta (dst),y ; zero-terminate the string
        sta ampersand_flag
        ldy #>$100  ; A/Y - A is already zero
        rts


out_hex_byte:
        bit skip_lz_flag
        bpl dont_skip_lz
        tax     ; just to get flags
        beq skip_lz
        inc skip_lz_flag

dont_skip_lz:
        pha
        lsr
        lsr
        lsr
        lsr
        tax
        lda hexdigits,x
        sta (dst),y
        iny
        pla
        and #$0f
        tax
        lda hexdigits,x
        sta (dst),y
        iny
skip_lz:
        rts

evaluate_expression_patch:
        jsr $0079
        php
        cmp #'&'
        bne not_ampersand
        plp
        jsr $0073
        php
; check if a width modifier was specified
        cmp #'b'
        beq width_flag
        cmp #'w'
        beq width_flag
        cmp #'l'
        beq overwrite_flag
; if no modifier still raise the flag, but
; do not consume the next character
        lda ampersand_flag
        bne not_ampersand
        inc ampersand_flag
        bne not_ampersand
width_flag:
; sort out priorities: L > W > B
        ldx ampersand_flag
        cpx #'l'
        beq no_flag_change
        cmp ampersand_flag
        bcc no_flag_change
overwrite_flag:
        sta ampersand_flag
no_flag_change:
        plp
        jsr $0073
        php
not_ampersand:
        plp
        jmp $adbb   ; continue ROM expression evaluation


run_patch:
        jsr unconfig_copiers
        jmp $a659



keyword_ptrs:
end_keyword:
        keyword "end"
for_keyword:
        keyword "for"
next_keyword:
        keyword "next"
data_keyword:
        keyword "data"
input2_keyword:
        keyword "input#"
input_keyword:
        keyword "input"
dim_keyword:
        keyword "dim"
read_keyword:
        keyword "read"
let_keyword:
        keyword "let"
goto_keyword:
        keyword "goto"
run_keyword:
        keyword "run"
if_keyword:
        keyword "if"
restore_keyword:
        keyword "restore"
gosub_keyword:
        keyword "gosub"
return_keyword:
        keyword "return"
rem_keyword:
        keyword "rem"
stop_keyword:
        keyword "stop"
on_keyword:
        keyword "on"
wait_keyword:
        keyword "wait"
load_keyword:
        keyword "load"
save_keyword:
        keyword "save"
verify_keyword:
        keyword "verify"
def_keyword:
        keyword "def"
poke_keyword:
        keyword "poke"
print2_keyword:
        keyword "print#"
print_keyword:
        keyword "print"
cont_keyword:
        keyword "cont"
list_keyword:
        keyword "list"
clr_keyword:
        keyword "clr"
cmd_keyword:
        keyword "cmd"
sys_keyword:
        keyword "sys"
open_keyword:
        keyword "open"
close_keyword:
        keyword "close"
get_keyword:
        keyword "get"
new_keyword:
        keyword "new"
tab_keyword:
        keyword "tab("
to_keyword:
        keyword "to"
fn_keyword:
        keyword "fn"
spc_keyword:
        keyword "spc("
then_keyword:
        keyword "then"
not_keyword:
        keyword "not"
step_keyword:
        keyword "step"
plus_keyword:
        keyword "+"
minus_keyword:
        keyword "-"
asterisk_keyword:
        keyword "*"
slash_keyword:
        keyword "/"
apostrophe_keyword:
        keyword "^"
and_keyword:
        keyword "and"
or_keyword:
        keyword "or"
less_keyword:
        keyword ">"
equal_keyword:
        keyword "="
more_keyword:
        keyword "<"
sgn_keyword:
        keyword "sgn"

int_keyword:
        keyword "int"
abs_keyword:
        keyword "abs"
usr_keyword:
        keyword "usr"
fre_keyword:
        keyword "fre"
pos_keyword:
        keyword "pos"
sqr_keyword:
        keyword "sqr"
rnd_keyword:
        keyword "rnd"
log_keyword:
        keyword "log"
exp_keyword:
        keyword "exp"
cos_keyword:
        keyword "cos"
sin_keyword:
        keyword "sin"
tan_keyword:
        keyword "tan"
atn_keyword:
        keyword "atn"
peek_keyword:
        keyword "peek"
len_keyword:
        keyword "len"
str_keyword:
        keyword "str$"
val_keyword:
        keyword "val"
asc_keyword:
        keyword "asc"
chr_keyword:
        keyword "chr$"
left_keyword:
        keyword "left$"
right_keyword:
        keyword "right$"
mid_keyword:
        keyword "mid$"
go_keyword:
        keyword "go"

; our custom tokens start here
token_id .set $CC
vlist_keyword:
        keyword "vlist"
vpoke_keyword:
        keyword "vpoke"
vdpoke_keyword:
        keyword "vdpoke"
copy_keyword:
        keyword "copy"
dpoke_keyword:
        keyword "dpoke"
vcfg_keyword:
        keyword "vcfg"
vdata_keyword:
        keyword "vdata"
label_keyword:
        keyword "label"
dlon_keyword:
        keyword "dlon"
dloff_keyword:
        keyword "dloff"

; tokens that are both a function and a keyword
npf_tokens_start = token_id
vdelayh_keyword:
        keyword "vdelayh"
vdelayv_keyword:
        keyword "vdelayv"
vwaitbad_keyword:
        keyword "vwaitbad"
vnop_keyword:
        keyword "vnop"
vskip_keyword:
        keyword "vskip"
virq_keyword:
        keyword "virq"
vdeca_keyword:
        keyword "vdeca"
vdecb_keyword:
        keyword "vdecb"
vbadline_keyword:
        keyword "vbadline"
vmov_keyword:
        keyword "vmov"
vwaitrep_keyword:
        keyword "vwaitrep"
vend_keyword:
        keyword "vend"
vsetmaskh_keyword:
        keyword "vmaskh"
vsetmaskv_keyword:
        keyword "vmaskv"
vsetmaskph_keyword:
        keyword "vmaskph"
vsetmaskpv_keyword:
        keyword "vmaskpv"
vseta_keyword:
        keyword "vseta"
vsetb_keyword:
        keyword "vsetb"
vxfer_keyword:
        keyword "vxfer"
bank_keyword:
        keyword "bank"
vwait_keyword:
        keyword "vwait"
vbra_keyword:
        keyword "vbra"
racer_keyword:
        keyword "racer"

last_npf_token = token_id

; pure function tokens
dpeek_keyword:
        keyword "dpeek"
vdpeek_keyword:
        keyword "vdpeek"
vpeek_keyword:
        keyword "vpeek"
lo_keyword:
        keyword "lo"
hi_keyword:
        keyword "hi"
about_keyword:
        keyword "about"
        .byte 0 ; end marker

binary_store:
        .byte 0,0
print_binary:
        sta binary_store
        stx binary_store+1
        ldy #8
        php
        bcc @space_loop
        ldy #5
@space_loop:
        lda #' '
        jsr $ab47
        dey
        bne @space_loop
        lda #'%'
        jsr $ab47

        ldy #6
        plp
        bcc @loop
        lda binary_store+1
        ldy #9
        lsr
        bpl @ninthbit
@loop:
        asl binary_store
@ninthbit:
        lda #0
        adc #'0'
        jsr $ab47
        dey
        bne @loop
        rts


print_reg_name:
        tay
        lda #' '
        jsr $ab47
        lda #':'
        jsr $ab47

        cpy #$50-3      ; (final 3 registers assumed to be unknown)
        bcc valid_reg_name
        ldy #$2f
valid_reg_name:
        lda register_table_lo,y
        sta src
        lda register_table_hi,y
        sta src+1

        ldy #0
@loop:
        lda (src),y
        iny
        pha
        and #$7f
        jsr $ab47
        pla
        bpl @loop
@padding_loop:
        cpy #$09
        beq @done
        lda #' '
        jsr $ab47
        iny
        bne @padding_loop
@done:
        rts

register_names:
        keyword "sp0x"
        keyword "sp0y"
        keyword "sp1x"
        keyword "sp1y"
        keyword "sp2x"
        keyword "sp2y"
        keyword "sp3x"
        keyword "sp3y"
        keyword "sp4x"
        keyword "sp4y"
        keyword "sp5x"
        keyword "sp5y"
        keyword "sp6x"
        keyword "sp6y"
        keyword "sp7x"
        keyword "sp7y"
        keyword "msigx"
        keyword "scroly"
        keyword "raster"
        keyword "lpenx"
        keyword "lpeny"
        keyword "spena"
        keyword "scrolx"
        keyword "yxpand"
        keyword "vmcsb"
        keyword "vicirq"
        keyword "irqmask"
        keyword "spbgpr"
        keyword "spmc"
        keyword "xxpand"
        keyword "spspcl"
        keyword "spbgcl"
        keyword "extcol"
        keyword "bgcol0"
        keyword "bgcol1"
        keyword "bgcol2"
        keyword "bgcol3"
        keyword "spmc0"
        keyword "spmc1"
        keyword "sp0col"
        keyword "sp1col"
        keyword "sp2col"
        keyword "sp3col"
        keyword "sp4col"
        keyword "sp5col"
        keyword "sp6col"
        keyword "sp7col"
        keyword "???"      ; XSCAN on C-128, not valid on C64
        keyword "???"      ; CLKRATE on C-128, not valid on C64
; VASYL registers begin here
        keyword "ctrl1"    ; $31
        keyword "dlistl"
        keyword "dlisth"
        keyword "adr0l"
        keyword "adr0h"
        keyword "step0"
        keyword "port0"
        keyword "adr1l"
        keyword "adr1h"
        keyword "step1"
        keyword "port1"
        keyword "rep0"
        keyword "rep1"
        keyword "dlstrobe"
        keyword "???"
        keyword "ctrl2"
        keyword "dlist2l"
        keyword "dlist2h"
        keyword "dl2strobe"
        keyword "sbasel"
        keyword "sbaseh"
        keyword "scycstart"
        keyword "scycstop"
        keyword "sstepl"
        keyword "ssteph"
        keyword "spaddingl"
        keyword "spaddingh"
        keyword "sxorbyte"
        .byte 0


keyword_adr_lo:
        .lobytes end_keyword, for_keyword, next_keyword, data_keyword
        .lobytes input2_keyword, input_keyword, dim_keyword, read_keyword
        .lobytes let_keyword, goto_keyword, run_keyword, if_keyword
        .lobytes restore_keyword, gosub_keyword, return_keyword, rem_keyword
        .lobytes stop_keyword, on_keyword, wait_keyword, load_keyword
        .lobytes save_keyword, verify_keyword, def_keyword, poke_keyword
        .lobytes print2_keyword, print_keyword, cont_keyword, list_keyword
        .lobytes clr_keyword, cmd_keyword, sys_keyword, open_keyword
        .lobytes close_keyword, get_keyword, new_keyword, tab_keyword
        .lobytes to_keyword, fn_keyword, spc_keyword, then_keyword, not_keyword
        .lobytes step_keyword, plus_keyword, minus_keyword, asterisk_keyword
        .lobytes slash_keyword, apostrophe_keyword, and_keyword, or_keyword
        .lobytes less_keyword, equal_keyword, more_keyword, sgn_keyword
        .lobytes int_keyword, abs_keyword, usr_keyword, fre_keyword
        .lobytes pos_keyword, sqr_keyword, rnd_keyword, log_keyword
        .lobytes exp_keyword, cos_keyword, sin_keyword, tan_keyword
        .lobytes atn_keyword, peek_keyword, len_keyword, str_keyword
        .lobytes val_keyword, asc_keyword, chr_keyword, left_keyword
        .lobytes right_keyword, mid_keyword, go_keyword, vlist_keyword
        .lobytes vpoke_keyword, vdpoke_keyword, copy_keyword
        .lobytes dpoke_keyword, vcfg_keyword, vdata_keyword, label_keyword
        .lobytes dlon_keyword, dloff_keyword
        .lobytes vdelayh_keyword, vdelayv_keyword
        .lobytes vwaitbad_keyword, vnop_keyword
        .lobytes vskip_keyword, virq_keyword, vdeca_keyword, vdecb_keyword, vbadline_keyword
        .lobytes vmov_keyword, vwaitrep_keyword, vend_keyword
        .lobytes vsetmaskh_keyword, vsetmaskv_keyword, vsetmaskph_keyword, vsetmaskpv_keyword
        .lobytes vseta_keyword, vsetb_keyword
        .lobytes vxfer_keyword, bank_keyword, vwait_keyword, vbra_keyword, racer_keyword
        .lobytes dpeek_keyword, vdpeek_keyword, vpeek_keyword
        .lobytes lo_keyword, hi_keyword
        .lobytes about_keyword

keyword_adr_hi:
        .hibytes end_keyword, for_keyword, next_keyword, data_keyword
        .hibytes input2_keyword, input_keyword, dim_keyword, read_keyword
        .hibytes let_keyword, goto_keyword, run_keyword, if_keyword
        .hibytes restore_keyword, gosub_keyword, return_keyword, rem_keyword
        .hibytes stop_keyword, on_keyword, wait_keyword, load_keyword
        .hibytes save_keyword, verify_keyword, def_keyword, poke_keyword
        .hibytes print2_keyword, print_keyword, cont_keyword, list_keyword
        .hibytes clr_keyword, cmd_keyword, sys_keyword, open_keyword
        .hibytes close_keyword, get_keyword, new_keyword, tab_keyword
        .hibytes to_keyword, fn_keyword, spc_keyword, then_keyword, not_keyword
        .hibytes step_keyword, plus_keyword, minus_keyword, asterisk_keyword
        .hibytes slash_keyword, apostrophe_keyword, and_keyword, or_keyword
        .hibytes less_keyword, equal_keyword, more_keyword, sgn_keyword
        .hibytes int_keyword, abs_keyword, usr_keyword, fre_keyword
        .hibytes pos_keyword, sqr_keyword, rnd_keyword, log_keyword
        .hibytes exp_keyword, cos_keyword, sin_keyword, tan_keyword
        .hibytes atn_keyword, peek_keyword, len_keyword, str_keyword
        .hibytes val_keyword, asc_keyword, chr_keyword, left_keyword
        .hibytes right_keyword, mid_keyword, go_keyword, vlist_keyword
        .hibytes vpoke_keyword, vdpoke_keyword, copy_keyword
        .hibytes dpoke_keyword, vcfg_keyword, vdata_keyword, label_keyword
        .hibytes dlon_keyword, dloff_keyword
        .hibytes vdelayh_keyword, vdelayv_keyword
        .hibytes vwaitbad_keyword, vnop_keyword
        .hibytes vskip_keyword, virq_keyword, vdeca_keyword, vdecb_keyword, vbadline_keyword
        .hibytes vmov_keyword, vwaitrep_keyword, vend_keyword
        .hibytes vsetmaskh_keyword, vsetmaskv_keyword, vsetmaskph_keyword, vsetmaskpv_keyword
        .hibytes vseta_keyword, vsetb_keyword
        .hibytes vxfer_keyword, bank_keyword, vwait_keyword, vbra_keyword, racer_keyword
        .hibytes dpeek_keyword, vdpeek_keyword, vpeek_keyword
        .hibytes lo_keyword, hi_keyword
        .hibytes about_keyword

function_ptrs:
; Original BASIC V2 commands and functions.
; Just for reference.
;
;        .word $A830 ;                    perform END     $80
;        .word $A741 ;                    perform FOR     $81
;        .word $AD1D ;                    perform NEXT    $82
;        .word $A8F7 ;                    perform DATA    $83
;        .word $ABA4 ;                    perform INPUT#  $84
;        .word $ABBE ;                    perform INPUT   $85
;        .word $B080 ;                    perform DIM     $86
;        .word $AC05 ;                    perform READ    $87
;        .word $A9A4 ;                    perform LET     $88
;        .word $A89F ;                    perform GOTO    $89
;        .word $A870 ;                    perform RUN     $8A
;        .word $A927 ;                    perform IF      $8B
;        .word $A81C ;                    perform RESTORE $8C
;        .word $A882 ;                    perform GOSUB   $8D
;        .word $A8D1 ;                    perform RETURN  $8E
;        .word $A93A ;                    perform REM     $8F
;        .word $A82E ;                    perform STOP    $90
;        .word $A94A ;                    perform ON      $91
;        .word $B82C ;                    perform WAIT    $92
;        .word $E167 ;                    perform LOAD    $93
;        .word $E155 ;                    perform SAVE    $94
;        .word $E164 ;                    perform VERIFY  $95
;        .word $B3B2 ;                    perform DEF     $96
;        .word $B823 ;                    perform POKE    $97
;        .word $AA7F ;                    perform PRINT#  $98
;        .word $AA9F ;                    perform PRINT   $99
;        .word $A856 ;                    perform CONT    $9A
;        .word $A69B ;                    perform LIST    $9B
;        .word $A65D ;                    perform CLR     $9C
;        .word $AA85 ;                    perform CMD     $9D
;        .word $E129 ;                    perform SYS     $9E
;        .word $E1BD ;                    perform OPEN    $9F
;        .word $E1C6 ;                    perform CLOSE   $A0
;        .word $AB7A ;                    perform GET     $A1
;        .word $A641 ;                    perform NEW     $A2
;
;;                *** action addresses for functions
;        .word $BC39 ;                    perform SGN     $B4
;        .word $BCCC ;                    perform INT     $B5
;        .word $BC58 ;                    perform ABS     $B6
;        .word $0310 ;                    perform USR     $B7
;        .word $B37D ;                    perform FRE     $B8
;        .word $B39E ;                    perform POS     $B9
;        .word $BF71 ;                    perform SQR     $BA
;        .word $E097 ;                    perform RND     $BB
;        .word $B9EA ;                    perform LOG     $BC
;        .word $BFED ;                    perform EXP     $BD
;        .word $E264 ;                    perform COS     $BE
;        .word $E26B ;                    perform SIN     $BF
;        .word $E2B4 ;                    perform TAN     $C0
;        .word $E30E ;                    perform ATN     $C1
;        .word $B80D ;                    perform PEEK    $C2
;        .word $B77C ;                    perform LEN     $C3
;        .word $B465 ;                    perform STR$    $C4
;        .word $B7AD ;                    perform VAL     $C5
;        .word $B78B ;                    perform ASC     $C6
;        .word $B6EC ;                    perform CHR$    $C7
;        .word $B700 ;                    perform LEFT$   $C8
;        .word $B72C ;                    perform RIGHT$  $C9
;        .word $B737 ;                    perform MID$    $CA
;
;;              *** precedence byte and action addresses for operators
;;              like the primary commands these are called by pushing the address onto the stack
;;              and doing an RTS, so again the actual address -1 needs to be pushed
;        .word $6979 ; B8                 +
;        .word $5279 ; B8                 -
;        .word $2A7B ; BA                 *
;        .word $117B ; BB                 /
;        .word $7A7F ; BF                 ^
;        .word $E850 ; AF                 AND
;        .word $E546 ; AF                 OR
;        .word $B37D ; BF                 >
;        .word $D35A ; AE                 =
;        .word $1564 ; B0                 <

vb_functions:
        .word VLIST_IMPL - 1    ; $CC
        .word VPOKE_IMPL - 1    ; $CD
        .word VDPOKE_IMPL - 1   ; $CE
        .word COPY_IMPL - 1     ; $CF
        .word DPOKE_IMPL - 1    ; $D0
        .word VCFG_IMPL - 1     ; $D1
        .word VDATA_IMPL - 1    ; $D2
        .word LABEL_IMPL - 1    ; $D3
        .word DLON_IMPL - 1     ; $D4
        .word DLOFF_IMPL - 1    ; $D5
        .word VDELAYH_IMPL      ; $D6
        .word VDELAYV_IMPL      ; $D7
no_op_functions:
        .word VWAITBAD_IMPL     ; $D8
        .word VNOP_IMPL         ; $D9
        .word VSKIP_IMPL        ; $DA
        .word VIRQ_IMPL         ; $DB
        .word VDECA_IMPL        ; $DC
        .word VDECB_IMPL        ; $DD
        .word VBADLINE_IMPL     ; $DE
        .word VMOV_IMPL         ; $DF
        .word VWAITREP_IMPL     ; $E0
        .word VEND_IMPL         ; $E1
vsetmaskh_func:
        .word VMASK_IMPL        ; $E2
vsetmaskv_func:
        .word VMASK_IMPL        ; $E3
vsetmaskph_func:
        .word VMASK_IMPL        ; $E4
vsetmaskpv_func:
        .word VMASK_IMPL        ; $E5
vseta_func:
        .word VSETA_IMPL        ; $E6
vsetb_func:
        .word VSETB_IMPL        ; $E7
        .word VXFER_IMPL        ; $E8
        .word BANK_IMPL         ; $E9
        .word VWAIT_IMPL        ; $EA
        .word VBRA_IMPL         ; $EB
        .word RACER_IMPL        ; $EC

        .word DPEEK_IMPL        ; $ED   ; first function-only keyword
        .word VDPEEK_IMPL       ; $EE
        .word VPEEK_IMPL        ; $EF
        .word LO_IMPL           ; $F0
        .word HI_IMPL           ; $F1
        .word ABOUT_IMPL        ; $F2

highmem_data_end:

ampersand_flag:
        .byte 0
racer_mode:
        .byte 2
register_table_lo:
        .res $51    ; +1 extra byte for end marker
register_table_hi:
        .res $51
char_counter:
        .res 1
firstarg_lo:
        .res 1
firstarg_hi:
        .res 1
function_flag:
        .res 1
regtest_finite:
        .res 1
value:
        .res 4
digit_oddness:
        .res 1
byte_count:
        .res 1
skip_lz_flag:   ; leading zeros
        .res 1

copier_adr:
        .res 2
copier_step:
        .res 1
copier_backup:
        .res 3
register_mask:
        .res 1
vend_flag:
        .res 1
current_opcode:
        .res 1
second_byte:
        .res 1
current_mode:
        .res 1
two_args_flag:
        .res 1
current_function_index:
        .res 1
copy_to_copier:
        .res 1
copy_from_copier:
        .res 1
selected_bank:
        .res 1
copier_saver:
        .res 1
mask_or_delay:
        .res 1
; compute the number of bytes in HIGHCODE segment rounded up to page size
__HIGHMEM_SIZE = ((* - highmem_data_start) + 255) & $ff00;
        .out .sprintf("__HIGHMEM_SIZE = %04x", __HIGHMEM_SIZE)

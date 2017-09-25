;;Steve's NES demo for Thomas

    .inesprg 2
    .ineschr 1
    .inesmap 0
    .inesmir 1
    
    .include "muse-flags.inc"
    
    .rsset 0    
; 7 bytes of zeropage.
MUSE_ZEROPAGE   .rs 7   ;00
joypad1 .rs 1           ;07button states for the current frame
joypad1_old .rs 1       ;08last frame's button states
joypad1_pressed .rs 1   ;09current frame's off_to_on transitions
sleeping .rs 1          ;0A main program sets this and waits for the NMI to clear it.  Ensures the main program is run only once per frame.  
                        ;   for more information, see Disch's document: http://nesdevhandbook.googlepages.com/theframe.html
needdraw .rs 1          ;0B drawing flag.
dbuffer_index .rs 1     ;0C current position in the drawing buffer  not implemented yet
ptr1 .rs 2              ;0D a pointer
noteVisuals .rs 1       ;0F
sineCounter .rs 1       ;10
sineValue   .rs 1       ;11
nmiTimer    .rs 1       ;12
heartTimer  .rs 1       ;13
yPos        .rs 1       ;14

  .rsset $200
spriteRam    .rs 256

  .rsset $300
; 256 bytes of RAM.

MUSE_RAM        .rs 256

    .bank 0
    .org $8000

reset:
 SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs
 
vblankwait1:       ; First wait for vblank to make sure PPU is ready
  BIT $2002
  BPL vblankwait1
 
clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0200, x
  INX
  BNE clrmem

;;Set Default values for debuging
  LDA #$66
  STA noteVisuals
  LDA #$67
  STA sineCounter
  LDA #$68
  STA sineValue
  LDA #$69
  STA nmiTimer
  LDA #$0
  STA heartTimer
  LDA $14
  STA yPos

vblankwait2:      ; Second wait for vblank, PPU is ready after this
  BIT $2002
  BPL vblankwait2
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   


LoadPalettes:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006             ; write the high byte of $3F00 address
  LDA #$00
  STA $2006             ; write the low byte of $3F00 address
  LDX #$00              ; start out at 0
LoadPalettesLoop:
  LDA palette, x        ; load data from address (palette + the value in x)
                          ; 1st time through loop it will load palette+0
                          ; 2nd time through loop it will load palette+1
                          ; 3rd time through loop it will load palette+2
                          ; etc
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20            ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites<- lol wut? It's CPXing to $20, but says $10. �\(�_O)/�
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down
 
 
 
LoadSprites:
  LDX #0              ; start at 0
LoadSpritesLoop:
  LDA sprites, x        ; load data from address (sprites +  x)
  STA spriteRam, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$38             ; Compare X to hex $10, decimal 16
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
                        ; if compare was equal to 16, keep going down
             
             
             
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address
  LDX #$00              ; start out at 0

LoadBackground1:
  LDA background1, x     ; load data from address (background + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$00              ; Compare X to hex $00, decimal 256 - copying 256 bytes
  BNE LoadBackground1  ; Branch to LoadBackgroundXLoop if compare was Not Equal to zero
                        ; if compare was equal to 256, keep going down
LoadBackground2:
  LDA background2, x     ; load data from address (background + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$00              ; Compare X to hex $00, decimal 256 - copying 256 bytes
  BNE LoadBackground2  ; Branch to LoadBackgroundXLoop if compare was Not Equal to zero
                        ; if compare was equal to 256, keep going down
LoadBackground3:
  LDA background3, x     ; load data from address (background + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$00              ; Compare X to hex $00, decimal 256 - copying 256 bytes
  BNE LoadBackground3  ; Branch to LoadBackgroundXLoop if compare was Not Equal to zero
                        ; if compare was equal to 256, keep going down
LoadBackground4:
  LDA background4, x     ; load data from address (background + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$00              ; Compare X to hex $00, decimal 256 - copying 256 bytes
  BNE LoadBackground4  ; Branch to LoadBackgroundXLoop if compare was Not Equal to zero
                        ; if compare was equal to 256, keep going down 
LoadAttribute:
LoadAttributeLoop:
  LDA attribute, x      ; load data from address (attribute + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$60              ; Compare X to hex $20, decimal 32 - copying 32 bytes
  BNE LoadAttributeLoop  ; Branch to LoadAttributeLoop if compare was Not Equal to zero
                        ; if compare was equal to 128, keep going down
 
 
  LDA $2002              ; Reset Scroll
  LDA #$00
  STA $2005
  STA $2005
  STA $2006
  STA $2006     

    ; Initialize MUSE.
    lda #LOW(snd_data)
    ldx #HIGH(snd_data)
    jsr MUSE_init
    
    ; Load the song.
    lda #0
    jsr MUSE_startMusic
    
    ; MUSE_init sets the MUSE_PAUSE flag by default, so clear that now.
    ; I'm also setting the MUSE_NTSC_MODE flag here for demonstration purposes.
    ; In a real program you should detect the system and set the flag based on that.
    lda #MUSE_NTSC_MODE
    jsr MUSE_setFlags
    
    JSR resetSineCounter

;;begin graphics             
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
 
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side, turn on ppu
  STA $2001
  

inf:
    jmp inf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NMI:
  PHA             ;push accumulator on stack
  TXA             ;move x to accumulator
  PHA             ;push x to stack
  TYA             ;move y to accumulator
  PHA             ;push y to stack

  ;;Increment nmiTimer
  LDA nmiTimer
  ADC #1
  STA nmiTimer
  CMP #1            ;increasing slows sine wave
  BNE .dontJump
  JSR advanceSineCounter
  JSR advanceHeartTimer


.dontJump     ;just keep going, just a hardware limitation work around

  JSR updateNoteVisuals
 ;do sprite DMA
updateSprites:
  LDA #$00
  STA $2003       ; set the low byte (00) CHR-Rom?
  LDA #$02
  STA $4014       ; set the high byte (02) of the chr-rom? address, start the transfer

   ;;update the sprites after drawing old ones
  LDY #0
  LDX #0

;;;;Draw Thomas
.loop:
  TYA
  CMP #24        ;;
  BEQ .drawMario
  LDX sineCounter
  LDA sinewave, X
  LSR A
  CLC
  STA sineValue
  STA spriteRam, y
  STA yPos

  ;;Change the RAM

    ;;increment byte counter
  TYA 
  ADC #4
  TAY
  JMP .loop

  ;;;;draw Mario

.drawMario:
  TYA
  CMP #40
  BEQ drawHeart1
  LDA sprites, y
  ADC noteVisuals
  STA spriteRam, y
  TYA
  ADC #4
  TAY
  JMP .drawMario

drawHeart1:
  TYA
  CMP #56
  BEQ done

;update vertical position when the heartTimer begins
  LDA heartTimer
  ;CMP #0
  ;BEQ .spawnY

  LDA sprites, y            ;test from mario
  ;ADC noteVisuals
  ADC sineValue
  STA spriteRam, y

;;access the horizontal position byte
.continue:
  TYA
  ADC #3
  TAY
;;update horizontal data with heartTimer  
  LDA sprites, y
  ADC heartTimer
  ADC #$28                            ;horizontal offset right
  STA spriteRam, y
  
  TYA
  ADC #1
  TAY
  JMP drawHeart1

.spawnY:
  ;LDA sprites, y
  ;ADC sineValue
  ;STA spriteRam, y

 ;JSR advanceHeartTimer
  jmp .continue
  ;TYA
  ;ADC #4
  ;TAY
  ;JMP drawHeart1


done:
  LDA #1
  STA needdraw
  LDA needdraw
  beq .drawing_done   ;if drawing flag is clear, skip drawing
  lda $2002           ;else, draw
  ;;jsr draw_buffer     ;not implemented
  lda #$00
  sta needdraw        ;done drawing, so clear drawing flag

.drawing_done:
  LDA #$00
  STA $2005
  STA $2005   ;set scroll

; .include "joypads.asm"



    ; Update the music (sound effects not used in this demo).
  JSR MUSE_update

  LDA #$00
  STA sleeping    
       
  PLA             ;restore registers
  TYA             ;restore A
  PLA             ;pop old X to A
  TAX             ;restore X
  PLA
  RTI              ;end NMI, turn off interrupt flag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
irq:
    jmp irq
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General data.
    .include "snd-data/snd-data.asm"
; Song data.
    .include "snd-data/snd-data-happybirthday.asm"
; No sound effect data.


 
draw_buffer:
  LDA #$00
  STA $2003       ; set the low byte (00) CHR-Rom?
  LDA #$02
  STA $4014       ; set the high byte (02) of the chr-rom? address, start the transfer
  RTS

updateNoteVisuals:
  LDA $03E5
  LSR A 
  LSR A
  STA noteVisuals 
  RTS

advanceHeartTimer:
  LDA heartTimer
  CMP #$80
  BEQ .resetHeartTimer
  ADC #1
  STA heartTimer
  RTS

.resetHeartTimer:
  LDA #0
  STA heartTimer
  RTS

advanceSineCounter:
  LDA #0
  STA nmiTimer
  LDA sineCounter
  CMP #255
  BEQ resetSineCounter
  ADC #1
  STA sineCounter
  RTS

resetSineCounter:
  LDA #0
  STA sineCounter
  RTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    .bank 1
    .org $A000
; MUSE library requires ~8K of space, and must be aligned to a 256 byte page.
    .include "muse-nesasm.inc"
    
    .bank 2
    .org $C000
; DPCM data (must be at $C000-FFFF and aligned to 64 bytes).
    .include "snd-data/snd-data-dpcm.asm"

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;  ;----- fourth 8k bank of PRG-ROM    
  .bank 3
  .org $E000
palette:
  .db $22,$29,$1A,$0F,  $22,$36,$17,$0F,  $22,$30,$21,$0F,  $22,$27,$17,$0F   ;;background palette
  .db $22,$12,$36,$15,  $22,$15,$35,$25,  $22,$1C,$15,$14,  $22,$02,$38,$3C   ;;sprite palette
 
sprites:
     ;vert tile attr horiz

     ;;thomas
  .db $80, $B0, $00, $10 ;'t' 4
  .db $80, $B1, $00, $18 ;'h' 5
  .db $80, $B2, $00, $20 ;'o' 6
  .db $80, $B3, $00, $28 ;'m' 7
  .db $80, $B4, $00, $30 ;'a' 8
  .db $80, $B5, $00, $38 ;'s' 9

   ;;mario 
  .db $80, $32, $00, $80   ;sprite 0
  .db $80, $33, $00, $88   ;sprite 1
  .db $88, $34, $00, $80   ;sprite 2
  .db $88, $35, $00, $88   ;sprite 3

  ;;heart
  .db $80, $BA, %00000001, $00
  .db $88, $CA, %00000001, $00
  .db $80, $BA, %01000001, $08 
  .db $88, $CA, %01000001, $08
 
 
background1:
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 2
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$36,$37,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 2
  .db $24,$24,$24,$24,$24,$24,$24,$35,$25,$25,$38,$24,$24,$24,$24,$24  ;;all sky
 
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 3
  .db $24,$24,$24,$24,$24,$24,$24,$39,$3A,$3B,$3C,$24,$24,$24,$24,$24  ;;some brick tops
 
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 4
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;brick bottoms

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
 
background2:
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$11,$0A,$19,$19,$22,$24  ;; Happy 
  .db $0B,$12,$1B,$1D,$11,$0D,$0A,$22,$2B,$24,$24,$24,$24,$24,$24,$24   ;;Birthday
 
  .db $24,$24,$36,$37,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $24,$35,$25,$25,$38,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $24,$39,$3A,$3B,$3C,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
 
background3:
  .db $24,$24,$24,$24,$4c,$4d,$4e,$4f,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $24,$24,$24,$24,$5c,$5d,$5e,$5f,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 2
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $24,$24,$24,$24,$6c,$6d,$6e,$6f,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$36,$37,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $24,$24,$24,$24,$7c,$7d,$7e,$7f,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 2
  .db $24,$24,$24,$24,$24,$24,$24,$35,$25,$25,$38,$24,$24,$24,$24,$24  ;;all sky
 
  .db $24,$24,$24,$24,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$24,$24  ;;row 3
  .db $24,$24,$24,$24,$24,$24,$24,$39,$3A,$3B,$3C,$24,$24,$24,$24,$24  ;;some brick tops
 
  .db $24,$24,$24,$24,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 4
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;brick bottoms

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
 
 
background4:
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
 
attribute:
  .db %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010
  .db %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010

  .db %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010
  .db %10101010, %10101010, %01010101, %01010101, %10101010, %10101010, %10101010, %10101010

  .db %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010
  .db %01010101, %01010101, %01010101, %01010101, %10101010, %10101010, %10101010, %10101010

  .db %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
 
  sinewave:
 .db $64,$66,$69,$6b,$6e,$70,$73,$75
 .db $78,$7a,$7c,$7f,$81,$83,$86,$88
 .db $8a,$8d,$8f,$91,$93,$95,$97,$99
 .db $9c,$9e,$a0,$a2,$a3,$a5,$a7,$a9
 .db $ab,$ac,$ae,$b0,$b1,$b3,$b4,$b6
 .db $b7,$b8,$ba,$bb,$bc,$bd,$be,$bf
 .db $c0,$c1,$c2,$c3,$c4,$c4,$c5,$c6
 .db $c6,$c7,$c7,$c7,$c8,$c8,$c8,$c8
 .db $c8,$c8,$c8,$c8,$c8,$c7,$c7,$c7
 .db $c6,$c6,$c5,$c4,$c4,$c3,$c2,$c1
 .db $c0,$bf,$be,$bd,$bc,$bb,$ba,$b8
 .db $b7,$b6,$b4,$b3,$b1,$b0,$ae,$ac
 .db $ab,$a9,$a7,$a5,$a3,$a2,$a0,$9e
 .db $9c,$99,$97,$95,$93,$91,$8f,$8d
 .db $8a,$88,$86,$83,$81,$7f,$7c,$7a
 .db $78,$75,$73,$70,$6e,$6b,$69,$66
 .db $64,$62,$5f,$5d,$5a,$58,$55,$53
 .db $50,$4e,$4c,$49,$47,$45,$42,$40
 .db $3e,$3b,$39,$37,$35,$33,$31,$2f
 .db $2c,$2a,$28,$26,$25,$23,$21,$1f
 .db $1d,$1c,$1a,$18,$17,$15,$14,$12
 .db $11,$10,$0e,$0d,$0c,$0b,$0a,$09
 .db $08,$07,$06,$05,$04,$04,$03,$02
 .db $02,$01,$01,$01,$00,$00,$00,$00
 .db $00,$00,$00,$00,$00,$01,$01,$01
 .db $02,$02,$03,$04,$04,$05,$06,$07
 .db $08,$09,$0a,$0b,$0c,$0d,$0e,$10
 .db $11,$12,$14,$15,$17,$18,$1a,$1c
 .db $1d,$1f,$21,$23,$25,$26,$28,$2a
 .db $2c,$2f,$31,$33,$35,$37,$39,$3b
 .db $3e,$40,$42,$45,$47,$49,$4c,$4e
 .db $50,$53,$55,$58,$5a,$5d,$5f,$62

 
;;;;;;;;;;;;;;  vectors ;;;;;;;;;;;;;;;
 
    
    
  .org $FFFA
  .dw NMI
  .dw reset
  .dw irq

    ;;;;;;;;;;;;;;;;

  .bank 4     ;chr ROM
  .org $0000
  .incbin "mario.chr"   ;includes 8KB graphics file from SMB1

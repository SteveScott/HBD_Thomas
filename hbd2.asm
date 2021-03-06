; Simplest possible example of using MUSE with NESASM.

    .inesprg 2
    .ineschr 1
    .inesmap 0
    .inesmir 1
    
    .include "muse-flags.inc"
    
    .rsset 0    
; 7 bytes of zeropage.
MUSE_ZEROPAGE   .rs 7
joypad1 .rs 1           ;button states for the current frame
joypad1_old .rs 1       ;last frame's button states
joypad1_pressed .rs 1   ;current frame's off_to_on transitions
sleeping .rs 1          ;main program sets this and waits for the NMI to clear it.  Ensures the main program is run only once per frame.  
                        ;   for more information, see Disch's document: http://nesdevhandbook.googlepages.com/theframe.html
needdraw .rs 1          ;drawing flag.
dbuffer_index .rs 1     ;current position in the drawing buffer  not implemented yet
ptr1 .rs 2              ;a pointer

    .rsset $200
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
  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites<- lol wut? It's CPXing to $20, but says $10. �\(�_O)/�
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down
 
 
 
LoadSprites:
  LDX #0              ; start at 0
LoadSpritesLoop:
  LDA sprites, x        ; load data from address (sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$10              ; Compare X to hex $10, decimal 16
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
  CPX #$40              ; Compare X to hex $20, decimal 32 - copying 32 bytes
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
    
    
;;begin graphics             
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
 
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side, turn on ppu
  STA $2001
 

inf:
    jmp inf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
nmi:
    PHA             ;push accumulator on stack
    TXA             ;move x to accumulator
    PHA             ;push x to stack
    TYA             ;move y to accumulator
    PHA             ;push y to stack

    LDA #$00
    STA $2003       ; set the low byte (00) CHR-Rom?
    LDA #$02
    STA $4014       ; set the high byte (02) of the chr-rom? address, start the transfer

  ;do sprite DMA
  ;update pallets if needed
  ;draw stuff
  ;set scroll

  LDA #1
  STA needdraw
  LDA needdraw
  beq .drawing_done   ;if drawing flag is clear, skip drawing
  lda $2002           ;else, draw
  jsr draw_buffer     ;not implemented
  lda #$00
  sta needdraw        ;done drawing, so clear drawing flag

.drawing_done
  LDA #$00
  STA $2005
  STA $2005   ;set scroll

;; .include "joypads.asm"

    ; Update the music (sound effects not used in this demo).
    jsr MUSE_update

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
  .db $22,$12,$36,$15,  $22,$02,$38,$3C,  $22,$1C,$15,$14,  $22,$02,$38,$3C   ;;sprite palette
 
sprites:
     ;vert tile attr horiz
  .db $80, $32, $00, $80   ;sprite 0
  .db $80, $33, $00, $88   ;sprite 1
  .db $88, $34, $00, $80   ;sprite 2
  .db $88, $35, $00, $88   ;sprite 3
 
 
background1:
  .db $00,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $00,$01,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 2
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $00,$02,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$36,$37,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $00,$03,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 2
  .db $24,$24,$24,$24,$24,$24,$24,$35,$25,$25,$38,$24,$24,$24,$24,$24  ;;all sky
 
  .db $00,$04,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 3
  .db $24,$24,$24,$24,$24,$24,$24,$39,$3A,$3B,$3C,$24,$24,$24,$24,$24  ;;some brick tops
 
  .db $00,$05,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 4
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;brick bottoms

  .db $00,$06,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $00,$07,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
 
background2:
  .db $01,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $01,$01,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $01,$02,$24,$24,$24,$24,$24,$24,$24,$24,$11,$0A,$19,$19,$22,$24  ;; Happy 
  .db $0B,$12,$1B,$1D,$11,$0D,$0A,$22,$2B,$24,$24,$24,$24,$24,$24,$24   ;;Birthday
 
  .db $01,$03,$36,$37,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $01,$35,$25,$25,$38,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $01,$39,$3A,$3B,$3C,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $01,$06,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $01,$07,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
 
background3:
  .db $02,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $02,$01,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 2
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $02,$02,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$36,$37,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $02,$03,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 2
  .db $24,$24,$24,$24,$24,$24,$24,$35,$25,$25,$38,$24,$24,$24,$24,$24  ;;all sky
 
  .db $02,$04,$24,$24,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$24,$24  ;;row 3
  .db $24,$24,$24,$24,$24,$24,$24,$39,$3A,$3B,$3C,$24,$24,$24,$24,$24  ;;some brick tops
 
  .db $02,$05,$24,$24,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$24,$24  ;;row 4
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;brick bottoms

   .db $02,$06,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

   .db $02,$07,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
 
 
background4:
  .db $03,$00,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $03,$01,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $03,$02,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $03,$03,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky
 
  .db $03,$04,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;all sky

  .db $03,$05,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;;row 1
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
 
 
 
;;;;;;;;;;;;;;  vectors ;;;;;;;;;;;;;;;
 
    
    
    .org $FFFA
    .dw nmi
    .dw reset
    .dw irq

    ;;;;;;;;;;;;;;;;

    .bank 4     ;chr ROM
    .org $0000
    .incbin "mario.chr"   ;includes 8KB graphics file from SMB1

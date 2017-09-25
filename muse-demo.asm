; Simplest possible example of using MUSE with NESASM.

    .inesprg 2
    .ineschr 1
    .inesmap 0
    .inesmir 1
    
    .include "muse-flags.inc"
    
    .rsset 0    
; 7 bytes of zeropage.
MUSE_ZEROPAGE   .rs 7

    .rsset $200
; 256 bytes of RAM.
MUSE_RAM        .rs 256

    .bank 0
    .org $8000

reset:
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
    
    ; Enable NMI to start playing.
    lda #$80
    sta $2000

inf:
    jmp inf

nmi:
    ; Update the music (sound effects not used in this demo).
    jsr MUSE_update
    rti
    
irq:
    jmp irq
    
; General data.
    .include "snd-data/snd-data.asm"
; Song data.
    .include "snd-data/snd-data-happybirthday.asm"
; No sound effect data.

    .bank 1
    .org $A000
; MUSE library requires ~8K of space, and must be aligned to a 256 byte page.
    .include "muse-nesasm.inc"
    
    .bank 2
    .org $C000
; DPCM data (must be at $C000-FFFF and aligned to 64 bytes).
    .include "snd-data/snd-data-dpcm.asm"
    
    .bank 3
    .org $FFFA
    .word nmi
    .word reset
    .word irq

    .bank 4     ;chr ROM
    .org $0100
    .incbin "mario.chr"   ;includes 8KB graphics file from SMB1
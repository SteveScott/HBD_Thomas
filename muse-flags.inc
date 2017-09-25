; Flags for MUSE_setFlags.

; If set, music updates are paused.
MUSE_PAUSE_MUSIC             = %10000000
; If set, sound effect updates are paused.
MUSE_PAUSE_SFX               = %01000000
; If set, pauses both music and sound effects.
MUSE_PAUSE                   = MUSE_PAUSE_MUSIC|MUSE_PAUSE_SFX
; If set, global volume (MUSE_setVolume) doesn't apply to sound effects.
MUSE_GLOBAL_VOL_BEFORE_SFX   = %00100000
; If set, every 6th frame is skipped (for NTSC).
MUSE_SPEED_COMPENSATION      = %00010000
; If set, notes are tuned down 1 semitone, 28 cents (for NTSC/Dendy).
MUSE_NOTE_FREQ_COMPENSATION  = %00001000
; If set, speed and note frequencies are compensated for NTSC.
MUSE_NTSC_MODE               = MUSE_SPEED_COMPENSATION|MUSE_NOTE_FREQ_COMPENSATION
; If set, no speed/frequency compensation is done (the default).
MUSE_PAL_MODE                = 0
; If set, note frequencies are compensated for Dendy.
MUSE_DENDY_MODE              = MUSE_NOTE_FREQ_COMPENSATION

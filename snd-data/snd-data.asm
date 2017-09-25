snd_data:
  .word MUSE_L0
  .word MUSE_L1

MUSE_L0:
  .word MUSE_L2

MUSE_L1:

MUSE_L3:
  .word MUSE_L19

MUSE_L19:
  .word MUSE_L20 - 1
  .word MUSE_L21 - 1
  .word MUSE_L22 - 1
  .word MUSE_L23 - 1

MUSE_L20:
  .byte 240, $FF, 0
MUSE_L21:
  .byte 64, $FF, 0
MUSE_L22:
  .byte 128, $FF, 0
MUSE_L23:
  .byte 127, $FF, 0

MUSE_L4:


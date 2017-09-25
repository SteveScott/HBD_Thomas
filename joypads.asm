LatchController:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016     ;  tell both the controllers to latch buttons
 
ButtonA:
  LDA $4016       ; player 1 - A
  AND #%00000001  ; look only at bit 0
  BNE ButtonB
 
; Button stuff here
 
ButtonB:
  LDA $4016       ; player 1 - B
  AND #%00000001  ; look only at bit 0
  BNE SelectButton
 
; Button stuff here
 
SelectButton:
  LDA $4016       ; player 1 - Select
  AND #%00000001  ; look only at bit 0
  BNE StartButton
 
; Button stuff here
 
StartButton:
  LDA $4016       ; player 1 - Start
  AND #%00000001  ; look only at bit 0
  BNE UpButton
 
; Button stuff here
 
UpButton:
  LDA $4016       ; player 1 - Up
  AND #%00000001  ; look only at bit 0
  BNE DownButton
  LDA $0200       ; load sprite Y position
  CLC             ; make sure the carry flag is clear
  ADC #$01        ; A = A + 1
  STA $0200       ; save sprite Y position
  LDA $0204       ; load sprite Y position
  CLC             ; make sure the carry flag is clear
  ADC #$01        ; A = A + 1
  STA $0204       ; save sprite Y position
  LDA $0208       ; load sprite Y position
  CLC             ; make sure the carry flag is clear
  ADC #$01        ; A = A + 1
  STA $0208       ; save sprite Y position
  LDA $020C       ; load sprite Y position
  CLC             ; make sure the carry flag is clear
  ADC #$01        ; A = A + 1
  STA $020C       ; save sprite Y position
 
DownButton:
  LDA $4016       ; player 1 - Down
  AND #%00000001  ; look only at bit 0
  BNE LeftButton
  LDA $0200       ; load sprite Y position
  SEC             ; make sure carry flag is set
  SBC #$01        ; A = A - 1
  STA $0200       ; save sprite Y position
  LDA $0204       ; load sprite Y position
  SEC             ; make sure carry flag is set
  SBC #$01        ; A = A - 1
  STA $0204       ; save sprite Y position
  LDA $0208       ; load sprite Y position
  SEC             ; make sure carry flag is set
  SBC #$01        ; A = A - 1
  STA $0208       ; save sprite Y position
  LDA $020C       ; load sprite Y position
  SEC             ; make sure carry flag is set
  SBC #$01        ; A = A - 1
  STA $020C       ; save sprite Y position
 
LeftButton:
  LDA $4016       ; player 1 - Left
  AND #%00000001  ; look only at bit 0
  BNE RightButton
  LDA $0203       ; load sprite X position
  CLC             ; make sure the carry flag is clear
  ADC #$01        ; A = A + 1
  STA $0203       ; save sprite X position
  LDA $0207       ; load sprite X position
  CLC             ; make sure the carry flag is clear
  ADC #$01        ; A = A + 1
  STA $0207       ; save sprite X position
  LDA $020B       ; load sprite X position
  CLC             ; make sure the carry flag is clear
  ADC #$01        ; A = A + 1
  STA $020B       ; save sprite X position
  LDA $020F       ; load sprite X position
  CLC             ; make sure the carry flag is clear
  ADC #$01        ; A = A + 1
  STA $020F       ; save sprite X position
 
RightButton:
  LDA $4016       ; player 1 - Right
  AND #%00000001  ; look only at bit 0
  BNE NotRightButton
  LDA $0203       ; load sprite X position
  SEC             ; make sure carry flag is set
  SBC #$01        ; A = A - 1
  STA $0203       ; save sprite X position
  LDA $0207       ; load sprite X position
  SEC             ; make sure carry flag is set
  SBC #$01        ; A = A - 1
  STA $0207       ; save sprite X position
  LDA $020B       ; load sprite X position
  SEC             ; make sure carry flag is set
  SBC #$01        ; A = A - 1
  STA $020B       ; save sprite X position
  LDA $020F       ; load sprite X position
  SEC             ; make sure carry flag is set
  SBC #$01        ; A = A - 1
  STA $020F       ; save sprite X position
 
NotRightButton:
  JMP ButtonA

  RTI
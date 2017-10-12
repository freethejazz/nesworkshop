.include "constants.asm"
.include "header.asm"

.segment "ZEROPAGE"
sprite_x: .res 1
sprite_y: .res 1
sprite_v: .res 1  ; sprite's vertical movement direction
                  ; 0 for up, 1 for down
sprite_h: .res 1  ; sprite's horizontal movement direction
                  ; 0 for left, 1 for right
sprite_v_x: .res 1
sprite_v_y: .res 1
sprite_lh: .res 1
sprite_rh: .res 1

sprite_animation_step: .res 1
animation_counter: .res 1
ground_y: .res 1

was_pressing_a: .res 1
controller1: .res 1
temp_storage: .res 1

.segment "BSS"

.segment "CODE"
.proc irq_handler
  RTI
.endproc

.proc reset_handler
  SEI           ; turn on interrupts
  CLD           ; turn off non-existent decimal mode
  LDX #$00
  STX PPUCTRL   ; disable NMI
  STX PPUMASK   ; turn off display

vblankwait:     ; wait for PPU to fully boot up
  BIT PPUSTATUS
  BPL vblankwait

  JMP main
.endproc

.proc nmi_handler
  LDA #$00    ; draw SOMETHING first,
  STA OAMADDR ; in case we run out
  LDA #$02    ; of vblank time,
  STA OAMDMA  ; then update positions

  JSR read_controller
  JSR update_sprite_position
  JSR update_sprite_frame
  JSR draw_sprite
  ;JSR draw_ground

  RTI
.endproc

.proc main
  LDA #$70        ; set up initial sprite values
  STA sprite_x    ; these are stored in zeropage
  LDA #$30
  STA sprite_y
  LDA #$00
  STA sprite_v
  LDA #$00
  STA sprite_h
  LDA #$d9
  STA ground_y
  LDA #$00
  STA sprite_lh
  LDA #$00
  STA sprite_rh
  LDA #$00
  STA was_pressing_a

  LDA #$01
  STA sprite_animation_step

  LDA #$00
  STA animation_counter

  LDX PPUSTATUS   ; reset PPUADDR latch
  LDX #$3f
  STX PPUADDR
  LDX #$00
  STX PPUADDR     ; set PPU to write to $3f00 (palette ram)

copy_palettes:
  LDA palettes,x  ; use indexed addressing into palette storage
  STA PPUDATA
  INX
  CPX #$20          ; have we copied 32 values?
  BNE copy_palettes ; if no, repeat

vblankwait:       ; wait for another vblank before continuing
  BIT PPUSTATUS
  BPL vblankwait

  LDA #%10010000  ; turn on NMIs, sprites use first pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

forever:
  JMP forever     ; do nothing, forever
.endproc

.proc draw_sprite
  PHA ; store all registers in stack
  TXA ; this subroutine does not use
  PHA ; X or Y registers, so we don't
  TYA ; actually need to store/replace
  PHA ; them, just here as an example.
  PHP

  ; sprite data at $0200, $0204, $0208, $020c
  ; store y values first
  LDA sprite_y
  STA $0200
  STA $0204
  CLC
  ADC #$08
  STA $0208
  STA $020c

  ; store sprite tile numbers
  LDA sprite_lh
  CMP #$01
  BEQ lh_up

  LDA #$0c
  STA $0201
  JMP lf_done

  lh_up:
  LDA #$13
  STA $0201

  lf_done:

  LDA sprite_rh
  CMP #$01
  BEQ rh_up

  LDA #$0d
  STA $0205
  JMP rh_done

  rh_up:
  LDA #$12
  STA $0205

  rh_done:
  CLC
  LDA #$0e
  ADC sprite_animation_step
  STA $0209

  LDA #$11
  SEC
  SBC sprite_animation_step
  STA $020d

  ; store attributes
  LDA #%00000010
  STA $0202
  LDA #%00000010
  STA $0206
  LDA #%00000010
  STA $020a
  LDA #%00000010
  STA $020e

  ; store x values
  LDA sprite_x
  STA $0203
  STA $020b
  CLC
  ADC #$08
  STA $0207
  STA $020f

  PLP ; restore all registers from stack
  PLA ; again, X and Y registers never
  TAY ; changed, so some of this could
  PLA ; be removed.
  TAX
  PLA
  RTS
.endproc

.proc read_controller
  PHA
  TXA
  PHA
  PHP

  ; write a 1, then a 0, to CONT_PORT_1
  ; to latch button states
  LDA #$01
  STA CONT_PORT_1
  LDA #$00
  STA CONT_PORT_1

  LDA #$01
  STA controller1 ; move the '1' until done
                  ; (ring counter)
get_buttons:
  LDA CONT_PORT_1   ; read from controller port
  LSR A             ; shift accumulator bit 0 into carry flag
  ROL controller1   ; move carry flag into controller1
  BCC get_buttons   ; repeat until original '1' is moved to carry

  PLP
  PLA
  TAX
  PLA
  RTS
.endproc

.proc update_sprite_position
  PHA
  PHP

  LDA controller1
  AND #BTN_LEFT
  BEQ not_left_pressed

  LDA sprite_x
  SEC
  SBC #$02
  STA sprite_x
  JMP done_with_horizontal

not_left_pressed:
  LDA controller1
  AND #BTN_RIGHT
  BEQ done_with_horizontal

  LDA sprite_x
  CLC
  ADC #$02
  STA sprite_x

done_with_horizontal:
  LDA controller1
  AND #BTN_UP
  BEQ not_up_pressed

  LDA sprite_y
  SEC
  SBC #$02
  STA sprite_y
  JMP done_with_controller

not_up_pressed:
  LDA controller1
  AND #BTN_DOWN
  BEQ done_with_controller

  LDA sprite_y
  CLC
  ADC #$02
  STA sprite_y

done_with_controller:
  PLP
  PLA
  RTS
.endproc

.proc update_sprite_frame
  PHA
  PHP

  LDA controller1
  AND #BTN_B
  BEQ lh_down
  LDA #$01
  STA sprite_lh
  JMP done_lh

  lh_down:
  LDA #$00
  STA sprite_lh

  done_lh:

  LDA controller1
  AND #BTN_A
  BEQ rh_down
  LDA #$01
  STA sprite_rh
  JMP done_rh

  rh_down:
  LDA #$00
  STA sprite_rh

  done_rh:

  LDA controller1
  AND #%00001111
  BEQ no_animation_change

  INC animation_counter
  LDA animation_counter
  CMP #$06
  BNE no_animation_change

  LDA sprite_animation_step
  EOR #%00000001
  STA sprite_animation_step
  LDA #$00
  STA animation_counter

  no_animation_change:

  PLP
  PLA
  RTS
.endproc

.segment "RODATA"
palettes:
.byte $0c, $00, $10, $30
.byte $0c, $01, $21, $31
.byte $0c, $06, $16, $26
.byte $0c, $07, $08, $1a

.byte $0c, $00, $10, $30
.byte $0c, $01, $0f, $31
.byte $0c, $06, $16, $26
.byte $0c, $09, $19, $29

.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "CHR"
.incbin "sprites.chr"

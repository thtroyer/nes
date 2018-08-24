.setcpu "6502"

PPU_CTRL1 = $2000
PPU_CTRL2 = $2001
PPU_STATUS = $2002
PPU_SPR_ADDR = $2003
PPU_SPR_IO = $2004
PPU_VRAM_ADDR1 = $2005
PPU_VRAM_ADDR2 = $2006
PPU_VRAM_IO = $2007

APU_MODCTRL = $4010
APU_SPR_DMA = $4014

APU_PAD1 = $4016
APU_PAD2 = $4017

CRTL2_GRAYSCALE = %00000001
CRTL2_DISABLE_LEFT_BG_CLIP = %00000010
CRTL2_DISABLE_LEFT_SPR_CLIP = %00000100
CRTL2_BACKGROUND_RENDER = %00001000
CRTL2_SPRITE_RENDER = %00010000
CRTL2_INTENSE_RED = %00100000
CRTL2_INTENSE_GREEN = %01000000
CRTL2_INTENSE_BLUE = %10000000

CRTL1_NMI_ENABLED = %10000000
CRTL1_SPRITES_8x16 = %00100000
CRTL1_BG_PT_ADDR_1 = %00010000
CRTL1_SP_PT_ADDR_1 = %00001000
CRTL1_VRAM_INC = %00000100
CRTL1_BASE_NAMETABLE_2000 = %00000000
CRTL1_BASE_NAMETABLE_2400 = %00000001
CRTL1_BASE_NAMETABLE_2800 = %00000010
CRTL1_BASE_NAMETABLE_2C00 = %00000011

.segment "HEADER"
; the header!
.byte "NES", $1a ; ines header
.byte 1 ; PRG-ROM pages (16kb)
.byte $01 ; CHR-ROM pages (8kb)


; Zero page RAM
.segment "ZEROPAGE"
sprite_x: .res 1
sprite_y: .res 1
buttons: .res 1
pointerHigh: .res 1
pointerLow: .res 1


; General RAM
;.segment "BSS"


.segment "CODE"

ReadController:
  LDA #$01
  STA APU_PAD1 
  LDA #$00
  STA APU_PAD1 
  LDX #$08

ReadControllerLoop:
  LDA APU_PAD1 
  ; use bit shifting to pull lowest bit from controller and put into buttons
  LSR A
  ROL buttons
  DEX
  BNE ReadControllerLoop
  RTS

MoveSpritesUp:
  DEC sprite_y
  RTS
MoveSpritesDown:
  INC sprite_y
  RTS
MoveSpritesLeft:
  DEC sprite_x
  RTS
MoveSpritesRight:
  INC sprite_x
  RTS

SetSpritePositions:
  ; sprite positions relative to x and y
  LDA sprite_y
  STA $0200
  STA $0204
  STA $0208
  LDA sprite_x
  STA $0203
  ADC #$08
  STA $0207
  ADC #$08
  STA $020B
  RTS

CopySpritesToPpu:
  ;; copy sprites back to PPU as the PPU forgets them every cycle.
  ; write address $0200 to PPU for DMA sprite transfer
  LDA #$00
  STA PPU_SPR_ADDR
  LDA #$02
  STA APU_SPR_DMA 
  RTS

HandleControllerInput:
  JSR ReadController

  ;button bit order:
  ; A B select start up down left right
  LDA buttons
  AND #%00001000
  BNE MoveSpritesUp

  LDA buttons
  AND #%00000100
  BNE MoveSpritesDown

  LDA buttons
  AND #%00000010
  BNE MoveSpritesLeft

  LDA buttons
  AND #%00000001
  BNE MoveSpritesRight

  RTS

vblank_cycle:
  lda PPU_STATUS 
  bpl vblank_cycle
  rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Main program start
Reset:
  SEI
  CLD
  .repeat 3
  jsr vblank_cycle
  .endrep

  LDA #$80
  STA sprite_x
  STA sprite_y

  ;; Palettes start at PPU address $3F00 and $3F10. To set this address, PPU address port $2006 is used.
  ;;
  ;; This code tells the PPU to set its address to $3F10. Then the PPU data port at $2007 is ready to accept data. The first write will go to the
  ;; address you set ($3F10), then the PPU will automatically increment the address after each read or write.
LoadPalettes:
  ;read PPU status to reset the high/low latch to high
  LDA PPU_STATUS 
  ;write 3F00 to set its address there
  LDA #$3F
  STA PPU_VRAM_ADDR2 
  LDA #$00
  STA PPU_VRAM_ADDR2 

  LDX #$00
LoadPalettesLoop:
  ; loop through palettes and write to PPU ($2007)
  ; 32 bytes (0x20)
  LDA Palettes, x
  STA PPU_VRAM_IO 
  INX
  CPX #$20
  BNE LoadPalettesLoop

LoadSprites:
  LDX #$00
LoadSpritesLoop:
  LDA Sprites, x
  STA $0200, x
  INX
  CPX #$10
  BNE LoadSpritesLoop

LoadBackground:
  LDA PPU_STATUS
  ; write $2000 address to $2006
  LDA #$20
  STA $2006
  LDA #$00
  STA $2006

  LDA #>Background
  STA pointerLow
  LDA #<Background 
  STA pointerHigh

; Need to copy 930 (0x3A2) bytes to PPU
; 3*255(0xFF) with 165 (0xA5) remainder
  LDX #$00
@OUTER_LOOP:
  CPX #$03
  BCS @LAST_LOOP
  LDY #$00
@INNER_LOOP:
  LDA (pointerHigh), Y
  STA PPU_VRAM_IO 
  INY
  BNE @INNER_LOOP
  INX
  INC pointerLow
  JMP @OUTER_LOOP
@LAST_LOOP:
  LDA (pointerHigh), Y
  STA PPU_VRAM_IO 
  INY
  CPY #$A5
  BCC @LAST_LOOP

SetupPpu:
  JSR PPU_SETUP
  JSR EnableSprites

GameLoop:
  JMP GameLoop

PPU_SETUP:
  ;LDA #%00000000
  ;ORA CRTL1_NMI_ENABLED
  ;;ORA CRTL1_SPRITES_8x16
  ;ORA CRTL1_BG_PT_ADDR_1
  ;;ORA CRTL1_SP_PT_ADDR_1
  ;;ORA CRTL1_VRAM_INC
  ;;ORA CRTL1_BASE_NAMETABLE_2000
  ;;ORA CRTL1_BASE_NAMETABLE_2400
  ;;ORA CRTL1_BASE_NAMETABLE_2800
  ;;ORA CRTL1_BASE_NAMETABLE_2C00
  LDA #%10010000
  STA PPU_CTRL1 
  RTS

EnableSprites:
  ;LDA #%00000000
  ;;ORA CRTL2_GRAYSCALE
  ;ORA CRTL2_DISABLE_LEFT_BG_CLIP
  ;ORA CRTL2_DISABLE_LEFT_SPR_CLIP
  ;ORA CRTL2_BACKGROUND_RENDER
  ;ORA CRTL2_SPRITE_RENDER
  ;;ORA CRTL2_INTENSE_RED
  ;;ORA CRTL2_INTENSE_GREEN
  ;;ORA CRTL2_INTENSE_BLUE
  ;;LDA #%00011110
  ;STA PPU_CTRL2 
  ;RTS
  LDA #%00011110
  STA PPU_CTRL2

PpuCleanup:

VBlank:
  JSR HandleControllerInput
  JSR SetSpritePositions
  JSR CopySpritesToPpu
  JSR PPU_SETUP
  JSR EnableSprites

  ;;This is the PPU clean up section, so rendering the next frame starts properly.
  ;LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  ;STA PPU_CTRL1 
  ;LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  ;STA PPU_CTRL2
  LDA #$00        ;;tell the ppu there is no background scrolling
  STA PPU_VRAM_ADDR1 
  STA PPU_VRAM_ADDR1 

  RTI

Dummy:
  RTI

Background:
  .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .byte $10,$40,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$41,$10
  .byte $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byte $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byte $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byte $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byte $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byte $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byte $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byte $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byte $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byte $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byte $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byte $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byte $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byte $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byte $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byte $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byte $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byte $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byte $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byte $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byte $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byte $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byte $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byte $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byte $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byte $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byte $10,$43,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$42,$10
  .byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

attribute:
  .byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
  .byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
  .byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
  .byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
  .byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
  .byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
  .byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
  .byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
  .byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
  .byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
  .byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
  .byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
  .byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
  .byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
  .byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55

Palettes:
  ; each palette consists of 4 bytes, starting with $0F (transparent background)
  ; colors are defined in the PPU and are looked up by bytes. No RGB here.

  ; background
  .byte $0F,$2D,$3D,$30,$0F,$2D,$3D,$30,$0F,$2D,$3D,$30,$0F,$2D,$3D,$30
  ; sprite
  .byte $0F,$1C,$15,$19,$0F,$02,$38,$12,$0F,$1C,$15,$16,$0F,$02,$38,$3C

Sprites:
  ;   vert tile attr horiz
  .byte $80, $00, $00, $80 ; T sprite
  .byte $80, $01, $01, $84 ; o sprite
  .byte $80, $02, $02, $90 ; m sprite

.segment "VECTORS"
.word VBlank, Reset, Dummy

.segment "CHRROM"
.incbin "simple.chr"

;;; USAGE ;;;
; Compile with nesasm
; $ nesasm hello_world.asm
; Then run .nes file with an NES emulator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; .ines header  
  .inesprg 1
  .ineschr 1
  .inesmap 0
  .inesmir 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Variable declarations
  .rsset $0000
sprite_x .rs 1
sprite_y .rs 1
buttons .rs 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Subroutine definitions
  .bank 0
  .org $C000


ReadController:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016
  LDX #$08

ReadControllerLoop:
  LDA $4016
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
  STA $2003
  LDA #$02
  STA $4014
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Main program start
RESET:
  SEI
  CLD
  LDA #$80
  STA sprite_x
  STA sprite_y
  LDA #$00
  STA $0052

  ;; Palettes start at PPU address $3F00 and $3F10. To set this address, PPU address port $2006 is used.
  ;; The port must be written twice, once for the high byte then for the low byte.
  ;;
  ;; This code tells the PPU to set its address to $3F10. Then the PPU data port at $2007 is ready to accept data. The first write will go to the
  ;; address you set ($3F10), then the PPU will automatically increment the address after each read or write.
LoadPalettes:
  ;read PPU status to reset the high/low latch to high
  LDA $2002
  ;write 3F00 to set its address there
  LDA #$3F
  STA $2006
  LDA #$00
  STA $2006

  LDX #$00
LoadPalettesLoop:
  ; loop through PaletteData and write to PPU ($2007)
  ; 32 bytes (0x20)
  LDA PaletteData, x
  STA $2007                     ; write to PPU
  INX                           ; (inc X)
  CPX #$20                      ; Compare X to $20 (decimal 32)
  BNE LoadPalettesLoop          ; (when (not= x 32) (recur))

  ;; SPRITES
  ;;
  ;; 1 | Y Position  | $00 = top of screen, $EF = bottom of screen
  ;; 2 | Tile Number | 0 - 256, tile number for the graphic to be taken from the pattern table.
  ;; 3 | Attributes  | Holds color and display info:
  ;;                   76543210
  ;;                   |||   ||
  ;;                   |||   ++- Color Palette of sprite.  Choose which set of 4 from the 16 colors to use
  ;;                   |||
  ;;                   ||+------ Priority (0: in front of background; 1: behind background)
  ;;                   |+------- Flip sprite horizontally
  ;;                   +-------- Flip sprite vertically
  ;; 4 | X Position  | $00 = left, $F9 = right
  ;;
  ;; These 4 bytes repeat 64 times (one set per sprite) to fill the 256 bytes of sprite memory. To edit sprite 0, change bytes $0200-0203, Sprite 1 is $0204-0207, etc.

  ;; PPU Control ($2000)
  ;;  PPUCTRL ($2000)
  ;;  76543210
  ;;  | ||||||
  ;;  | ||||++- Base nametable address
  ;;  | ||||    (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
  ;;  | |||+--- VRAM address increment per CPU read/write of PPUDATA
  ;;  | |||     (0: increment by 1, going across; 1: increment by 32, going down)
  ;;  | ||+---- Sprite pattern table address for 8x8 sprites (0: $0000; 1: $1000)
  ;;  | |+----- Background pattern table address (0: $0000; 1: $1000)
  ;;  | +------ Sprite size (0: 8x8; 1: 8x16)
  ;;  |
  ;;  +-------- Generate an NMI at the start of the
  ;;            vertical blanking interval vblank (0: off; 1: on)
  ;T sprite
  ;x
  LDA #$80
  STA $0203
  ;y
  LDA #$80
  STA $0200
  ;tile number
  LDA  #$00
  STA $0201
  ; color pallete, flags
  LDA  #$00
  STA $0202

  ;o sprite
  ;x
  LDA #$84
  STA $0207
  ;y
  LDA #$80
  STA $0204
  ;tile number
  LDA  #$01
  STA $0205
  ; color pallete, flags
  LDA  #$01
  STA $0206

  ; m sprite
  ;x
  LDA #$90
  STA $020B
  ;y
  LDA #$80
  STA $0208
  ;tile number
  LDA  #$02
  STA $0209
  ; color pallete, flags
  LDA  #$02
  STA $020A

  LDA #%10000000                 ; enable NMI, sprites from pattern table 0
  STA $2000

    ;; Set up the PPU
  ;; PPUMASK ($2001)
  ;;
  ;; 76543210
  ;; ||||||||
  ;; |||||||+- Grayscale (0: normal color; 1: AND all palette entries
  ;; |||||||   with 0x30, effectively producing a monochrome display;
  ;; |||||||   note that colour emphasis STILL works when this is on!)
  ;; ||||||+-- Disable background clipping in leftmost 8 pixels of screen
  ;; |||||+--- Disable sprite clipping in leftmost 8 pixels of screen
  ;; ||||+---- Enable background rendering
  ;; |||+----- Enable sprite rendering
  ;; ||+------ Intensify reds (and darken other colors)
  ;; |+------- Intensify greens (and darken other colors)
  ;; +-------- Intensify blues (and darken other colors)
  LDA #%00010000
  STA $2001


; Once main program execution hits here, it loops infinitely.
; VBlank throws NMI interrupt when the screen is done drawing
; which causes NMI section to run (60x per sec).  All program 
; logic is currently in NMI, which is not best practice, but 
; it works at the moment.
Forever:
  JMP Forever

NMI:
  JSR HandleControllerInput
  JSR SetSpritePositions
  JSR CopySpritesToPpu

  RTI

  .bank 1
  .org $E000

PaletteData:
  ; each palette consists of 4 bytes, starting with $0F (transparent background)
  ; colors are defined in the PPU and are looked up by bytes. No RGB here.

  ; background palette data
  .db $0F,$31,$32,$33,$0F,$35,$36,$37,$0F,$39,$3A,$3B,$0F,$3D,$3E,$0F
  ; sprite palette data
  .db $0F,$1C,$15,$19,$0F,$02,$38,$12,$0F,$1C,$15,$16,$0F,$02,$38,$3C

  ; Register interrupt handlers
  .org $FFFA
  .dw NMI
  .dw RESET
  .dw 0

  ; Bank 2 for sprites
  .bank 2
  .org $0000
  .incbin "simple.chr"

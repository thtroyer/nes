;;; USAGE ;;;
; Compile with nesasm
; $ nesasm hello_world.asm
; Then run .nes file with an NES emulator
;;;;;;;;;;;;;
; .ines header  
  .inesprg 1
  .ineschr 1
  .inesmap 0
  .inesmir 1

  .bank 0
  .org $C000

RESET:
  SEI
  CLD
  LDA #$80
  STA $0050
  STA $0051
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
  ;LDA ($0050) ;#$80
  LDA #$80
  STA $0200                     ; put sprite 0 in center ($80) of screen vertically
  STA $0203                     ; put sprite 0 in center ($80) of screen horizontally
  LDA  #$00
  STA $0201                     ; tile number = 0
  STA $0202                     ; color pallete = 0, no flipping

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
  LDA #%01010000
  STA $2001


Forever:
  JMP Forever

NMI:
  ; move sprites
  LDA $0051
  STA $0200
  LDA $0050
  STA $0203
  INC $0050

  ;; copy sprites back to PPU as the PPU forgets them every cycle.
  ; write address $0200 to PPU for DMA sprite transfer
  LDA #$00
  STA $2003
  LDA #$02
  STA $4014

  RTI

  .bank 1
  .org $E000

PaletteData:
  .db $0F,$31,$32,$33,$0F,$35,$36,$37,$0F,$39,$3A,$3B,$0F,$3D,$3E,$0F  ;background palette data
  .db $0F,$1C,$15,$1A,$0F,$02,$38,$3C,$0F,$1C,$15,$14,$0F,$02,$38,$3C  ;sprite palette data

  ; Register interrupt handlers
  .org $FFFA
  .dw NMI
  .dw RESET
  .dw 0

  ; Bank 2 for sprites
  .bank 2
  .org $0000
  .incbin "simple.chr"

.setcpu "6502"
; ca65 version kinda works

; defines
;PPU_CTRL1        = $2000
;PPU_CTRL2        = $2001
;PPU_STATUS       = $2002
;PPU_SPR_ADDR     = $2003
;PPU_SPR_IO       = $2004
;PPU_VRAM_ADDR1   = $2005
;PPU_VRAM_ADDR2   = $2006
;PPU_VRAM_IO      = $2007
;
;APU_MODCTRL      = $4010
;APU_SPR_DMA      = $4014
;
;APU_PAD1         = $4016
;APU_PAD2         = $4017
;
;BTN_CHECK    = %00000001
;
;SPR_ENABLED  = %00010000
;BG_ENABLED   = %00001000
;NO_L_CLIP    = %00000010
;
;NMI_ENABLED  = %10000000
;SPRITES_8x16 = %00100000
;BG_PT_ADDR_O = %00010000
;SP_PT_ADDR_O = %00001000
;VRAM_INC     = %00000100
;NT_20        = %00000000
;NT_24        = %00000001
;NT_28        = %00000010
;NT_2C        = %00000011

.segment "HEADER"
; the header!
.byt "NES", $1a ; ines header
.byt 1 ; PRG-ROM pages (16kb)
.byt $01 ; CHR-ROM pages (8kb)


; Zero page RAM
.segment "ZEROPAGE"
sprite_x: .res 1
sprite_y: .res 1
buttons: .res 1
pointerLo: .res 1
pointerHi: .res 1

; General RAM
;.segment "BSS"


.segment "CODE"
;Reset:
  ; disable interrupts
  ;sei
  ; disable non-functional decimal mode
  ;cld

  ;.repeat 3
  ;jsr vblank_cycle
  ;.endrep

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

vblank_cycle:
  ;lda PPU_STATUS
  lda $2002
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
  ; loop through palettes and write to PPU ($2007)
  ; 32 bytes (0x20)
  LDA palettes, x
  STA $2007
  INX
  CPX #$20
  BNE LoadPalettesLoop

LoadSprites:
  LDX #$00
LoadSpritesLoop:
  LDA sprites, x
  STA $0200, x
  INX
  CPX #$10
  BNE LoadSpritesLoop

; todo: replace LoadBackground
; After some struggling, this was the first background loading subroutine which worked.
; I don't fully understand it yet and will replace/refactor to fit my style.
;LoadBackground:
  ;RTS
  ;LDA $2002             ; read PPU status to reset the high/low latch
  ;LDA #$20
  ;STA $2006             ; write the high byte of $2000 address
  ;LDA #$00
  ;STA $2006             ; write the low byte of $2000 address;
;
  ;LDA #$00
  ;STA pointerLo       ; put the low byte of the address of background into pointer
  ;LDA #HIGH(background)
  ;STA pointerHi       ; put the high byte of the address into pointer
  ;
  ;LDX #$00            ; start at pointer + 0
  ;LDY #$00
;OutsideLoop:
  ;
;InsideLoop:
  ;LDA [pointerLo], y  ; copy one background byte from address in pointer plus Y
  ;STA $2007           ; this runs 256 * 4 times
  ;
  ;INY                 ; inside loop counter
  ;CPY #$00
  ;BNE InsideLoop      ; run the inside loop 256 times before continuing down
  ;
  ;INC pointerHi       ; low byte went 0 to 256, so high byte needs to be changed now
  ;
  ;INX
  ;CPX #$04
  ;BNE OutsideLoop     ; run the outside loop 256 times before continuing down
;
              
;LoadAttribute:
;; send attribute data to PPU
;; $23C0
  ;LDA $2002
  ;LDA #$23
  ;STA $2006
  ;LDA #$C0
  ;STA $2006
  ;LDX #$00
;LoadAttributeLoop:
  ;LDA attribute, x
  ;STA $2007
  ;INX
  ;CPX #$08
  ;BNE LoadAttributeLoop

EnableNMI:
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000

EnableSprites:
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
  LDA #%00011110
  STA $2001


; Once main program execution hits here, it loops infinitely.
; VBlank throws NMI interrupt when the screen is done drawing
; which causes NMI section to run (60x per sec).  All program 
; logic is currently in NMI, which is not best practice, but 
; it works at the moment.
Forever:
  JMP Forever

VBlank:
  JSR HandleControllerInput
  JSR SetSpritePositions
  JSR CopySpritesToPpu

  ;;This is the PPU clean up section, so rendering the next frame starts properly.
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
  LDA #$00        ;;tell the ppu there is no background scrolling
  STA $2005
  STA $2005

  RTI

dummy:
  rti


;.segment "RODATA"
background:
  .byt $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
  .byt $10,$40,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$41,$10
  .byt $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byt $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byt $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byt $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byt $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byt $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byt $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byt $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byt $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byt $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byt $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byt $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byt $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byt $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byt $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byt $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byt $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byt $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byt $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byt $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byt $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byt $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byt $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byt $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byt $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byt $10,$30,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$32,$10
  .byt $10,$43,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$42,$10
  .byt $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

attribute:
  .byt $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
  .byt $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
  .byt $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
  .byt $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
  .byt $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
  .byt $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
  .byt $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
  .byt $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
  .byt $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
  .byt $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
  .byt $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
  .byt $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
  .byt $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
  .byt $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
  .byt $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55

palettes:
  ; each palette consists of 4 bytes, starting with $0F (transparent background)
  ; colors are defined in the PPU and are looked up by bytes. No RGB here.

  ; background
  .byt $0F,$2D,$3D,$30,$0F,$2D,$3D,$30,$0F,$2D,$3D,$30,$0F,$2D,$3D,$30
  ; sprite
  .byt $0F,$1C,$15,$19,$0F,$02,$38,$12,$0F,$1C,$15,$16,$0F,$02,$38,$3C

sprites:
  ;   vert tile attr horiz
  .byt $80, $00, $00, $80 ; T sprite
  .byt $80, $01, $01, $84 ; o sprite
  .byt $80, $02, $02, $90 ; m sprite

  ;.byt $24,$24,$24,$24, $47,$47,$24,$24 ,$47,$47,$47,$47, $47,$47,$24,$24 ,$24,$24,$24,$24 ,$24,$24,$24,$24, $24,$24,$24,$24, $55,$56,$24,$24  ;;brick bottoms

.segment "VECTORS"
.word VBlank, Reset, dummy

.segment "CHRROM"
.incbin "simple.chr"

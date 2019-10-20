;*******************************************************************************
;*         MC68HC11F1 FRAMEWORK INCLUDE FILE FOR ASM11 ASSEMBLER               *
;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
; FREEWARE, Copyright (c) Tony G. Papadimitriou <tonyp@acm.org>
;*******************************************************************************

                    #Uses     macros.inc
                    #Message  ****************************
                    #Message  * Target: ASPiSYS F1 Board *
                    #Message  ****************************

MHZ                 def       16
F1                  def       *                   ;Tells us this INCLUDE has been used
__ASPISYS__         def       *                   ;Tells us the ASPISYS F1 INCLUDE has been used
          #ifdef BOOT
                    #Message  BOOTROM-preloaded version
                    #Uses     apps/bootrom.exp

REGS                def       0
          #endif
REGS                def       $1000               ;Register Base Address
          #ifz REGS
                    #Message  REGS is set to zero page RAM
RAM                 def       $0060               ;Start of USER RAM (past the REGS)
          #endif
RAM                 def       $0000               ;Start of USER RAM
RAM_END             def       RAM&$F000+$0DFF     ;End of primary RAM
          #if RAM > $1000
                    #Warning  Unexpected internal RAM {RAM(h)} location
          #endif
          #ifdef BOOT
EEPROM              def       BOOTEEPROM_END
          #endif
EEPROM              def       $0E00               ;Start of EEPROM
EEPROM_END          equ       EEPROM+511          ;End of EEPROM
#ifz REGS
          #if RAM <> $1000
XRAM                def       $1000               ;External RAM
          #endif
XRAM                def       RAM_END+1           ;External RAM
#endif
XRAM                def       $1060               ;External RAM

XRAM_END            equ       $7FFF

STACKTOP            def       RAM_END             ;Top of Stack
XSTACKTOP           equ       RAM&$F000+$03FF
          #ifdef BOOT
ROM                 def       BOOTROM_END
          #endif
ROM                 def       $8000               ;Start of ROM
          #if ROM < $8000
                    #Error    ROM ({ROM(h)}) should not be set below $8000
          #endif
ROM_END             equ       $FFBF               ;End of ROM
VECTORS             equ       $FFD6               ;Start of Vectors
RESERVED_VECTORS    equ       $FFC0               ;Reserved vectors (22 bytes)

                    #MEMORY   EEPROM    EEPROM_END
                    #MEMORY   ROM       ROM_END
                    #MEMORY   VECTORS   VECTORS|$00FF

                    #VECTORS
                    org       VECTORS

                    #SEG9
                    org       RESERVED_VECTORS-512

                    #RAM
                    org       RAM

                    #XRAM
                    org       XRAM

                    #EEPROM
                    org       EEPROM

                    #ROM
                    org       ROM

; Register definitions

PORTA               equ       REGS+$00            ;Port A Data
DDRA                equ       REGS+$01            ;Data Direction Register A
PORTG               equ       REGS+$02            ;Port G Data
DDRG                equ       REGS+$03            ;Data Direction Register G
;PORTB              equ       REGS+$04            ;[NOT AVAILABLE] Port B Data
;PORTF              equ       REGS+$05            ;[IN           ] Port F Data
;PORTC              equ       REGS+$06            ;[EXPANDED     ] Port C Data
;DDRC               equ       REGS+$07            ;[MODE         ] Data Direction Register C
PORTD               equ       REGS+$08            ;Port D Data
DDRD                equ       REGS+$09            ;Data Direction Register D
PORTE               equ       REGS+$0A            ;Port E Data
CFORC               equ       REGS+$0B            ;Timer Compare Force
OC1M                equ       REGS+$0C            ;Output Compare 1 Mask
OC1D                equ       REGS+$0D            ;Output Compare 1 Data
TCNT                equ       REGS+$0E            ;Timer Count
TIC1                equ       REGS+$10            ;Timer Input Capture 1
TIC2                equ       REGS+$12            ;Timer Input Capture 2
TIC3                equ       REGS+$14            ;Timer Input Capture 3
TOC1                equ       REGS+$16            ;Timer Output Compare 1
TOC2                equ       REGS+$18            ;Timer Output Compare 2
TOC3                equ       REGS+$1A            ;Timer Output Compare 3
TOC4                equ       REGS+$1C            ;Timer Output Compare 4
TIC4                equ       REGS+$1E            ;Timer Input Capture 4
TOC5                equ       REGS+$1E            ;Timer Output Compare 5
TCTL1               equ       REGS+$20            ;Timer Control 1
TCTL2               equ       REGS+$21            ;Timer Control 2
TMSK1               equ       REGS+$22            ;Timer Interrupt Mask 1
TFLG1               equ       REGS+$23            ;Timer Interrupt Flag 1
TMSK2               equ       REGS+$24            ;Timer Interrupt Mask 2
TFLG2               equ       REGS+$25            ;Timer Interrupt Flag 2
PACTL               equ       REGS+$26            ;Pulse Accumulator Control
PACNT               equ       REGS+$27            ;Pulse Accumulator Counter
SPCR                equ       REGS+$28            ;SPI Control Register
SPSR                equ       REGS+$29            ;SPI Status Register
SPDR                equ       REGS+$2A            ;SPI Data Register
BAUD                equ       REGS+$2B            ;Baud Rate Control Register
SCCR1               equ       REGS+$2C            ;SCI Control 1
SCCR2               equ       REGS+$2D            ;SCI Control 2
SCSR                equ       REGS+$2E            ;SCI Status Register
SCDR                equ       REGS+$2F            ;SCI Data Register
ADCTL               equ       REGS+$30            ;AD Control Status Register
ADR1                equ       REGS+$31            ;AD Result 1
ADR2                equ       REGS+$32            ;AD Result 2
ADR3                equ       REGS+$33            ;AD Result 3
ADR4                equ       REGS+$34            ;AD Result 4
BPROT               equ       REGS+$35            ;Block Protect
OPT2                equ       REGS+$38            ;Additional Options
OPTION              equ       REGS+$39            ;System Configuration Options
COPRST              equ       REGS+$3A            ;COP Reset
PPROG               equ       REGS+$3B            ;EEPROM Programming Control
HPRIO               equ       REGS+$3C            ;Highest Interrupt Priority
INIT                equ       REGS+$3D            ;INIT
XINIT               equ       $103D               ;Out-of-reset INIT
CONFIG              equ       REGS+$3F            ;Configuration Register
CSSTRH              equ       REGS+$5C            ;Chip Select Clock Stretch
CSCTL               equ       REGS+$5D            ;Chip Select Program Control
CSGADR              equ       REGS+$5E            ;Chip Select General Address
CSGSIZ              equ       REGS+$5F            ;Chip Select General Address Size

                    #Uses     common.inc

?$5555              equ       $8000+$5555         ;AT28C256 special address
?$2AAA              equ       $8000+$2AAA         ;AT28C256 special address

                    #MEMORY   CONFIG

                    #DATA
                    org       CONFIG

?                   macro
                    mset      1,{EEPROM>8|$0F(h)}
                    mset      1,$~1.{:1-1}~
                    #Message  CONFIG must be set to ~1~
;                   fcb       ~1~                 ;EEPROM at {EEPROM(h)}-{EEPROM_END(h)} (for B11.EXE use)
                    endm

                    @?

;*******************************************************************************

                    #ROM
                    #push
                    #MapOff
#ifdef ATMEL
AT28C256_SDPOff     proc
                    psha                          ;AT28C256 Software Data Protection Off (read-write)
                    tpa
                    psha
                    sei

                    lda       #$AA                ;Load $AA to $5555
                    sta       ?$5555

                    coma                          ;Load $55 to $2AAA
                    sta       ?$2AAA

                    lda       #$80                ;Load $80 to $5AAA
                    sta       ?$5555

                    lda       #$AA                ;Load $AA to $5555
                    sta       ?$5555

                    coma                          ;Load $55 to $2AAA
                    sta       ?$2AAA

                    lda       #$20                ;Load $20 to $5AAA
                    sta       ?$5555

                    pula
                    tap
                    pula
                    rts

;*******************************************************************************

AT28C256_SDPOn      proc
                    psha                          ;AT28C256 Software Data Protection On (read-only)
                    tpa
                    psha
                    sei

                    lda       #$AA                ;Load $AA to $5555
                    sta       ?$5555

                    coma                          ;Load $55 to $2AAA
                    sta       ?$2AAA

                    lda       #$A0                ;Load $A0 to $5AAA
                    sta       ?$5555

                    pula
                    tap
                    pula
                    rts

;*******************************************************************************

AT28C256_ChipErase  proc
                    psha                          ;AT28C256 Software Chip Erase
                    tpa
                    psha
                    sei

                    lda       #$AA                ;Load $AA to $5555
                    sta       ?$5555

                    coma                          ;Load $55 to $2AAA
                    sta       ?$2AAA

                    lda       #$80                ;Load $80 to $5AAA
                    sta       ?$5555

                    lda       #$AA                ;Load $AA to $5555
                    sta       ?$5555

                    coma                          ;Load $55 to $2AAA
                    sta       ?$2AAA

                    lda       #$10                ;Load $10 to $5AAA
                    sta       ?$5555

                    pula
                    tap
                    pula
                    rts
#endif
                    #pull

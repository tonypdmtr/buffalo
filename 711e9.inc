;*******************************************************************************
;* MC68HC711E9 FRAMEWORK INCLUDE FILE FOR ASM11 ASSEMBLER
;*******************************************************************************

                    #Uses     macros.inc
                    #Message  *********************
                    #Message  * Target: 68HC711E9 *
                    #Message  *********************

E9                  def       *                   ;Simply tells us this INCLUDE has been used (use with #ifdef E9)
REGS                def       $1000               ;Register Base Address
          #ifz REGS
                    #Message  REGS is set to zero page RAM
RAM                 def       $0040               ;Start of USER RAM (past the REGS)
          #endif
RAM                 def       $0000               ;Start of USER RAM
RAM_END             equ       $01FF               ;End of RAM

STACKTOP            equ       $01FF               ;Top of Stack

EEPROM              equ       $B600               ;Start of EEPROM
EEPROM_END          equ       $B7FF               ;End of EEPROM

ROM                 def       $D000               ;Start of ROM
          #if ROM < $D000
                    #Error    ROM ({ROM(h)}) should not be set below $D000
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

                    #DATA
                    org       EEPROM

                    #ROM
                    org       ROM

                    #Uses     common.inc

; Register definitions

PORTA               equ       REGS+$00,1          ;Port A Data
PIOC                equ       REGS+$02,1          ;Parallel IO Control
PORTC               equ       REGS+$03,1          ;Port C Data
PORTB               equ       REGS+$04,1          ;Port B Data
PORTCL              equ       REGS+$05,1          ;Port C Latched Data
DDRC                equ       REGS+$07,1          ;Data Direction Register C
PORTD               equ       REGS+$08,1          ;Port D Data
DDRD                equ       REGS+$09,1          ;Data Direction Register D
PORTE               equ       REGS+$0A,1          ;Port E Data
CFORC               equ       REGS+$0B,1          ;Timer Compare Force
OC1M                equ       REGS+$0C,1          ;Output Compare 1 Mask
OC1D                equ       REGS+$0D,1          ;Output Compare 1 Data
TCNT                equ       REGS+$0E,2          ;Timer Count
TIC1                equ       REGS+$10,2          ;Timer Input Capture 1
TIC2                equ       REGS+$12,2          ;Timer Input Capture 2
TIC3                equ       REGS+$14,2          ;Timer Input Capture 3
TOC1                equ       REGS+$16,2          ;Timer Output Compare 1
TOC2                equ       REGS+$18,2          ;Timer Output Compare 2
TOC3                equ       REGS+$1A,2          ;Timer Output Compare 3
TOC4                equ       REGS+$1C,2          ;Timer Output Compare 4
TIC4                equ       REGS+$1E,2          ;Timer Input Capture 4
TOC5                equ       REGS+$1E,2          ;Timer Output Compare 5
TCTL1               equ       REGS+$20,1          ;Timer Control 1
TCTL2               equ       REGS+$21,1          ;Timer Control 2
TMSK1               equ       REGS+$22,1          ;Timer Interrupt Mask 1
TFLG1               equ       REGS+$23,1          ;Timer Interrupt Flag 1
TMSK2               equ       REGS+$24,1          ;Timer Interrupt Mask 2
TFLG2               equ       REGS+$25,1          ;Timer Interrupt Flag 2
PACTL               equ       REGS+$26,1          ;Pulse Accumulator Control
PACNT               equ       REGS+$27,1          ;Pulse Accumulator Counter
SPCR                equ       REGS+$28,1          ;SPI Control Register
SPSR                equ       REGS+$29,1          ;SPI Status Register
SPDR                equ       REGS+$2A,1          ;SPI Data Register
BAUD                equ       REGS+$2B,1          ;Baud Rate Control Register
SCCR1               equ       REGS+$2C,1          ;SCI Control 1
SCCR2               equ       REGS+$2D,1          ;SCI Control 2
SCSR                equ       REGS+$2E,1          ;SCI Status Register
SCDR                equ       REGS+$2F,1          ;SCI Data Register
ADCTL               equ       REGS+$30,1          ;AD Control Status Register
ADR1                equ       REGS+$31,1          ;AD Result 1
ADR2                equ       REGS+$32,1          ;AD Result 2
ADR3                equ       REGS+$33,1          ;AD Result 3
ADR4                equ       REGS+$34,1          ;AD Result 4
BPROT               equ       REGS+$35,1          ;Block Protect
EPROG               equ       REGS+$36,1          ;EPROG (E20 only, not E9)
OPTION              equ       REGS+$39,1          ;System Configuration Options
COPRST              equ       REGS+$3A,1          ;COP Reset
PPROG               equ       REGS+$3B,1          ;EEPROM Programming Control
HPRIO               equ       REGS+$3C,1          ;Highest Interrupt Priority
INIT                equ       REGS+$3D,1          ;INIT
XINIT               equ       $103D,1             ;Out-of-reset INIT
CONFIG              equ       REGS+$3F,1          ;Configuration Register

                    #MEMORY   CONFIG

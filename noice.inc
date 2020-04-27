;*******************************************************************************
;* MC68HC11F1 FRAMEWORK INCLUDE FILE FOR NOICE11 AND ASM11 ASSEMBLER
;*******************************************************************************

                    #Uses     macros.inc
                    #Message  ************************************************
                    #Message  * Target: ASPiSYS F1 Board with NOICE resident *
                    #Message  ************************************************

F1                  def       *                   ;Tells us we are using an F1 chip
__ASPISYS__         def       *                   ;Tells us we are using the ASPISYS F1 Board
__NOICE__           def       *                   ;Tells us we are using the NOICE debugger

REGS                equ       $1000               ;Register Base Address

RAM                 equ       $0000               ;Start of USER RAM
RAM_END             equ       $03FF               ;End of USER RAM

; RAM from $400 to $DFF is reserved for NoICE11

RAM1                equ       $1060               ;Start of USER RAM
RAM1_END            equ       $1FFF               ;End of USER RAM

XRAM                equ       $2000               ;Start of USER RAM
XRAM_END            equ       $2FFF               ;End of USER RAM

EEPROM              equ       $0E00               ;Start of EEPROM
EEPROM_END          equ       $0FFF               ;End of EEPROM

STACKTOP            equ       RAM_END             ;Top of Stack

ROM                 equ       $3000               ;Start of ROM (RAM acting as ROM)
ROM_END             equ       $7FFF               ;End of ROM

VECTORS             equ       $0400               ;Start of Vectors

RESERVED_VECTORS    equ       $0400               ;Reserved vectors (normally 22 bytes)

                    #MEMORY   EEPROM    EEPROM_END
                    #MEMORY   ROM       ROM_END
                    #MEMORY   VECTORS   VECTORS+42-1

                    #SEG9
                    org       ROM_END-$200

                    #VECTORS
                    org       VECTORS

                    #RAM
                    org       RAM

                    #ROM
                    org       ROM

                    #Uses     common.inc

Villop              set       $0422               ;Illegal opcode trap

; Register definitions

PORTA               equ       REGS+$00,1          ;Port A Data
DDRA                equ       REGS+$01,1          ;Data Direction Register A
PORTG               equ       REGS+$02,1          ;Port G Data
DDRG                equ       REGS+$03,1          ;Data Direction Register G
PORTB               equ       REGS+$04,1          ;Port B Data
PORTF               equ       REGS+$05,1          ;Port F Data
PORTC               equ       REGS+$06,1          ;Port C Data
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
OPT2                equ       REGS+$38,1          ;Additional Options
OPTION              equ       REGS+$39,1          ;System Configuration Options
COPRST              equ       REGS+$3A,1          ;COP Reset
PPROG               equ       REGS+$3B,1          ;EEPROM Programming Control
HPRIO               equ       REGS+$3C,1          ;Highest Interrupt Priority
INIT                equ       REGS+$3D,1          ;INIT
XINIT               equ       $103D,1             ;Out-of-reset INIT
CONFIG              equ       REGS+$3F,1          ;Configuration Register
CSSTRH              equ       REGS+$5C,1          ;Chip Select Clock Stretch
CSCTL               equ       REGS+$5D,1          ;Chip Select Program Control
CSGADR              equ       REGS+$5E,1          ;Chip Select General Address
CSGSIZ              equ       REGS+$5F,1          ;Chip Select General Address Size

                    #DATA
                    org       EEPROM              ;Reset DATA segment to start of EEPROM

                    #ROM

                    #!Drop    SetChipSelects

SetChipSelects      macro                         ;Dummy
                    endm

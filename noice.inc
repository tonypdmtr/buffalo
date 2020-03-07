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

PORTA               equ       REGS+$00            ;Port A Data
DDRA                equ       REGS+$01            ;Data Direction Register A
PORTG               equ       REGS+$02            ;Port G Data
DDRG                equ       REGS+$03            ;Data Direction Register G
PORTB               equ       REGS+$04            ;Port B Data
PORTF               equ       REGS+$05            ;Port F Data
PORTC               equ       REGS+$06            ;Port C Data
DDRC                equ       REGS+$07            ;Data Direction Register C
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

                    #DATA
                    org       EEPROM              ;Reset DATA segment to start of EEPROM

                    #ROM

                    #!Drop    SetChipSelects

SetChipSelects      macro                         ;Dummy
                    endm

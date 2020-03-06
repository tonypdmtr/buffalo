;*******************************************************************************
;* WSI Development Kit Include File for the ASM11 Assembler                    *
;*******************************************************************************

                    #Uses     macros.inc
                    #Message  ****************************************
                    #Message  * Target: WSI DK68HC11 Development Kit *
                    #Message  ****************************************

__WSI__             def       $F1                 ;Tells us this INCLUDE has been used
__DK68HC11__        def       *

REGS                equ       $8000               ;Register Base Address

RAM                 equ       $8040               ;Start of USER RAM
RAM_END             equ       $89FF               ;PSD RAM $8200-$89FF

EEPROM              equ       $0FFF               ;Start of EEPROM
EEPROM_END          equ       $0FFF               ;End of EEPROM

XRAM                equ       $0000               ;External RAM
XRAM_END            equ       $7FFF

FLASH               equ       $2000               ;Start of Flash window
FLASH_END           equ       $AFFF               ;End of Flash window

STACKTOP            equ       RAM_END&$0FFF       ;Top of Stack (before INIT change)

ROM                 def       $E000               ;Start of boot EEPROM
ROM_END             equ       $FFBF               ;End of ROM

VECTORS             equ       $FFD6               ;Start of Vectors
RESERVED_VECTORS    equ       $FFC0               ;Reserved vectors (22 bytes)

          #if ROM < $C000
                    #Error    ROM ({ROM(h)}) should not be lower than $C000
          #endif
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

                    #ROM
                    org       ROM

                    #Uses     common.inc

; Register definitions

PORTA               equ       REGS+$00            ;Port A Data
PORTD               equ       REGS+$08            ;Port D Data
DDRD                equ       REGS+$09            ;Data Direction Register D
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
OPTION              equ       REGS+$39            ;System Configuration Options
COPRST              equ       REGS+$3A            ;COP Reset
PPROG               equ       REGS+$3B            ;EEPROM Programming Control
HPRIO               equ       REGS+$3C            ;Highest Interrupt Priority
INIT                equ       REGS+$3D            ;INIT
XINIT               equ       $003D               ;Out-of-reset INIT
CONFIG              equ       REGS+$3F            ;Configuration Register

                    #MEMORY   CONFIG

LCD_PORT            equ       $8B00

; WSI register definitions

WSIREGS             equ       $8A00               ;Base for WSI internal registers

DATAIN_A            equ       WSIREGS+$00
DATAIN_B            equ       WSIREGS+$01
DATAIN_C            equ       WSIREGS+$10
DATAIN_D            equ       WSIREGS+$11
DATAOUT_A           equ       WSIREGS+$04
DATAOUT_B           equ       WSIREGS+$05
DATAOUT_C           equ       WSIREGS+$12
DATAOUT_D           equ       WSIREGS+$13
DIRECTION_A         equ       WSIREGS+$06
DIRECTION_B         equ       WSIREGS+$07
DIRECTION_C         equ       WSIREGS+$14
DIRECTION_D         equ       WSIREGS+$15
DRIVE_A             equ       WSIREGS+$08
DRIVE_B             equ       WSIREGS+$09
DRIVE_C             equ       WSIREGS+$16
DRIVE_D             equ       WSIREGS+$17
OUTENABLE_A         equ       WSIREGS+$0C
OUTENABLE_B         equ       WSIREGS+$0D
OUTENABLE_C         equ       WSIREGS+$1A
OUTENABLE_D         equ       WSIREGS+$1B
CONTROL_A           equ       WSIREGS+$02
CONTROL_B           equ       WSIREGS+$03
IMC_A               equ       WSIREGS+$0A
IMC_B               equ       WSIREGS+$0B
IMC_C               equ       WSIREGS+$18
OMC_AB              equ       WSIREGS+$20
OMC_BC              equ       WSIREGS+$21
OMCMASK_AB          equ       WSIREGS+$22
OMCMASK_BC          equ       WSIREGS+$23
MAINPROTECT         equ       WSIREGS+$C0
ALTPROTECT          equ       WSIREGS+$C2
JTAG                equ       WSIREGS+$C7
PMMR0               equ       WSIREGS+$B0
PMMR1               equ       WSIREGS+$B2
PMMR2               equ       WSIREGS+$B4
PAGE                equ       WSIREGS+$E0
VM                  equ       WSIREGS+$E2

;PSD JTAG

JEN.                equ       Bit0.               ;JTAG enable

;PSD PMMR0

APD_ENABLE.         equ       Bit1.
PLD_TURBO.          equ       Bit3.
PLD_ARRAY_CLK.      equ       Bit4.
PLD_MCELL_CLK.      equ       Bit5.

;PSD PMMR1

SLEEP_ENABLE.       equ       Bit1.

;PSD PMMR2

PLD_CNTL0.          equ       Bit2.
PLD_CNTL1.          equ       Bit3.
PLD_CNTL2.          equ       Bit4.
PLD_ALE.            equ       Bit5.
PLD_DBE.            equ       Bit6.

;PSD VM

SRAM_CODE.          equ       Bit0.
EE_CODE.            equ       Bit1.
FL_CODE.            equ       Bit2.
EE_DATA.            equ       Bit3.
FL_DATA.            equ       Bit4.
PIO_EN.             equ       Bit7.

;Flash/EEPROM parameters

NVM_DATA_POLL       equ       $80                 ;flash/eeprom status "data poll" bit at DQ7
NVM_DATA_TOGGLE     equ       $40                 ;flash/eeprom status "toggle poll" bit at DQ6
NVM_ERROR           equ       $20                 ;flash/eeprom status "error" bit at DQ5
MAX_EEPROM_RETRY    equ       $0FFF               ;Maximum number of attemps to check status after
                                                  ;a write operation to EEPROM

;*******************************************************************************
                    #ROM
;*******************************************************************************

                    #push
                    #MapOff

?RAM                equ       RAM&$F000>8         ;RAM @ $8000
?REGS               equ       REGS&$F000>12       ;REGS @ $1000

SetupWSI            proc
                    lda       #?RAM|?REGS
                    sta       XINIT               ;HC11ED0 initial INIT register

                    lda       #NOCOP.
                    sta       CONFIG

                    lda       #$FF                ;WSI's LCD port enabled
                    sta       CONTROL_A
                    sta       DIRECTION_A
                    sta       DATAOUT_B
                    sta       DATAOUT_C
                    sta       DIRECTION_C
                    lda       #$0F
                    sta       DIRECTION_B

                    clr       PAGE

                    tsx                           ;X := old SP
                    xgdx
                    anda      #$0F                ;zero page nibble
                    ora       #?RAM               ;update with new page nibble
                    xgdx
                    txs                           ;SP := new X
                    rts

;*******************************************************************************
; Purpose: Select WSI page
; Input  : A = 3-bit page (0-4)
; Output : None

WSIPage             proc
                    cmpa      #4
                    bhi       Fail@@
                    sta       PAGE
                    clc
                    rts

Fail@@              sec
                    rts

;*******************************************************************************
                    #pull
;*******************************************************************************

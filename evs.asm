;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
; Disassembled by Shadow11 [Monday, May 24, 1999, 2:41:10 pm]
; Original filename: 11EVS25.S19 CRC $C72D9173 (22180 bytes)
;
; Re-assemble with ASM11 to produce original S19 file (however, without the S0
;  header; to compare you got to remove the S0 line from the original file.)
;
; Some problems were found already but not corrected until the source
; is "fully" reversed which requires getting the same S19 as the original.
;
; Assemble with the HINT conditional to have reverse engineering hints displayed
;*******************************************************************************

#ifdef ?
  #Hint +===================================================
  #Hint | Available conditionals (for use with -Dx option)
  #Hint +===================================================
  #Hint | DEBUG: for SIM11x runs (faster bps, etc.)
  #Hint | HINT : Enables display of message address(es)
  #Hint +===================================================
  #Fatal Run ASM11 -Dx (where x is any of the above)
#endif
          #ifdef DEBUG
                    #Hint     DEBUG (do NOT burn EPROM)
          #endif
                    #ExtraOn                      ;Extra mnemonics allowed
                    #CaseOn
                    #OptRelOff                    ;No JMP/JSR optimizations until source is "fully" reversed
                    #OptRtsOff                    ;No JSR/RTS optimizations until source is "fully" reversed

;*******************************************************************************
; Macros
;*******************************************************************************

Hint                macro     Message
          #ifdef HINT
                    #Hint     ~'~1~....................'.1.20~ @ {:PC(x)}
          #endif
                    endm

;-------------------------------------------------------------------------------

ShowDelay           macro
          #ifdef HINT
                    #temp     ~1~*:temp+:cycles+:ocycles
                    #Hint     \@~procname~\@ [{:temp} cycles] ~{1000*:temp/BUS_HZ} msec @{BUS_KHZ(3)} MHz bus
          #endif
                    endm

;*******************************************************************************

                    #Page     EQUATES

BUS_HZ              def       2000000
BUS_KHZ             def       BUS_HZ/1000
BUS_MHZ             def       BUS_HZ/1000000

CR                  equ       13
LF                  equ       10

REGS                def       $1000
BAUD                def       REGS+$2B
SCCR2               def       REGS+$2D
SCSR                def       REGS+$2E
SCDR                def       REGS+$2F
BPROT               def       REGS+$35
COPRST              def       REGS+$3A
PPROG               def       REGS+$3B
HPRIO               def       REGS+$3C
INIT                def       REGS+$3D
XINIT               def       $103D
CONFIG              def       REGS+$3F

RBOOT.              def       $80

          #ifdef F1
CSSTRH              def       REGS+$5C            ;Chip Select Clock Stretch
CSCTL               def       REGS+$5D            ;Chip Select Program Control
CSGADR              def       REGS+$5E            ;Chip Select General Address
CSGSIZ              def       REGS+$5F            ;Chip Select General Address Size
          #endif

;-------------------------------------------------------------------------------
; System Variables
;-------------------------------------------------------------------------------

STACKTOP            def       $55B0
NestLevel           def       $5405               ;Possibly counts the ISR nesting level
SOME_IO             def       $55C0               ;Possibly some I/O
REGBASE             def       $543D               ;Register base
JSR_Hook            def       $541F

RangeBegin          def       $017B
RangeEnd            def       $017D

RegisterPointer     equ       $5400

;-------------------------------------------------------------------------------
; EEPROM programming constants
;-------------------------------------------------------------------------------

ERASED_STATE        def       -1

eeByteErase         equ       $16
eeBulkErase         equ       $06

;-------------------------------------------------------------------------------
; Peripherals
;-------------------------------------------------------------------------------

DEV_ONE_CTRL        equ       $74
DEV_ONE_DATA        equ       $77

                    #VECTORS

                    org       $BFD6
          #ifdef DEBUG
                    org       $FFD6
          #endif
                    #SEG1
                    org       $8400

                    #SEG2
                    org       $8600

                    #SEG3
                    org       $9400

                    #SEG4
                    org       $A400

                    #SEG5
                    org       $B400

                    #SEG6
                    org       $B800

                    #SEG7
                    org       $BC10

          #ifdef F1
                    #SEG0
                    org       $A960

StartF1             proc
                    clr       CSSTRH              ;No clock stretch
                    clr       CSGADR              ;RAM at $0000
                    lda       #%1                 ;32K RAM
                    sta       CSGSIZ
                    lda       #%01000101          ;CSIO1 Active High, 32K ROM at $8000
                    sta       CSCTL
                    jmp       Start

                    #Memory   StartF1 :PC-1
          #endif
                    #Memory   $8400 $85AB
                    #Memory   $8600 $90B3
                    #Memory   $9400 $9F1F
                    #Memory   $A400 $A95F
                    #Memory   $B400 $B5DD
                    #Memory   $B800 $BC07
                    #Memory   $BC10 $BD87

          #ifdef DEBUG
                    #Memory   $FFD6 $FFFF
          #else
                    #Memory   $BFD6 $BFFF
          #endif

                    #RAM
                    org       $E0
BUF6
Head                rmb       2
Tail                rmb       2
                    rmb       2
BUF6_LEN            equ       *-BUF6
Buffer              rmb       1                   ;size is random at this time

;*******************************************************************************
                    #SEG1
;*******************************************************************************

L8400               jmp       L8512               ; $8400 7E 85 12
L8403               jmp       L8535               ; $8403 7E 85 35
WRITE_DEV_ONE_Hook  jmp       WRITE_DEV_ONE       ; $8406 7E 84 48
L8409               jmp       L8587               ; $8409 7E 85 87
GetTimedChar_Hook   jmp       GetTimedChar        ; $840C 7E BC EF
L840F               jmp       InitSCI             ; $840F 7E BD 0C
PutChar_Hook        jmp       PutChar             ; $8412 7E BC 40
                    jmp       LDB63_Device        ; $8415 7E BD 63

;*******************************************************************************

L8418               proc
                    clr       [BPROT              ; $8418 7F 00 35
                    bclr      [HPRIO,#RBOOT.      ; $841B 15 3C 80  Disable bootstrap ROM
                    lda       #6                  ; $841E 86 06
                    sta       $71                 ; $8420 97 71

                    ldx       #$017B              ; $8422 CE 01 7B
Loop@@              bsr       READ_DEV_ONE        ; $8425 8D 18
                    bsr       WRITE_DEV_ONE       ; $8427 8D 1F
                    sta       ,x                  ; $8429 A7 00
                    inx                           ; $842B 08
                    cmpx      #$0180              ; $842C 8C 01 80
                    bne       Loop@@              ; $842F 26 F4

                    tab                           ; $8431 16
                    tsta                          ; $8432 4D
                    bmi       VerifyRange         ; $8433 2B 1B
                    beq       VerifyRange         ; $8435 27 19
                    deca                          ; $8437 4A
                    beq       L848E               ; $8438 27 54
                    deca                          ; $843A 4A
                    beq       EE_BulkErase_Hook   ; $843B 27 4F
                    bra       EE_EraseRange_Hook  ; $843D 20 49

;*******************************************************************************

READ_DEV_ONE        proc
                    ldb       DEV_ONE_CTRL        ; $843F D6 74
                    andb      #$20                ; $8441 C4 20
                    beq       READ_DEV_ONE        ; $8443 27 FA
                    lda       DEV_ONE_DATA        ; $8445 96 77
                    rts                           ; $8447 39

;*******************************************************************************

WRITE_DEV_ONE       proc
                    tst       DEV_ONE_CTRL        ; $8448 7D 00 74
                    bpl       WRITE_DEV_ONE       ; $844B 2A FB
                    sta       DEV_ONE_DATA        ; $844D 97 77
                    rts                           ; $844F 39

;*******************************************************************************

VerifyRange         proc
                    ldx       RangeBegin          ; $8450 FE 01 7B
Loop@@              lda       #ERASED_STATE       ; $8453 86 FF
                    cmpa      ,x                  ; $8455 A1 00
                    bne       L8462               ; $8457 26 09
                    cmpx      RangeEnd            ; $8459 BC 01 7D
                    beq       L8461               ; $845C 27 03
                    inx                           ; $845E 08
                    bra       Loop@@              ; $845F 20 F2

L8461               deca                          ; $8461 4A
L8462               bsr       WRITE_DEV_ONE       ; $8462 8D E4
                    tstb                          ; $8464 5D
                    bmi       L846F               ; $8465 2B 08
                    cmpa      #$FE                ; $8467 81 FE
                    beq       L84AC               ; $8469 27 41
                    bsr       READ_DEV_ONE        ; $846B 8D D2
                    beq       L84AC               ; $846D 27 3D

L846F               bsr       READ_DEV_ONE        ; $846F 8D CE
                    bsr       Delay20ms           ; $8471 8D 0C
                    lda       DEV_ONE_CTRL        ; $8473 96 74
                    lda       DEV_ONE_DATA        ; $8475 96 77
                    bset      $3C,#$80            ; $8477 14 3C 80
                    ldx       RESET_VECTOR        ; $847A FE BF FE
                    jmp       ,x                  ; $847D 6E 00

;*******************************************************************************

                    #Cycles   6
Delay20ms           proc
                    ldy       #5714               ; $847F 18 CE 16 52
                    #Cycles
Loop@@              dey                           ; $8483 18 09
                    bne       Loop@@              ; $8485 26 FC
                    #temp     :cycles
                    rts                           ; $8487 39

                    @ShowDelay 5714

;*******************************************************************************
; "long branch" hooks
;*******************************************************************************

EE_EraseRange_Hook  bra       EE_EraseRange       ; $8488 20 57
VerifyRange_Hook    bra       VerifyRange         ; $848A 20 C4
EE_BulkErase_Hook   bra       EE_BulkErase        ; $848C 20 46
L848E               bra       L849B               ; $848E 20 0B

;*******************************************************************************

                    #Cycles   6
Delay4ms_2          proc
                    ldy       #1143               ; $8490 18 CE 04 77
                    #Cycles
Loop@@              dey                           ; $8494 18 09
                    bne       Loop@@              ; $8496 26 FC
                    #temp     :cycles
                    rts                           ; $8498 39

                    @ShowDelay 1143

;*******************************************************************************

L8499               bsr       READ_DEV_ONE        ; $8499 8D A4

;-------------------------------------------------------------------------------

L849B               proc
                    ldx       RangeBegin          ; $849B FE 01 7B
Loop@@              lda       ,x                  ; $849E A6 00
                    bsr       WRITE_DEV_ONE       ; $84A0 8D A6
                    bsr       READ_DEV_ONE        ; $84A2 8D 9B
                    cmpx      RangeEnd            ; $84A4 BC 01 7D
                    beq       L846F               ; $84A7 27 C6
                    inx                           ; $84A9 08
                    bra       Loop@@              ; $84AA 20 F2

;-------------------------------------------------------------------------------

L84AC               proc
                    ldx       RangeBegin          ; $84AC FE 01 7B
Loop@@              bsr       READ_DEV_ONE        ; $84AF 8D 8E
                    bsr       WRITE_DEV_ONE       ; $84B1 8D 95
                    sta       $0181               ; $84B3 B7 01 81
                    ora       #$08                ; $84B6 8A 08
                    sta       ,x                  ; $84B8 A7 00
                    lda       $0181               ; $84BA B6 01 81
                    cmpx      #$8000              ; $84BD 8C 80 00
                    blo       Write@@             ; $84C0 25 06
                    ldb       #$20                ; $84C2 C6 20
                    bsr       L8503               ; $84C4 8D 3D
                    bra       Skip@@              ; $84C6 20 04

Write@@             ldb       #$02                ; $84C8 C6 02
                    bsr       EE_Burn             ; $84CA 8D 28
Skip@@              cmpx      RangeEnd            ; $84CC BC 01 7D
                    beq       L8499               ; $84CF 27 C8
                    inx                           ; $84D1 08
                    bra       Loop@@              ; $84D2 20 DB

;*******************************************************************************

EE_BulkErase        proc
                    ldx       RangeBegin          ; $84D4 FE 01 7B
                    lda       #ERASED_STATE       ; $84D7 86 FF
                    sta       ,x                  ; $84D9 A7 00
                    ldb       #6                  ; $84DB C6 06
                    bsr       EE_Burn             ; $84DD 8D 15
                    bra       VerifyRange_Hook    ; $84DF 20 A9

;*******************************************************************************

EE_EraseRange       proc
                    ldx       RangeBegin          ; $84E1 FE 01 7B
                    lda       #ERASED_STATE       ; $84E4 86 FF
                    sta       ,x                  ; $84E6 A7 00
Loop@@              ldb       #$16                ; $84E8 C6 16
                    bsr       EE_Burn             ; $84EA 8D 08
                    inx                           ; $84EC 08
                    cmpx      RangeEnd            ; $84ED BC 01 7D
                    bls       Loop@@              ; $84F0 23 F6
                    bra       VerifyRange_Hook    ; $84F2 20 96

;*******************************************************************************

EE_Burn             proc
                    stb       [PPROG              ; $84F4 D7 3B
                    sta       ,x                  ; $84F6 A7 00
                    inc       [PPROG              ; $84F8 7C 00 3B
                    bsr       Delay20ms           ; $84FB 8D 82
                    clr       [PPROG              ; $84FD 7F 00 3B
                    ldb       #$80                ; $8500 C6 80
                    rts                           ; $8502 39

;*******************************************************************************

L8503               proc
                    stb       [BAUD               ; $8503 D7 2B
                    sta       ,x                  ; $8505 A7 00
                    inc       [BAUD               ; $8507 7C 00 2B
                    bsr       Delay4ms_2          ; $850A 8D 84
                    clr       [BAUD               ; $850C 7F 00 2B
                    ldb       #$80                ; $850F C6 80
                    rts                           ; $8511 39

;*******************************************************************************

L8512               proc
                    ldy       #-1                 ; $8512 18 CE FF FF
Loop@@              ldb       DEV_ONE_CTRL        ; $8516 D6 74
                    andb      #$20                ; $8518 C4 20
                    bne       Read@@              ; $851A 26 16
                    jsr       KickCOP             ; $851C BD A7 DB
                    dey                           ; $851F 18 09
                    bne       Loop@@              ; $8521 26 F3
                    jsr       NewLine             ; $8523 BD 89 86
                    ldx       #MsgReadError       ; $8526 CE B4 94
                    jsr       PrintMessage        ; $8529 BD 88 F9
                    jsr       L8587               ; $852C BD 85 87
                    jmp       COMMON_ISR_RESET    ; $852F 7E 8E E3

Read@@              lda       DEV_ONE_DATA        ; $8532 96 77
                    rts                           ; $8534 39

;*******************************************************************************

L8535               proc
                    lda       #0                  ; $8535 86 00
                    sta       $70                 ; $8537 97 70

                    lda       #$D0                ; $8539 86 D0
                    sta       $71                 ; $853B 97 71

                    lda       SOME_IO             ; $853D B6 55 C0
                    ora       #%01000000          ; $8540 8A 40
                    anda      #%11111101          ; $8542 84 FD
                    sta       SOME_IO             ; $8544 B7 55 C0

                    jsr       Delay20ms           ; $8547 BD 84 7F
                    ora       #$01                ; $854A 8A 01
                    sta       SOME_IO             ; $854C B7 55 C0

                    ldb       #$0C                ; $854F C6 0C
                    stb       $73                 ; $8551 D7 73

                    ora       #$80                ; $8553 8A 80
                    sta       SOME_IO             ; $8555 B7 55 C0

                    ora       #$04                ; $8558 8A 04
                    sta       SOME_IO             ; $855A B7 55 C0

                    ora       #$02                ; $855D 8A 02
                    sta       SOME_IO             ; $855F B7 55 C0

                    lda       #$FF                ; $8562 86 FF
                    jsr       WRITE_DEV_ONE_Hook  ; $8564 BD 84 06

                    ldx       #L8418              ; $8567 CE 84 18
Loop@@              lda       ,x                  ; $856A A6 00
                    jsr       WRITE_DEV_ONE_Hook  ; $856C BD 84 06
                    jsr       L8400               ; $856F BD 84 00
                    inx                           ; $8572 08
                    cmpx      #$8517              ; $8573 8C 85 17
                    bls       Loop@@              ; $8576 23 F2

                    ldx       #$FFFF              ; $8578 CE FF FF
Delay@@             dex                           ; $857B 09
                    bne       Delay@@             ; $857C 26 FD

                    lda       #6                  ; $857E 86 06
                    sta       $71                 ; $8580 97 71
                    lda       DEV_ONE_CTRL        ; $8582 96 74
                    lda       DEV_ONE_DATA        ; $8584 96 77
                    rts                           ; $8586 39

;*******************************************************************************

L8587               proc
                    lda       SOME_IO             ; $8587 B6 55 C0
                    anda      #$FD                ; $858A 84 FD
                    sta       SOME_IO             ; $858C B7 55 C0
                    jsr       Delay20ms           ; $858F BD 84 7F
                    anda      #$FB                ; $8592 84 FB
                    sta       SOME_IO             ; $8594 B7 55 C0
                    anda      #$7F                ; $8597 84 7F
                    sta       SOME_IO             ; $8599 B7 55 C0
                    clr       $0073               ; $859C 7F 00 73
                    anda      #$BF                ; $859F 84 BF
                    ora       #$02                ; $85A1 8A 02
                    sta       SOME_IO             ; $85A3 B7 55 C0
                    anda      #$FE                ; $85A6 84 FE
                    sta       SOME_IO             ; $85A8 B7 55 C0
                    rts                           ; $85AB 39

;*******************************************************************************
                    #SEG2
;*******************************************************************************

L8600               proc
                    ldx       #$5408              ; $8600
                    stb       $540C               ; $8603 F7 54 0C
Loop@@              lda       ,x                  ; $8606 A6 00
                    ldy       #WRITE_DEV_ONE_Hook ; $8608 18 CE 84 06 ???
                    jsr       L861D               ; $860C BD 86 1D
                    ldy       #L8400              ; $860F 18 CE 84 00
                    jsr       L861D               ; $8613 BD 86 1D
                    inx                           ; $8616 08
                    cmpx      #$540D              ; $8617 8C 54 0D
                    bne       Loop@@              ; $861A 26 EA
                    rts                           ; $861C 39

;*******************************************************************************

L861D               proc
                    ldb       $5441               ; $861D F6 54 41
                    aby                           ; $8620 18 3A
                    ldb       $5442               ; $8622 F6 54 42
                    aby                           ; $8625 18 3A
                    jsr       ,Y                  ; $8627 18 AD 00
                    rts                           ; $862A 39

;*******************************************************************************

L862B               proc
                    jsr       L8757               ; $862B BD 87 57
                    ldy       #L8403              ; $862E 18 CE 84 03
                    jsr       L861D               ; $8632 BD 86 1D
                    ldb       #$FF                ; $8635 C6 FF
L8637               bsr       L8659               ; $8637 8D 20
                    cmpa      #$FE                ; $8639 81 FE
                    beq       L8646               ; $863B 27 09
                    ldx       #errNOTBLANK        ; $863D CE B4 2C
L8640               jsr       NewLine             ; $8640 BD 89 86
                    jsr       PrintMessage        ; $8643 BD 88 F9
L8646               ldy       #WRITE_DEV_ONE_Hook ; $8646 18 CE 84 06 ???
                    jsr       L861D               ; $864A BD 86 1D
                    bsr       L865C               ; $864D 8D 0D
                    ldy       #L8409              ; $864F 18 CE 84 09
                    jsr       L861D               ; $8653 BD 86 1D
                    jmp       COMMON_ISR_RESET    ; $8656 7E 8E E3

;*******************************************************************************

L8659               proc
                    jsr       L8600               ; $8659 BD 86 00

;*******************************************************************************

L865C               proc
                    ldy       #L8400              ; $865C 18 CE 84 00
                    jsr       L861D               ; $8660 BD 86 1D
                    rts                           ; $8663 39

;*******************************************************************************

L8664               proc
                    jsr       L8757               ; $8664 BD 87 57
                    ldy       #L8403              ; $8667 18 CE 84 03
                    jsr       L861D               ; $866B BD 86 1D
                    clrb                          ; $866E 5F
                    bsr       L8659               ; $866F 8D E8
                    cmpa      #$FE                ; $8671 81 FE
                    beq       L8695               ; $8673 27 20

                    ldx       #errNOTBLANK        ; $8675 CE B4 2C
                    jsr       NewLine             ; $8678 BD 89 86
                    jsr       PrintMessage        ; $867B BD 88 F9
                    ldx       #MsgContinue        ; $867E CE B4 6A
                    jsr       PrintMessage        ; $8681 BD 88 F9
                    jsr       L87BE               ; $8684 BD 87 BE
                    lda       $549B               ; $8687 B6 54 9B
                    suba      #$0D                ; $868A 80 0D
                    ldy       #WRITE_DEV_ONE_Hook ; $868C 18 CE 84 06 ???
                    jsr       L861D               ; $8690 BD 86 1D
                    bne       L8646               ; $8693 26 B1

L8695               ldx       $5408               ; $8695 FE 54 08
                    pshx                          ; $8698 3C
                    jsr       NewLine             ; $8699 BD 89 86
Loop@@              tst       $5409               ; $869C 7D 54 09
                    bne       L86AA               ; $869F 26 09
                    ldx       #MsgCR              ; $86A1 CE B4 68
                    jsr       PrintMessage        ; $86A4 BD 88 F9
                    jsr       L88CE               ; $86A7 BD 88 CE
L86AA               jsr       L89B4               ; $86AA BD 89 B4
                    tst       $549E               ; $86AD 7D 54 9E
                    beq       L86B5               ; $86B0 27 03
                    lda       $540D               ; $86B2 B6 54 0D
L86B5               ldy       #WRITE_DEV_ONE_Hook ; $86B5 18 CE 84 06 ???
                    jsr       L861D               ; $86B9 BD 86 1D
                    ldy       #L8400              ; $86BC 18 CE 84 00
                    jsr       L861D               ; $86C0 BD 86 1D
                    ldx       $5408               ; $86C3 FE 54 08
                    cmpx      $540A               ; $86C6 BC 54 0A
                    beq       L86D0               ; $86C9 27 05
                    jsr       L8A58               ; $86CB BD 8A 58
                    bra       Loop@@              ; $86CE 20 CC

L86D0               ldx       #MsgCR              ; $86D0 CE B4 68
                    jsr       PrintMessage        ; $86D3 BD 88 F9
                    jsr       L88CE               ; $86D6 BD 88 CE
                    ldy       #WRITE_DEV_ONE_Hook ; $86D9 18 CE 84 06 ???
                    jsr       L861D               ; $86DD BD 86 1D
                    pulx                          ; $86E0 38
                    clr       $54B7               ; $86E1 7F 54 B7
                    stx       $5408               ; $86E4 FF 54 08
                    tst       $549E               ; $86E7 7D 54 9E
                    beq       L86EF               ; $86EA 27 03
                    inc       $549F               ; $86EC 7C 54 9F
L86EF               ldy       #L8400              ; $86EF 18 CE 84 00
                    jsr       L861D               ; $86F3 BD 86 1D
                    ldy       #WRITE_DEV_ONE_Hook ; $86F6 18 CE 84 06 ???
                    jsr       L861D               ; $86FA BD 86 1D
                    tst       $54B7               ; $86FD 7D 54 B7
                    beq       L8707               ; $8700 27 05
                    jsr       L8991               ; $8702 BD 89 91
                    bra       L8717               ; $8705 20 10

L8707               psha                          ; $8707 36
                    jsr       L89B4               ; $8708 BD 89 B4
                    tst       $549F               ; $870B 7D 54 9F
                    beq       L8713               ; $870E 27 03
                    lda       $540D               ; $8710 B6 54 0D
L8713               pulb                          ; $8713 33
                    sba                           ; $8714 10
                    bne       L8732               ; $8715 26 1B
L8717               ldx       $5408               ; $8717 FE 54 08
                    cmpx      $540A               ; $871A BC 54 0A
                    beq       L8724               ; $871D 27 05
                    jsr       L8A58               ; $871F BD 8A 58
                    bra       L86EF               ; $8722 20 CB

L8724               ldx       #MsgReadComplete    ; $8724 CE B4 86
                    tst       $54B7               ; $8727 7D 54 B7
                    bne       L872F               ; $872A 26 03
                    ldx       #MsgVerifyOK        ; $872C CE B4 58
L872F               jmp       L8640               ; $872F 7E 86 40

L8732               ldx       #errNOVERIFY        ; $8732 CE B4 3F
                    jmp       L8640               ; $8735 7E 86 40

;*******************************************************************************

L8738               proc
                    bsr       L8757               ; $8738 8D 1D
                    clr       $54B7               ; $873A 7F 54 B7
L873D               ldy       #L8403              ; $873D 18 CE 84 03
                    jsr       L861D               ; $8741 BD 86 1D
                    ldb       #1                  ; $8744 C6 01
                    jsr       L8600               ; $8746 BD 86 00
                    tst       $549E               ; $8749 7D 54 9E
                    beq       L8751               ; $874C 27 03
                    jsr       LA5C8               ; $874E BD A5 C8
L8751               ldx       $5408               ; $8751 FE 54 08
                    jmp       L86EF               ; $8754 7E 86 EF

;*******************************************************************************

L8757               proc
                    clr       $549E               ; $8757 7F 54 9E
                    clr       $549F               ; $875A 7F 54 9F
                    lda       $5417               ; $875D B6 54 17
                    beq       L8770               ; $8760 27 0E
                    cmpa      #$02                ; $8762 81 02
                    blo       Done@@              ; $8764 25 1F
                    bhi       Skip@@              ; $8766 22 0F
Loop@@              ldd       $540A               ; $8768 FC 54 0A
                    subd      $5408               ; $876B B3 54 08
                    bcc       AnRTS@@             ; $876E 24 06
L8770               inc       NestLevel           ; $8770 7C 54 05
                    jmp       COMMON_ISR_RESET    ; $8773 7E 8E E3

AnRTS@@             rts                           ; $8776 39

Skip@@              dec       $5417               ; $8777 7A 54 17
                    ldd       $540C               ; $877A FC 54 0C
                    std       $540E               ; $877D FD 54 0E
                    inc       $549E               ; $8780 7C 54 9E
                    bra       Loop@@              ; $8783 20 E3

Done@@              ldd       $5408               ; $8785 FC 54 08
                    std       $540A               ; $8788 FD 54 0A
                    rts                           ; $878B 39

;*******************************************************************************

L878C               proc
                    lda       $5417               ; $878C B6 54 17
                    cmpa      #1                  ; $878F 81 01
                    bhi       L8770               ; $8791 22 DD
                    deca                          ; $8793 4A
                    bne       L8770               ; $8794 26 DA
                    ldd       $5408               ; $8796 FC 54 08
                    std       $540A               ; $8799 FD 54 0A
                    ldy       #L8403              ; $879C 18 CE 84 03
                    jsr       L861D               ; $87A0 BD 86 1D
                    ldb       #2                  ; $87A3 C6 02
                    jmp       L8637               ; $87A5 7E 86 37

;*******************************************************************************

L87A8               proc
                    bsr       L8757               ; $87A8 8D AD
                    inc       $54B7               ; $87AA 7C 54 B7
                    bra       L873D               ; $87AD 20 8E

;*******************************************************************************

L87AF               proc
                    jsr       L8757               ; $87AF BD 87 57
                    ldy       #L8403              ; $87B2 18 CE 84 03
                    jsr       L861D               ; $87B6 BD 86 1D
                    ldb       #3                  ; $87B9 C6 03
                    jmp       L8637               ; $87BB 7E 86 37

;*******************************************************************************

L87BE               proc
                    psha                          ; $87BE 36
                    pshb                          ; $87BF 37
                    pshx                          ; $87C0 3C

                    jsr       LA6F5               ; $87C1 BD A6 F5
                    ldy       #$5428              ; $87C4 18 CE 54 28

                    brset     $00,Y,#$02,L8829    ; $87C8 18 1E 00 02 5C

Loop@@              jsr       KickCOP             ; $87CD BD A7 DB
                    ldy       #$7C00              ; $87D0 18 CE 7C 00
                    brset     $01,Y,#$04,L8854    ; $87D4 18 1E 01 04 7B
                    brclr     $00,Y,#$01,Loop@@   ; $87D9 18 1F 00 01 EF
                    lda       $7C03               ; $87DE B6 7C 03

L87E1               anda      #$7F                ; $87E1 84 7F
                    sta       $549B               ; $87E3 B7 54 9B
                    bra       L8805               ; $87E6 20 1D

;*******************************************************************************

L87E8               proc
                    psha                          ; $87E8 36
                    pshb                          ; $87E9 37
                    pshx                          ; $87EA 3C

                    jsr       LA6F5               ; $87EB BD A6 F5
                    ldy       #$5428              ; $87EE 18 CE 54 28
                    brset     $00,Y,#$02,L8843    ; $87F2 18 1E 00 02 4C
                    ldy       #$7C00              ; $87F7 18 CE 7C 00
                    brclr     $00,Y,#$01,L8805    ; $87FB 18 1F 00 01 05
                    ldb       #255                ; $8800 C6 FF
                    stb       $5433               ; $8802 F7 54 33
L8805               sta       $7C03               ; $8805 B7 7C 03
                    ldy       #$5428              ; $8808 18 CE 54 28
                    brset     $00,Y,#$02,Done@@   ; $880C 18 1E 00 02 11

Loop@@              jsr       KickCOP             ; $8811 BD A7 DB
                    ldy       #$7C00              ; $8814 18 CE 7C 00
                    brclr     $00,Y,#$40,Loop@@   ; $8818 18 1F 00 40 F4
                    brset     $01,Y,#$04,L8854    ; $881D 18 1E 01 04 32

Done@@              pulx                          ; $8822 38
                    pulb                          ; $8823 33
                    pula                          ; $8824 32
                    jsr       LA771               ; $8825 BD A7 71
                    rts                           ; $8828 39

;*******************************************************************************

L8829               proc
                    jsr       KickCOP             ; $8829 BD A7 DB
                    ldy       #$7C00              ; $882C 18 CE 7C 00
                    brset     $00,Y,#$01,L886F    ; $8830 18 1E 00 01 3A
                    ldy       #$7C04              ; $8835 18 CE 7C 04
                    brclr     $00,Y,#$01,L8829    ; $8839 18 1F 00 01 EB
                    lda       $7C07               ; $883E B6 7C 07
                    bra       L87E1               ; $8841 20 9E

L8843               sta       $7C07               ; $8843 B7 7C 07
L8846               jsr       KickCOP             ; $8846 BD A7 DB
                    ldy       #$7C04              ; $8849 18 CE 7C 04
                    brclr     $00,Y,#$40,L8846    ; $884D 18 1F 00 40 F4
                    bra       L8805               ; $8852 20 B1

L8854               ldy       #$5428              ; $8854 18 CE 54 28
                    brclr     $00,Y,#$02,L8866    ; $8858 18 1F 00 02 09

                    clra                          ; $885D 4F
                    sta       $7C04               ; $885E B7 7C 04
                    lda       #$7F                ; $8861 86 7F
                    sta       $7C04               ; $8863 B7 7C 04

L8866               lda       $7C03               ; $8866 B6 7C 03
                    jsr       LA771               ; $8869 BD A7 71
                    jsr       LA801               ; $886C BD A8 01
L886F               clr       $5428               ; $886F 7F 54 28
                    jsr       LA771               ; $8872 BD A7 71
                    jmp       COMMON_ISR_RESET    ; $8875 7E 8E E3

;*******************************************************************************

L8878               proc
                    ldx       $5414               ; $8878 FE 54 14
                    ldb       #1                  ; $887B C6 01
                    clr       RegisterPointer     ; $887D 7F 54 00

                    cmpa      #'P'                ; $8880 81 50
                    bne       Y@@                 ; $8882 26 05
                    ldb       #8                  ; $8884 C6 08
                    inc       RegisterPointer     ; $8886 7C 54 00

Y@@                 cmpa      #'Y'                ; $8889 81 59
                    bne       X@@                 ; $888B 26 05
                    ldb       #6                  ; $888D C6 06
                    inc       RegisterPointer     ; $888F 7C 54 00

X@@                 cmpa      #'X'                ; $8892 81 58
                    bne       A@@                 ; $8894 26 05
                    ldb       #4                  ; $8896 C6 04
                    inc       RegisterPointer     ; $8898 7C 54 00

A@@                 cmpa      #'A'                ; $889B 81 41
                    bne       B@@                 ; $889D 26 02
                    ldb       #3                  ; $889F C6 03

B@@                 cmpa      #'B'                ; $88A1 81 42
                    bne       Done@@              ; $88A3 26 02
                    ldb       #2                  ; $88A5 C6 02

Done@@              abx                           ; $88A7 3A
                    stx       $5408               ; $88A8 FF 54 08
                    rts                           ; $88AB 39

;*******************************************************************************

L88AC               proc
                    sta       $549A               ; $88AC B7 54 9A
                    tab                           ; $88AF 16
                    adda      $542B               ; $88B0 BB 54 2B
                    sta       $542B               ; $88B3 B7 54 2B
                    tba                           ; $88B6 17
                    bsr       BCD2Hex_High        ; $88B7 8D 02
                    bra       BCD2Hex             ; $88B9 20 04

;*******************************************************************************

BCD2Hex_High        proc
                    lsra:4                        ; $88BB 44 44 44 44

;*******************************************************************************

BCD2Hex             proc
                    anda      #$0F                ; $88BF 84 0F
                    adda      #$90                ; $88C1 8B 90
                    daa                           ; $88C3 19
                    adca      #$40                ; $88C4 89 40
                    daa                           ; $88C6 19

                    jsr       L87E8               ; $88C7 BD 87 E8
                    lda       $549A               ; $88CA B6 54 9A
                    rts                           ; $88CD 39

;*******************************************************************************

L88CE               proc
                    lda       $5408               ; $88CE B6 54 08
                    jsr       L88AC               ; $88D1 BD 88 AC
                    lda       $5409               ; $88D4 B6 54 09
                    jsr       L88AC               ; $88D7 BD 88 AC
                    rts                           ; $88DA 39

;*******************************************************************************

L88DB               proc
                    lda       #$3D                ; $88DB 86 3D
                    jsr       L87E8               ; $88DD BD 87 E8
                    jsr       L89B4               ; $88E0 BD 89 B4
                    tst       RegisterPointer     ; $88E3 7D 54 00
                    beq       Done@@              ; $88E6 27 0D
                    jsr       L88AC               ; $88E8 BD 88 AC
                    ldx       $5408               ; $88EB FE 54 08
                    inx                           ; $88EE 08
                    stx       $5408               ; $88EF FF 54 08
                    jsr       L89B4               ; $88F2 BD 89 B4
Done@@              jsr       L88AC               ; $88F5 BD 88 AC
                    rts                           ; $88F8 39

;*******************************************************************************

PrintMessage        proc
                    lda       ,x                  ; $88F9 A6 00
                    beq       :AnRTS              ; $88FB 27 FB
                    jsr       L87E8               ; $88FD BD 87 E8
                    inx                           ; $8900 08
                    bra       PrintMessage        ; $8901 20 F6

;*******************************************************************************

PRINT_MSG_HOOK      proc
                    jsr       PrintMessage        ; $8903 BD 88 F9

;*******************************************************************************

L8906               proc
                    jsr       NewLine             ; $8906 BD 89 86
                    clr       RegisterPointer     ; $8909 7F 54 00
                    ldx       #D897F              ; $890C CE 89 7F
                    stx       $5406               ; $890F FF 54 06
Loop@@              lda       ,x                  ; $8912 A6 00
                    beq       L8968               ; $8914 27 52
                    sta       $54BC               ; $8916 B7 54 BC
                    jsr       L87E8               ; $8919 BD 87 E8
                    jsr       L8878               ; $891C BD 88 78
                    jsr       L88DB               ; $891F BD 88 DB
                    lda       $54BC               ; $8922 B6 54 BC
                    cmpa      #$43                ; $8925 81 43
                    bne       Skip@@              ; $8927 26 02
                    bsr       L8942               ; $8929 8D 17
Skip@@              ldx       #MsgSpaces4         ; $892B CE B4 E2
                    jsr       PrintMessage        ; $892E BD 88 F9
                    ldx       $5406               ; $8931 FE 54 06
                    inx                           ; $8934 08
                    stx       $5406               ; $8935 FF 54 06
                    bra       Loop@@              ; $8938 20 D8

;*******************************************************************************

msgCCRBITS          fcc       'SXHINZVC'          ; $893A 53 58 48 49 4E 5A 56 43

L8942               proc
                    ldx       #MsgSpaces2         ; $8942 CE B4 E4
                    jsr       PrintMessage        ; $8945 BD 88 F9
                    ldx       #msgCCRBITS         ; $8948 CE 89 3A
                    lda       #8                  ; $894B 86 08
                    sta       $54BC               ; $894D B7 54 BC
Loop@@              ldb       $549A               ; $8950 F6 54 9A
                    lda       #'.'                ; $8953 86 2E
                    clc                           ; $8955 0C
                    aslb                          ; $8956 58
                    stb       $549A               ; $8957 F7 54 9A
                    bcc       Skip@@              ; $895A 24 02
                    lda       ,x                  ; $895C A6 00
Skip@@              jsr       L87E8               ; $895E BD 87 E8
                    inx                           ; $8961 08
                    dec       $54BC               ; $8962 7A 54 BC
                    bne       Loop@@              ; $8965 26 E9
                    rts                           ; $8967 39

;*******************************************************************************

L8968               proc
                    lda       #$53                ; $8968 86 53
                    jsr       L87E8               ; $896A BD 87 E8
                    lda       #$3D                ; $896D 86 3D
                    jsr       L87E8               ; $896F BD 87 E8
                    ldd       $5414               ; $8972 FC 54 14
                    addd      #9                  ; $8975 C3 00 09
                    std       $5408               ; $8978 FD 54 08
                    jsr       L88CE               ; $897B BD 88 CE
                    rts                           ; $897E 39

;*******************************************************************************
D897F               fcs       'PYXABC'            ; $897F 50 59 58 41 42 43 00
;*******************************************************************************

NewLine             lda       #CR                 ; $8986 86 0D
                    jsr       L87E8               ; $8988 BD 87 E8
                    lda       #LF                 ; $898B 86 0A
                    jsr       L87E8               ; $898D BD 87 E8
                    rts                           ; $8990 39

;*******************************************************************************

L8991               proc
                    sta       $5402               ; $8991 B7 54 02
                    lda       #$B7                ; $8994 86 B7
                    bsr       L89BE               ; $8996 8D 26
                    bsr       L89B4               ; $8998 8D 1A
                    cmpa      $5402               ; $899A B1 54 02
                    beq       Exit@@              ; $899D 27 14
                    jsr       NewLine             ; $899F BD 89 86
                    jsr       L88CE               ; $89A2 BD 88 CE
                    ldx       #MsgBadMemoryError  ; $89A5 CE B4 E7
                    jsr       PrintMessage        ; $89A8 BD 88 F9
                    jsr       NewLine             ; $89AB BD 89 86
                    lda       #7                  ; $89AE 86 07
                    jsr       L87E8               ; $89B0 BD 87 E8
Exit@@              rts                           ; $89B3 39

;*******************************************************************************

L89B4               proc
                    lda       $5443               ; $89B4 B6 54 43
                    bne       Skip@@              ; $89B7 26 03
                    jsr       KickCOP             ; $89B9 BD A7 DB
Skip@@              lda       #$B6                ; $89BC 86 B6

;*******************************************************************************

L89BE               proc
                    pshb                          ; $89BE 37
                    ldb       $55FF               ; $89BF F6 55 FF
                    andb      #$FB                ; $89C2 C4 FB
                    stb       $55FF               ; $89C4 F7 55 FF
                    sta       $5421               ; $89C7 B7 54 21
                    orb       #$02                ; $89CA CA 02

                    ldx       #$E700              ; $89CC CE E7 00
                    stx       JSR_Hook            ; $89CF FF 54 1F

                    ldx       $5408               ; $89D2 FE 54 08
                    stx       $5422               ; $89D5 FF 54 22
                    lda       #$39                ; $89D8 86 39
                    sta       $5424               ; $89DA B7 54 24
                    lda       $5402               ; $89DD B6 54 02

                    ldx       #$55FF              ; $89E0 CE 55 FF
                    jsr       JSR_Hook            ; $89E3 BD 54 1F

                    ldb       $55FF               ; $89E6 F6 55 FF
                    andb      #$FD                ; $89E9 C4 FD
                    orb       #$04                ; $89EB CA 04
                    stb       ,x                  ; $89ED E7 00
                    pulb                          ; $89EF 33
                    rts                           ; $89F0 39

;*******************************************************************************

L89F1               proc
                    ldx       $5414               ; $89F1 FE 54 14
                    abx                           ; $89F4 3A
                    stx       $5408               ; $89F5 FF 54 08
                    rts                           ; $89F8 39

;*******************************************************************************

L89F9               proc
                    bsr       L89F1               ; $89F9 8D F6

;*******************************************************************************

L89FB               proc
                    jsr       L89B4               ; $89FB BD 89 B4
                    sta       $540A               ; $89FE B7 54 0A
                    ldx       $5408               ; $8A01 FE 54 08
                    inx                           ; $8A04 08
                    stx       $5408               ; $8A05 FF 54 08
L8A08               jsr       L89B4               ; $8A08 BD 89 B4
                    sta       $540B               ; $8A0B B7 54 0B
                    rts                           ; $8A0E 39

;*******************************************************************************

L8A0F               proc
                    ldx       #$FFF6              ; $8A0F CE FF F6

;*******************************************************************************

L8A12               proc
                    stx       $5408               ; $8A12 FF 54 08
                    bsr       L89FB               ; $8A15 8D E4
                    jsr       L8A4F               ; $8A17 BD 8A 4F
                    rts                           ; $8A1A 39

;*******************************************************************************
; DEAD CODE ???
                    bsr       L89F1               ; $8A1B 8D D4
                    jsr       L89B4               ; $8A1D BD 89 B4
                    rts                           ; $8A20 39

;*******************************************************************************

L8A21               proc
                    bsr       L89F1               ; $8A21 8D CE
                    lda       $540A               ; $8A23 B6 54 0A
                    jsr       L8991               ; $8A26 BD 89 91
                    rts                           ; $8A29 39

;*******************************************************************************

L8A2A               proc
                    ldb       #$08                ; $8A2A C6 08

;*******************************************************************************

L8A2C               proc
                    bsr       L89F9               ; $8A2C 8D CB
                    jsr       L8A4F               ; $8A2E BD 8A 4F
                    rts                           ; $8A31 39

;*******************************************************************************

L8A32               proc
                    ldb       #$08                ; $8A32 C6 08
                    bsr       L89F1               ; $8A34 8D BB
                    lda       $540A               ; $8A36 B6 54 0A
                    jsr       L8991               ; $8A39 BD 89 91
                    jsr       L8A58               ; $8A3C BD 8A 58
                    lda       $540B               ; $8A3F B6 54 0B
                    jsr       L8991               ; $8A42 BD 89 91
                    rts                           ; $8A45 39

;*******************************************************************************

L8A46               proc
                    ldx       #$540A              ; $8A46 CE 54 0A

;*******************************************************************************

L8A49               proc
                    ldd       $5408               ; $8A49 FC 54 08
                    std       ,x                  ; $8A4C ED 00
                    rts                           ; $8A4E 39

;*******************************************************************************

L8A4F               proc
                    ldx       #$540A              ; $8A4F CE 54 0A
                    ldd       ,x                  ; $8A52 EC 00
                    std       $5408               ; $8A54 FD 54 08
                    rts                           ; $8A57 39

;*******************************************************************************

L8A58               proc
                    lda       #1                  ; $8A58 86 01

;*******************************************************************************

L8A5A               proc
                    tab                           ; $8A5A 16
                    ldx       $5408               ; $8A5B FE 54 08
                    abx                           ; $8A5E 3A
                    stx       $5408               ; $8A5F FF 54 08
                    rts                           ; $8A62 39

;*******************************************************************************

L8A63               lda       #1                  ; $8A63 86 01
                    sta       $5403               ; $8A65 B7 54 03
                    clr       $5402               ; $8A68 7F 54 02
                    ldd       $5408               ; $8A6B FC 54 08
                    subd      $5402               ; $8A6E B3 54 02
                    std       $5408               ; $8A71 FD 54 08
                    rts                           ; $8A74 39

;*******************************************************************************

L8A75               proc
                    ldd       $540A               ; $8A75 FC 54 0A
                    subd      $5408               ; $8A78 B3 54 08
                    bne       One@@               ; $8A7B 26 02
                    clra                          ; $8A7D 4F
                    rts                           ; $8A7E 39

One@@               lda       #1                  ; $8A7F 86 01
                    rts                           ; $8A81 39

;*******************************************************************************

L8A82               proc
                    ldx       #$54A2              ; $8A82 CE 54 A2

;*******************************************************************************

L8A85               proc
                    clr       ,x                  ; $8A85 6F 00
                    inx                           ; $8A87 08
                    cmpx      #$54B0              ; $8A88 8C 54 B0
                    bne       L8A85               ; $8A8B 26 F8
                    rts                           ; $8A8D 39

;*******************************************************************************

L8A8E               proc
                    ldx       #$54A2              ; $8A8E CE 54 A2
                    ldb       RegisterPointer     ; $8A91 F6 54 00
                    abx                           ; $8A94 3A
                    ldd       ,x                  ; $8A95 EC 00
                    inc:2     RegisterPointer     ; $8A97 7C 54 00 7C 54 00
                    std       $5408               ; $8A9D FD 54 08
                    rts                           ; $8AA0 39

;*******************************************************************************

L8AA1               proc
                    lda       #$08                ; $8AA1 86 08

;*******************************************************************************

L8AA3               proc
                    sta       $5401               ; $8AA3 B7 54 01
                    clr       $549A               ; $8AA6 7F 54 9A
                    ldx       #$54A2              ; $8AA9 CE 54 A2
                    ldd       $5408               ; $8AAC FC 54 08
                    subd      ,x                  ; $8AAF A3 00
                    bne       $8AB4               ; $8AB1 26 01
                    rts                           ; $8AB3 39

;*******************************************************************************
; DEAD CODE ???
                    inx:2                         ; $8AB4 08 08
                    ldb       $549A               ; $8AB6 F6 54 9A
                    incb:2                        ; $8AB9 5C 5C
                    stb       $549A               ; $8ABB F7 54 9A
                    cmpb      $5401               ; $8ABE F1 54 01
                    bls       $8AAC               ; $8AC1 23 E9
                    rts                           ; $8AC3 39

;*******************************************************************************

L8AC4               clrb                          ; $8AC4 5F
                    lda       #$08                ; $8AC5 86 08
L8AC7               sta       $5401               ; $8AC7 B7 54 01
                    stb       RegisterPointer     ; $8ACA F7 54 00
L8ACD               bsr       L8A8E               ; $8ACD 8D BF
                    beq       L8AEB               ; $8ACF 27 1A
                    ldb       $55FF               ; $8AD1 F6 55 FF
                    orb       #$08                ; $8AD4 CA 08
                    stb       $55FF               ; $8AD6 F7 55 FF
                    jsr       L89B4               ; $8AD9 BD 89 B4
                    ldb       RegisterPointer     ; $8ADC F6 54 00
                    lsrb                          ; $8ADF 54
                    ldx       #$54B0              ; $8AE0 CE 54 B0
                    abx                           ; $8AE3 3A
                    sta       ,x                  ; $8AE4 A7 00
                    lda       #$3F                ; $8AE6 86 3F
                    jsr       L8991               ; $8AE8 BD 89 91
L8AEB               ldb       RegisterPointer     ; $8AEB F6 54 00
                    cmpb      $5401               ; $8AEE F1 54 01
                    bls       L8ACD               ; $8AF1 23 DA
                    jmp       L9AFF               ; $8AF3 7E 9A FF

;*******************************************************************************

L8AF6               proc
                    ldb       #$08                ; $8AF6 C6 08
                    clra                          ; $8AF8 4F

;*******************************************************************************

L8AF9               proc
                    stb       RegisterPointer     ; $8AF9 F7 54 00
                    sta       $5401               ; $8AFC B7 54 01
Loop@@              bsr       L8A8E               ; $8AFF 8D 8D
                    beq       Skip@@              ; $8B01 27 0D
                    ldb       RegisterPointer     ; $8B03 F6 54 00
                    lsrb                          ; $8B06 54
                    ldx       #$54B0              ; $8B07 CE 54 B0
                    abx                           ; $8B0A 3A
                    lda       ,x                  ; $8B0B A6 00
                    jsr       L8991               ; $8B0D BD 89 91
Skip@@              lda       RegisterPointer     ; $8B10 B6 54 00
                    suba      #4                  ; $8B13 80 04
                    sta       RegisterPointer     ; $8B15 B7 54 00
                    cmpa      $5401               ; $8B18 B1 54 01
                    bpl       Loop@@              ; $8B1B 2A E2
                    rts                           ; $8B1D 39

;*******************************************************************************
; The following is DATA
;*******************************************************************************

D8B1E               fcc       'ABEKNQRU'          ; $8B1E 41 42 45 4B 4E 51 52 55
                    fcc       $5B,$5E             ; $8B26 5B 5E
                    fcc       'abekqru'           ; $8B28 61 62 65 6B 71 72 75
                    fcb       $7B,$87,$C7         ; $8B2F 7B 87 C7

; The following is possibly DATA

D8B32               fdb       $1213               ; $8B32 12 13
                    fdb       $1415               ; $8B34 14 15
                    fdb       $1C1D               ; $8B36 1C 1D
                    fdb       $1E1F               ; $8B38 1E 1F
                    fdb       $0809               ; $8B3A 08 09
                    fdb       $3035               ; $8B3D 30 35
                    fdb       $383A               ; $8B3E 38 3A
                    fdb       $3C8F               ; $8B40 3C 8F
                    fdb       $8C9C               ; $8B42 8C 9C
                    fdb       $BCCE               ; $8B44 BC CE
                    fdb       $DEDF               ; $8B46 DE DF
                    fdb       $FEFF               ; $8B48 FE FF
                    fdb       $838C               ; $8B4A 83 8C
                    fdb       $8EC3               ; $8B4B 8E C3
                    fdb       $CCCE               ; $8B4E CC CE
; END OF DATA

L8B50               ldx       #$8B1E              ; $8B50 CE 8B 1E
L8B53               cmpa      ,x                  ; $8B53 A1 00
                    bne       L8B5C               ; $8B55 26 05
                    inc       NestLevel           ; $8B57 7C 54 05
                    bra       L8B62               ; $8B5A 20 06

L8B5C               inx                           ; $8B5C 08
                    cmpx      $54B9               ; $8B5D BC 54 B9
                    bne       L8B53               ; $8B60 26 F1
L8B62               rts                           ; $8B62 39

L8B63               clr       $5416               ; $8B63 7F 54 16
                    clr       $542C               ; $8B66 7F 54 2C
                    jsr       L89B4               ; $8B69 BD 89 B4
                    sta       $5401               ; $8B6C B7 54 01
                    ldx       #D8B32              ; $8B6F CE 8B 32
                    stx       $54B9               ; $8B72 FF 54 B9
                    bsr       L8B50               ; $8B75 8D D9
                    beq       L8BCF               ; $8B77 27 56

L8B79               inc       NestLevel           ; $8B79 7C 54 05
                    rts                           ; $8B7C 39

L8B7D               lda       #$02                ; $8B7D 86 02
                    sta       $54BD               ; $8B7F B7 54 BD
                    jsr       L8B8D               ; $8B82 BD 8B 8D
                    deca                          ; $8B85 4A
                    sta       $5416               ; $8B86 B7 54 16
                    inca                          ; $8B89 4C
                    jmp       L8DFE               ; $8B8A 7E 8D FE

L8B8D               ldd       $5408               ; $8B8D FC 54 08
                    std       $549C               ; $8B90 FD 54 9C
                    lda       $54BD               ; $8B93 B6 54 BD
                    tst       $542C               ; $8B96 7D 54 2C
                    bpl       $8B9C               ; $8B99 2A 01
                    inca                          ; $8B9B 4C
                    jsr       L8A5A               ; $8B9C BD 8A 5A
                    ldx       #$54AE              ; $8B9F CE 54 AE
                    jsr       L8A49               ; $8BA2 BD 8A 49
                    jsr       L8A63               ; $8BA5 BD 8A 63
                    jsr       L89B4               ; $8BA8 BD 89 B4
                    tab                           ; $8BAB 16
                    clra                          ; $8BAC 4F
                    std       $540A               ; $8BAD FD 54 0A
                    ldd       $549C               ; $8BB0 FC 54 9C
                    tst       $540B               ; $8BB3 7D 54 0B
                    bmi       $8BC7               ; $8BB6 2B 0F
                    addd      $540A               ; $8BB8 F3 54 0A
L8BBB               std       $5408               ; $8BBB FD 54 08
                    lda       #$02                ; $8BBE 86 02
                    ora       $542C               ; $8BC0 BA 54 2C
                    sta       $542C               ; $8BC3 B7 54 2C
                    rts                           ; $8BC6 39

                    neg       $540B               ; $8BC7 70 54 0B
                    subd      $540A               ; $8BCA B3 54 0A
                    bra       L8BBB               ; $8BCD 20 EC

L8BCF               tab                           ; $8BCF 16
                    cmpb      #$CD                ; $8BD0 C1 CD
                    bne       L8BF1               ; $8BD2 26 1D
                    bsr       L8C28               ; $8BD4 8D 52
                    cmpa      #$A3                ; $8BD6 81 A3
                    beq       L8BE9               ; $8BD8 27 0F
                    cmpa      #$AC                ; $8BDA 81 AC
                    beq       L8BE9               ; $8BDC 27 0B
                    cmpa      #$EE                ; $8BDE 81 EE
                    beq       L8BE9               ; $8BE0 27 07
                    cmpa      #$EF                ; $8BE2 81 EF
                    beq       L8BE9               ; $8BE4 27 03
                    jmp       L8C9F               ; $8BE6 7E 8C 9F

L8BE9               lda       #$C0                ; $8BE9 86 C0
                    sta       $542C               ; $8BEB B7 54 2C
                    jmp       L8DF5               ; $8BEE 7E 8D F5

L8BF1               cmpb      #$1A                ; $8BF1 C1 1A
                    bne       L8C38               ; $8BF3 26 43
                    bsr       L8C28               ; $8BF5 8D 31
                    tab                           ; $8BF7 16
                    andb      #$F0                ; $8BF8 C4 F0
                    cmpb      #$A0                ; $8BFA C1 A0
                    beq       L8C24               ; $8BFC 27 26
                    cmpb      #$E0                ; $8BFE C1 E0
                    beq       L8C24               ; $8C00 27 22
                    cmpb      #$B0                ; $8C02 C1 B0
                    bne       L8C14               ; $8C04 26 0E
                    lda       #$88                ; $8C06 86 88
                    ldb       #$03                ; $8C08 C6 03
L8C0A               sta       $542C               ; $8C0A B7 54 2C
                    stb       $5416               ; $8C0D F7 54 16
                    tba                           ; $8C10 17
                    jmp       L8DFD               ; $8C11 7E 8D FD

L8C14               cmpb      #$90                ; $8C14 C1 90
                    bne       L8C1E               ; $8C16 26 06
                    lda       #$A0                ; $8C18 86 A0
L8C1A               ldb       #$02                ; $8C1A C6 02
                    bra       L8C0A               ; $8C1C 20 EC

L8C1E               lda       #$90                ; $8C1E 86 90
                    ldb       #$03                ; $8C20 C6 03
                    bra       L8C0A               ; $8C22 20 E6

L8C24               lda       #$84                ; $8C24 86 84
                    bra       L8C1A               ; $8C26 20 F2

L8C28               jsr       L8A58               ; $8C28 BD 8A 58
                    jsr       L89B4               ; $8C2B BD 89 B4
                    sta       $5401               ; $8C2E B7 54 01
                    jsr       L8A63               ; $8C31 BD 8A 63
                    lda       $5401               ; $8C34 B6 54 01
                    rts                           ; $8C37 39

L8C38               cmpb      #$18                ; $8C38 C1 18
                    bne       L8CA2               ; $8C3A 26 66
                    lda       #$80                ; $8C3C 86 80
                    sta       $542C               ; $8C3E B7 54 2C
                    bsr       L8C28               ; $8C41 8D E5
                    ldx       #$8B42              ; $8C43 CE 8B 42
                    stx       $54B9               ; $8C46 FF 54 B9
                    ldx       #$8B3A              ; $8C49 CE 8B 3A
                    jsr       L8B53               ; $8C4C BD 8B 53
                    beq       $8C5C               ; $8C4F 27 0B
                    clr       NestLevel           ; $8C51 7F 54 05
                    ldb       #$81                ; $8C54 C6 81
                    stb       $542C               ; $8C56 F7 54 2C
                    jmp       $8DB2               ; $8C59 7E 8D B2

                    inc       $5416               ; $8C5C 7C 54 16
                    tab                           ; $8C5F 16
                    cmpa      #$6E                ; $8C60 81 6E
                    beq       $8C94               ; $8C62 27 30
                    cmpa      #$AD                ; $8C64 81 AD
                    beq       $8C94               ; $8C66 27 2C
                    andb      #$F0                ; $8C68 C4 F0
                    cmpb      #$A0                ; $8C6A C1 A0
                    beq       $8C9A               ; $8C6C 27 2C
                    cmpb      #$E0                ; $8C6E C1 E0
                    beq       $8C9A               ; $8C70 27 28
                    ldx       #D8B32              ; $8C72 CE 8B 32
                    stx       $54B9               ; $8C75 FF 54 B9
                    jsr       L8B50               ; $8C78 BD 8B 50
                    bne       L8C9F               ; $8C7B 26 22
                    cmpb      #$60                ; $8C7D C1 60
                    beq       $8C9A               ; $8C7F 27 19
                    ldx       #$8B4A              ; $8C81 CE 8B 4A
                    stx       $54B9               ; $8C84 FF 54 B9
                    ldx       #$8B36              ; $8C87 CE 8B 36
                    jsr       L8B53               ; $8C8A BD 8B 53
                    beq       L8C9F               ; $8C8D 27 10
                    clr       NestLevel           ; $8C8F 7F 54 05
                    bra       L8CA2               ; $8C92 20 0E

                    jsr       L8A58               ; $8C94 BD 8A 58
                    jmp       L8D5F               ; $8C97 7E 8D 5F

                    lda       #$C0                ; $8C9A 86 C0
L8C9C               jmp       L8DA3               ; $8C9C 7E 8D A3

L8C9F               jmp       L8B79               ; $8C9F 7E 8B 79

L8CA2               ldx       #D8B32              ; $8CA2 CE 8B 32
L8CA5               ldb       ,x                  ; $8CA5 E6 00
                    cmpb      $5401               ; $8CA7 F1 54 01
                    beq       L8CB4               ; $8CAA 27 08
                    inx                           ; $8CAC 08
                    cmpx      #$8B3A              ; $8CAD 8C 8B 3A
                    bne       L8CA5               ; $8CB0 26 F3
                    bra       L8CFC               ; $8CB2 20 48

L8CB4               andb      #$02                ; $8CB4 C4 02
                    beq       L8CE9               ; $8CB6 27 31
                    lda       #$04                ; $8CB8 86 04
                    sta       $54BC               ; $8CBA B7 54 BC
                    sta       $54BD               ; $8CBD B7 54 BD
                    jsr       L8B8D               ; $8CC0 BD 8B 8D
                    lda       #$22                ; $8CC3 86 22
                    jsr       L8CCB               ; $8CC5 BD 8C CB
                    jmp       L8DF7               ; $8CC8 7E 8D F7

L8CCB               ldb       $5401               ; $8CCB F6 54 01
                    andb      #$08                ; $8CCE C4 08
                    beq       L8CD4               ; $8CD0 27 02
                    lda       #$04                ; $8CD2 86 04
L8CD4               sta       $54BD               ; $8CD4 B7 54 BD
                    lda       #$03                ; $8CD7 86 03
                    ldb       $542C               ; $8CD9 F6 54 2C
                    bpl       L8CE2               ; $8CDC 2A 04
                    orb       #$C0                ; $8CDE CA C0
                    bra       L8CE5               ; $8CE0 20 03

L8CE2               orb       $54BD               ; $8CE2 FA 54 BD
L8CE5               stb       $542C               ; $8CE5 F7 54 2C
                    rts                           ; $8CE8 39

L8CE9               lda       #$20                ; $8CE9 86 20
                    jsr       L8CCB               ; $8CEB BD 8C CB
                    cmpb      #$C0                ; $8CEE C1 C0
                    beq       L8CF3               ; $8CF0 27 01
                    deca                          ; $8CF2 4A
L8CF3               sta       $5416               ; $8CF3 B7 54 16
                    inc       $54BC               ; $8CF6 7C 54 BC
                    jmp       L8DFD               ; $8CF9 7E 8D FD

L8CFC               lda       $5401               ; $8CFC B6 54 01
                    cmpa      #$8F                ; $8CFF 81 8F
                    beq       L8D5D               ; $8D01 27 5A
                    cmpa      #$CF                ; $8D03 81 CF
                    beq       L8D5D               ; $8D05 27 56
                    anda      #$F0                ; $8D07 84 F0
                    cmpa      #$70                ; $8D09 81 70
                    beq       L8D89               ; $8D0B 27 7C
                    cmpa      #$B0                ; $8D0D 81 B0
                    beq       L8D89               ; $8D0F 27 78
                    cmpa      #$F0                ; $8D11 81 F0
                    beq       L8D89               ; $8D13 27 74
                    cmpa      #$20                ; $8D15 81 20
                    bne       $8D1C               ; $8D17 26 03
                    jmp       L8B7D               ; $8D19 7E 8B 7D

                    cmpa      #$50                ; $8D1C 81 50
                    bls       L8D5D               ; $8D1E 23 3D
                    lda       $5401               ; $8D20 B6 54 01
                    ldx       #L8B50              ; $8D23 CE 8B 50
                    stx       $54B9               ; $8D26 FF 54 B9
                    ldx       #$8B4A              ; $8D29 CE 8B 4A
                    jsr       L8B53               ; $8D2C BD 8B 53
                    beq       L8D36               ; $8D2F 27 05
                    clr       NestLevel           ; $8D31 7F 54 05
                    bra       L8D90               ; $8D34 20 5A

L8D36               cmpa      #$8D                ; $8D36 81 8D
                    beq       L8D5A               ; $8D38 27 20
                    cmpa      #$9D                ; $8D3A 81 9D
                    beq       L8DB6               ; $8D3C 27 78
                    cmpa      #$AD                ; $8D3E 81 AD
                    beq       L8D5F               ; $8D40 27 1D
                    cmpa      #$6E                ; $8D42 81 6E
                    beq       L8D5F               ; $8D44 27 19
                    anda      #$F0                ; $8D46 84 F0
                    cmpa      #$80                ; $8D48 81 80
                    beq       L8D9A               ; $8D4A 27 4E
                    cmpa      #$C0                ; $8D4C 81 C0
                    beq       L8D9A               ; $8D4E 27 4A
                    cmpa      #$90                ; $8D50 81 90
                    beq       L8DAA               ; $8D52 27 56
                    cmpa      #$D0                ; $8D54 81 D0
                    beq       L8DAA               ; $8D56 27 52
                    bra       L8D9E               ; $8D58 20 44

L8D5A               jmp       L8B7D               ; $8D5A 7E 8B 7D

L8D5D               bra       L8DCF               ; $8D5D 20 70

L8D5F               lda       #$04                ; $8D5F 86 04
                    tst       $542C               ; $8D61 7D 54 2C
                    bpl       L8D68               ; $8D64 2A 02
                    lda       #$C0                ; $8D66 86 C0
L8D68               sta       $542C               ; $8D68 B7 54 2C
                    jsr       L8A58               ; $8D6B BD 8A 58
                    jsr       L89B4               ; $8D6E BD 89 B4
                    psha                          ; $8D71 36
                    ldb       #$04                ; $8D72 C6 04
                    tst       $542C               ; $8D74 7D 54 2C
                    bpl       L8D7B               ; $8D77 2A 02
                    ldb       #$06                ; $8D79 C6 06
L8D7B               jsr       L89F9               ; $8D7B BD 89 F9
                    pula                          ; $8D7E 32
                    tab                           ; $8D7F 16
                    clra                          ; $8D80 4F
                    addd      $540A               ; $8D81 F3 54 0A
                    std       $5408               ; $8D84 FD 54 08
                    bra       L8DCA               ; $8D87 20 41

L8D89               ldb       $542C               ; $8D89 F6 54 2C
                    orb       #$08                ; $8D8C CA 08
                    bra       L8D95               ; $8D8E 20 05

L8D90               ldb       $542C               ; $8D90 F6 54 2C
                    orb       #$10                ; $8D93 CA 10
L8D95               stb       $542C               ; $8D95 F7 54 2C
                    bra       L8DEA               ; $8D98 20 50

L8D9A               lda       #$10                ; $8D9A 86 10
                    bra       L8DA3               ; $8D9C 20 05

L8D9E               lda       $542C               ; $8D9E B6 54 2C
                    ora       #$04                ; $8DA1 8A 04
L8DA3               sta       $542C               ; $8DA3 B7 54 2C
                    lda       #$01                ; $8DA6 86 01
                    bra       L8DF7               ; $8DA8 20 4D

L8DAA               lda       $542C               ; $8DAA B6 54 2C
                    ora       #$20                ; $8DAD 8A 20
                    sta       $542C               ; $8DAF B7 54 2C
                    lda       #$01                ; $8DB2 86 01
                    bra       L8DF7               ; $8DB4 20 41

L8DB6               jsr       L8A58               ; $8DB6 BD 8A 58
                    jsr       L8A08               ; $8DB9 BD 8A 08
                    clr       $540A               ; $8DBC 7F 54 0A
                    jsr       L8A4F               ; $8DBF BD 8A 4F
                    lda       $542C               ; $8DC2 B6 54 2C
                    ora       #$20                ; $8DC5 8A 20
                    sta       $542C               ; $8DC7 B7 54 2C
L8DCA               inc       $5416               ; $8DCA 7C 54 16
                    bra       L8E01               ; $8DCD 20 32

L8DCF               lda       #$01                ; $8DCF 86 01
                    sta       $542C               ; $8DD1 B7 54 2C
                    clr       $5416               ; $8DD4 7F 54 16
                    lda       $5401               ; $8DD7 B6 54 01
                    cmpa      #$39                ; $8DDA 81 39
                    beq       L8E18               ; $8DDC 27 3A
                    cmpa      #$3B                ; $8DDE 81 3B
                    beq       L8E0D               ; $8DE0 27 2B
                    cmpa      #$3F                ; $8DE2 81 3F
                    beq       L8E08               ; $8DE4 27 22
                    lda       #$01                ; $8DE6 86 01
                    bra       L8DFE               ; $8DE8 20 14

L8DEA               lda       $5401               ; $8DEA B6 54 01
                    cmpa      #$BD                ; $8DED 81 BD
                    beq       L8E1F               ; $8DEF 27 2E
                    cmpa      #$7E                ; $8DF1 81 7E
                    beq       L8E1F               ; $8DF3 27 2A
L8DF5               lda       #$02                ; $8DF5 86 02
L8DF7               adda      $5416               ; $8DF7 BB 54 16
                    sta       $5416               ; $8DFA B7 54 16
L8DFD               inca                          ; $8DFD 4C
L8DFE               jsr       L8A5A               ; $8DFE BD 8A 5A
L8E01               ldx       #$54AC              ; $8E01 CE 54 AC
                    jsr       L8A49               ; $8E04 BD 8A 49
                    rts                           ; $8E07 39

L8E08               jsr       L8A0F               ; $8E08 BD 8A 0F
                    bra       L8E01               ; $8E0B 20 F4

L8E0D               ldx       $5414               ; $8E0D FE 54 14
                    ldb       #$11                ; $8E10 C6 11
                    abx                           ; $8E12 3A
                    jsr       L8A12               ; $8E13 BD 8A 12
                    bra       L8E01               ; $8E16 20 E9

L8E18               ldb       #$0A                ; $8E18 C6 0A
                    jsr       L8A2C               ; $8E1A BD 8A 2C
                    bra       L8E01               ; $8E1D 20 E2

L8E1F               jsr       L8A58               ; $8E1F BD 8A 58
                    jsr       L89FB               ; $8E22 BD 89 FB
                    jsr       L8A4F               ; $8E25 BD 8A 4F
                    lda       $542C               ; $8E28 B6 54 2C
                    ora       #$08                ; $8E2B 8A 08
                    sta       $542C               ; $8E2D B7 54 2C
                    lda       #$02                ; $8E30 86 02
                    adda      $5416               ; $8E32 BB 54 16
                    sta       $5416               ; $8E35 B7 54 16
                    bra       L8E01               ; $8E38 20 C7

L8E3A               jsr       NewLine             ; $8E3A BD 89 86
L8E3D               lda       #$3E                ; $8E3D 86 3E
                    jsr       L87E8               ; $8E3F BD 87 E8
                    ldx       #$5444              ; $8E42 CE 54 44
L8E45               jsr       L87BE               ; $8E45 BD 87 BE
                    lda       $549B               ; $8E48 B6 54 9B
                    cmpa      #$18                ; $8E4B 81 18
                    beq       L8E3A               ; $8E4D 27 EB
                    cmpa      #$08                ; $8E4F 81 08
                    bne       L8E5B               ; $8E51 26 08
                    dex                           ; $8E53 09
                    cmpx      #$5444              ; $8E54 8C 54 44
                    blt       L8E3D               ; $8E57 2D E4
                    bra       L8E45               ; $8E59 20 EA

L8E5B               sta       ,x                  ; $8E5B A7 00
                    inx                           ; $8E5D 08
                    cmpx      #$5494              ; $8E5E 8C 54 94
                    beq       L8E67               ; $8E61 27 04
                    cmpa      #$0D                ; $8E63 81 0D
                    bne       L8E45               ; $8E65 26 DE
L8E67               lda       #$0D                ; $8E67 86 0D
                    sta       ,x                  ; $8E69 A7 00
                    clr       $5418               ; $8E6B 7F 54 18
                    rts                           ; $8E6E 39

L8E6F               cmpa      #'a'-1              ; $8E6F 81 60
                    bls       L8E78               ; $8E71 23 05
                    suba      #'a'-'A'            ; $8E73 80 20
                    sta       $5401               ; $8E75 B7 54 01
L8E78               rts                           ; $8E78 39

L8E79               pshb                          ; $8E79 37
                    pshx                          ; $8E7A 3C
                    ldx       #$5444              ; $8E7B CE 54 44
                    ldb       $5418               ; $8E7E F6 54 18
                    abx                           ; $8E81 3A
                    lda       ,x                  ; $8E82 A6 00
                    inc       $5418               ; $8E84 7C 54 18
                    sta       $5401               ; $8E87 B7 54 01
                    pulx                          ; $8E8A 38
                    pulb                          ; $8E8B 33
                    rts                           ; $8E8C 39

L8E8D               lda       #$FD                ; $8E8D 86 FD
                    clrb                          ; $8E8F 5F
                    sta       $54BD               ; $8E90 B7 54 BD
                    clra                          ; $8E93 4F
                    std       $5412               ; $8E94 FD 54 12
                    bsr       L8E79               ; $8E97 8D E0
                    cmpa      #$24                ; $8E99 81 24
                    bne       L8E9F               ; $8E9B 26 02
L8E9D               bsr       L8E79               ; $8E9D 8D DA
L8E9F               jsr       L8E6F               ; $8E9F BD 8E 6F
                    inc       $54BD               ; $8EA2 7C 54 BD
                    ldb       #$FF                ; $8EA5 C6 FF
                    stb       $542D               ; $8EA7 F7 54 2D
                    suba      #$30                ; $8EAA 80 30
                    bmi       L8ED7               ; $8EAC 2B 29
                    cmpa      #$09                ; $8EAE 81 09
                    bls       L8EBC               ; $8EB0 23 0A
                    suba      #$07                ; $8EB2 80 07
                    cmpa      #$09                ; $8EB4 81 09
                    bls       L8ED7               ; $8EB6 23 1F
                    cmpa      #$0F                ; $8EB8 81 0F
                    bhi       L8ED7               ; $8EBA 22 1B
L8EBC               sta       $542D               ; $8EBC B7 54 2D
                    ldd       $5412               ; $8EBF FC 54 12
                    asld:4                        ; $8EC2 05 05 05 05
                    sta       $5412               ; $8EC6 B7 54 12
                    tba                           ; $8EC9 17
                    ora       $542D               ; $8ECA BA 54 2D
                    sta       $5413               ; $8ECD B7 54 13
                    tst       $5428               ; $8ED0 7D 54 28
                    bne       L8ED7               ; $8ED3 26 02
                    bra       L8E9D               ; $8ED5 20 C6

L8ED7               inc       $5417               ; $8ED7 7C 54 17
                    rts                           ; $8EDA 39

L8EDB               jsr       L8E3A               ; $8EDB BD 8E 3A
                    bra       L8F0D               ; $8EDE 20 2D

L8EE0               inc       NestLevel           ; $8EE0 7C 54 05

COMMON_ISR_RESET    lds       #STACKTOP           ; $8EE3 8E 55 B0
                    ldy       #$0100              ; $8EE6 18 CE 01 00
L8EEA               jsr       KickCOP             ; $8EEA BD A7 DB
                    dey                           ; $8EED 18 09
                    bne       L8EEA               ; $8EEF 26 F9
                    jsr       LA801               ; $8EF1 BD A8 01
                    jsr       NewLine             ; $8EF4 BD 89 86
                    tst       NestLevel           ; $8EF7 7D 54 05
                    beq       L8F0A               ; $8EFA 27 0E
                    lda       #7                  ; $8EFC 86 07
                    jsr       L87E8               ; $8EFE BD 87 E8
                    ldx       #MsgIllEntryError   ; $8F01 CE B4 C4
                    jsr       PrintMessage        ; $8F04 BD 88 F9
                    clr       NestLevel           ; $8F07 7F 54 05
L8F0A               jsr       L8E3D               ; $8F0A BD 8E 3D
L8F0D               clr       $5417               ; $8F0D 7F 54 17
                    clr       $5404               ; $8F10 7F 54 04
                    ldx       #$BBBD              ; $8F13 CE BB BD
                    jsr       L8E79               ; $8F16 BD 8E 79
                    cmpa      #$0D                ; $8F19 81 0D
                    beq       L8EDB               ; $8F1B 27 BE
                    bra       L8F22               ; $8F1D 20 03

L8F1F               jsr       L8E79               ; $8F1F BD 8E 79
L8F22               jsr       L8E6F               ; $8F22 BD 8E 6F
                    inx                           ; $8F25 08
                    lda       ,x                  ; $8F26 A6 00
                    beq       L8EE0               ; $8F28 27 B6
                    anda      #$7F                ; $8F2A 84 7F
                    cmpa      $5401               ; $8F2C B1 54 01
                    beq       L8F3E               ; $8F2F 27 0D
L8F31               clr       $5418               ; $8F31 7F 54 18
                    inc       $5404               ; $8F34 7C 54 04
L8F37               lda       ,x                  ; $8F37 A6 00
                    bmi       L8F1F               ; $8F39 2B E4
                    inx                           ; $8F3B 08
                    bra       L8F37               ; $8F3C 20 F9

L8F3E               lda       ,x                  ; $8F3E A6 00
                    bpl       L8F1F               ; $8F40 2A DD
                    jsr       L8E79               ; $8F42 BD 8E 79
                    cmpa      #$0D                ; $8F45 81 0D
                    beq       L8F62               ; $8F47 27 19
                    cmpa      #$20                ; $8F49 81 20
                    bne       L8F31               ; $8F4B 26 E4

                    lda       $5404               ; $8F4D B6 54 04
                    cmpa      #$0E                ; $8F50 81 0E
                    beq       L8F62               ; $8F52 27 0E
                    cmpa      #$04                ; $8F54 81 04
                    beq       L8F62               ; $8F56 27 0A
                    cmpa      #$0F                ; $8F58 81 0F
                    beq       L8F62               ; $8F5A 27 06
                    bsr       L8FA2               ; $8F5C 8D 44
                    cmpa      #$0D                ; $8F5E 81 0D
                    bne       L8FBE               ; $8F60 26 5C
L8F62               ldb       $5404               ; $8F62 F6 54 04
                    aslb                          ; $8F65 58
                    ldx       #D8F6E              ; $8F66 CE 8F 6E
                    abx                           ; $8F69 3A
                    ldx       ,x                  ; $8F6A EE 00
                    jmp       ,x                  ; $8F6C 6E 00

;-------------------------------------------------------------------------------
; Table of subroutine entries
;-------------------------------------------------------------------------------

D8F6E               dw        L9B0D               ; $8F6E 9B 0D
                    dw        L9667               ; $8F70 96 67
                    dw        L8FC4               ; $8F72 8F C4
                    dw        L9070               ; $8F74 90 70
                    dw        L977D               ; $8F76 97 7D
                    dw        L944F               ; $8F78 94 4F
                    dw        L9617               ; $8F7A 96 17
                    dw        L9042               ; $8F7C 90 42
                    dw        L9400               ; $8F7E 94 00
                    dw        L8664               ; $8F80 86 64
                    dw        L9536               ; $8F82 95 36
                    dw        LA5DB               ; $8F84 A5 DB
                    dw        L9633               ; $8F86 96 33
                    dw        L9431               ; $8F88 94 31
                    dw        L96B7               ; $8F8A 96 B7
                    dw        L9ADF               ; $8F8C 9A DF
                    dw        L8738               ; $8F8E 87 38
                    dw        L862B               ; $8F90 86 2B
                    dw        LA585               ; $8F92 A5 85
                    dw        L87AF               ; $8F94 87 AF
                    dw        L87A8               ; $8F96 87 A8
                    dw        L878C               ; $8F98 87 8C
                    dw        L9684               ; $8F9A 96 84
                    dw        L950A               ; $8F9C 95 0A
                    dw        L9524               ; $8F9E 95 24
                    dw        L952D               ; $8FA0 95 2D

L8FA2               jsr       L8E8D               ; $8FA2 BD 8E 8D
                    ldb       $5417               ; $8FA5 F6 54 17
                    aslb                          ; $8FA8 58
                    cmpb      #$0A                ; $8FA9 C1 0A
                    bhi       L8FBE               ; $8FAB 22 11
                    ldx       #$5406              ; $8FAD CE 54 06
                    abx                           ; $8FB0 3A
                    ldd       $5412               ; $8FB1 FC 54 12
                    std       ,x                  ; $8FB4 ED 00
                    lda       $5401               ; $8FB6 B6 54 01
                    cmpa      #$20                ; $8FB9 81 20
                    beq       L8FA2               ; $8FBB 27 E5
                    rts                           ; $8FBD 39

L8FBE               inc       NestLevel           ; $8FBE 7C 54 05
                    jmp       COMMON_ISR_RESET    ; $8FC1 7E 8E E3

L8FC4               dec       $5417               ; $8FC4 7A 54 17
                    bmi       L900F               ; $8FC7 2B 46
                    clr       RegisterPointer     ; $8FC9 7F 54 00
                    clr       $541A               ; $8FCC 7F 54 1A
                    ldd       $5408               ; $8FCF FC 54 08
                    std       $54B9               ; $8FD2 FD 54 B9
L8FD5               jsr       L8A8E               ; $8FD5 BD 8A 8E
                    beq       L8FE3               ; $8FD8 27 09
                    ldb       RegisterPointer     ; $8FDA F6 54 00
                    cmpb      #$0A                ; $8FDD C1 0A
                    bne       L8FD5               ; $8FDF 26 F4
                    bra       L900F               ; $8FE1 20 2C

L8FE3               ldd       $54B9               ; $8FE3 FC 54 B9
                    std       $5408               ; $8FE6 FD 54 08
                    stx       $5406               ; $8FE9 FF 54 06
                    ldx       #$5408              ; $8FEC CE 54 08
                    ldb       $541A               ; $8FEF F6 54 1A
                    abx                           ; $8FF2 3A
                    incb:2                        ; $8FF3 5C 5C
                    stb       $541A               ; $8FF5 F7 54 1A
                    ldd       ,x                  ; $8FF8 EC 00
                    std       $5408               ; $8FFA FD 54 08
                    jsr       L8AA1               ; $8FFD BD 8A A1
                    beq       L900A               ; $9000 27 08
                    ldd       $5408               ; $9002 FC 54 08
                    ldx       $5406               ; $9005 FE 54 06
                    std       ,x                  ; $9008 ED 00
L900A               dec       $5417               ; $900A 7A 54 17
                    bpl       L8FD5               ; $900D 2A C6
L900F               jsr       NewLine             ; $900F BD 89 86
                    ldx       #MsgBRKPT           ; $9012 CE B4 15
                    jsr       PrintMessage        ; $9015 BD 88 F9
                    lda       #$73                ; $9018 86 73
                    jsr       L87E8               ; $901A BD 87 E8
                    lda       #$3D                ; $901D 86 3D
                    jsr       L87E8               ; $901F BD 87 E8
                    clr       RegisterPointer     ; $9022 7F 54 00
L9025               jsr       L8A8E               ; $9025 BD 8A 8E
                    beq       L9033               ; $9028 27 09
                    jsr       L88CE               ; $902A BD 88 CE
                    ldx       #MsgSpaces4         ; $902D CE B4 E2
                    jsr       PrintMessage        ; $9030 BD 88 F9
L9033               ldb       RegisterPointer     ; $9033 F6 54 00
                    cmpb      #$08                ; $9036 C1 08
                    bls       L9025               ; $9038 23 EB
L903A               jmp       COMMON_ISR_RESET    ; $903A 7E 8E E3

L903D               inc       NestLevel           ; $903D 7C 54 05
                    bra       L903A               ; $9040 20 F8

L9042               dec       $5417               ; $9042 7A 54 17
                    bmi       L906B               ; $9045 2B 24
                    clr       $541A               ; $9047 7F 54 1A
L904A               ldx       #$5408              ; $904A CE 54 08
                    ldb       $541A               ; $904D F6 54 1A
                    abx                           ; $9050 3A
                    incb:2                        ; $9051 5C 5C
                    stb       $541A               ; $9053 F7 54 1A
                    ldd       ,x                  ; $9056 EC 00
                    std       $5408               ; $9058 FD 54 08
                    jsr       L8AA1               ; $905B BD 8A A1
                    bne       L903D               ; $905E 26 DD
                    clr       ,x                  ; $9060 6F 00
                    clr       1,x                 ; $9062 6F 01
                    dec       $5417               ; $9064 7A 54 17
                    bpl       L904A               ; $9067 2A E1
                    bra       L900F               ; $9069 20 A4

L906B               jsr       L8A82               ; $906B BD 8A 82
                    bra       L900F               ; $906E 20 9F

L9070               dec       $5417               ; $9070 7A 54 17
                    bmi       L9088               ; $9073 2B 13
                    bne       L90B0               ; $9075 26 39
                    jsr       L8A46               ; $9077 BD 8A 46
                    ldb       #$08                ; $907A C6 08
                    jsr       L8A21               ; $907C BD 8A 21
                    jsr       L8A58               ; $907F BD 8A 58
                    lda       $540B               ; $9082 B6 54 0B
                    jsr       L8991               ; $9085 BD 89 91
L9088               jsr       L8A2A               ; $9088 BD 8A 2A
                    jsr       L8AA1               ; $908B BD 8A A1
                    beq       L9093               ; $908E 27 03
                    jmp       L8AC4               ; $9090 7E 8A C4

L9093               inc       $5427               ; $9093 7C 54 27
L9096               ldb       $55FF               ; $9096 F6 55 FF
                    orb       #$10                ; $9099 CA 10
                    stb       $55FF               ; $909B F7 55 FF
                    jsr       L8A2A               ; $909E BD 8A 2A
                    jsr       L8B63               ; $90A1 BD 8B 63
                    tst       NestLevel           ; $90A4 7D 54 05
                    bne       L90B0               ; $90A7 26 07
                    ldb       #$0A                ; $90A9 C6 0A
                    lda       #$0C                ; $90AB 86 0C
                    jmp       L8AC7               ; $90AD 7E 8A C7

L90B0               jmp       L941E               ; $90B0 7E 94 1E

                    nop                           ; $90B3 01

;*******************************************************************************
                    #SEG3
;*******************************************************************************

L9400               ldb       $5417               ; $9400
                    decb                          ; $9403 5A
                    bmi       L942C               ; $9404 2B 26
                    bne       L941E               ; $9406 26 16
                    ldd       $5408               ; $9408 FC 54 08
L940B               std       $541D               ; $940B FD 54 1D
                    beq       L941E               ; $940E 27 0E
                    jsr       L8A2A               ; $9410 BD 8A 2A
                    ldx       #$5429              ; $9413 CE 54 29
                    jsr       L8A49               ; $9416 BD 8A 49
                    jsr       L8AA1               ; $9419 BD 8A A1
                    beq       L9429               ; $941C 27 0B
L941E               inc       NestLevel           ; $941E 7C 54 05
                    clrd                          ; $9421 4F 5F
                    std       $541D               ; $9423 FD 54 1D
                    jmp       COMMON_ISR_RESET    ; $9426 7E 8E E3

L9429               jmp       L9093               ; $9429 7E 90 93

L942C               ldd       #1                  ; $942C CC 00 01
                    bra       L940B               ; $942F 20 DA

L9431               ldb       $5417               ; $9431 F6 54 17
                    decb                          ; $9434 5A
                    bmi       L9444               ; $9435 2B 0D
                    bne       L941E               ; $9437 26 E5
                    ldd       $5408               ; $9439 FC 54 08
L943C               std       $541B               ; $943C FD 54 1B
                    beq       L941E               ; $943F 27 DD
                    jmp       L9096               ; $9441 7E 90 96

L9444               ldd       #1                  ; $9444 CC 00 01
                    bra       L943C               ; $9447 20 F3

L9449               inc       NestLevel           ; $9449 7C 54 05
L944C               jmp       COMMON_ISR_RESET    ; $944C 7E 8E E3

L944F               clr       $54B9               ; $944F 7F 54 B9
                    ldb       $5417               ; $9452 F6 54 17
                    decb                          ; $9455 5A
                    bmi       L9449               ; $9456 2B F1
                    decb                          ; $9458 5A
                    beq       L9460               ; $9459 27 05
                    bpl       L9449               ; $945B 2A EC
                    jsr       L8A46               ; $945D BD 8A 46
L9460               lda       $5409               ; $9460 B6 54 09
                    anda      #$F0                ; $9463 84 F0
                    sta       $5409               ; $9465 B7 54 09
L9468               tst       $54B9               ; $9468 7D 54 B9
                    bne       L944C               ; $946B 26 DF
                    clr       $5418               ; $946D 7F 54 18
                    clr       $5419               ; $9470 7F 54 19
                    jsr       NewLine             ; $9473 BD 89 86
                    jsr       L88CE               ; $9476 BD 88 CE
                    ldx       #MsgSpaces4         ; $9479 CE B4 E2
                    jsr       PrintMessage        ; $947C BD 88 F9
L947F               bsr       L94D6               ; $947F 8D 55
                    jsr       L89B4               ; $9481 BD 89 B4
                    tsta                          ; $9484 4D
                    bmi       L948F               ; $9485 2B 08
                    cmpa      #' '                ; $9487 81 20
                    blo       L948F               ; $9489 25 04
                    cmpa      #$7F                ; $948B 81 7F
                    blo       L9491               ; $948D 25 02
L948F               lda       #'.'                ; $948F 86 2E
L9491               ldx       #$5444              ; $9491 CE 54 44
                    ldb       $5418               ; $9494 F6 54 18
                    abx                           ; $9497 3A
                    sta       ,x                  ; $9498 A7 00
                    inc       $5418               ; $949A 7C 54 18
                    jsr       L89B4               ; $949D BD 89 B4
                    jsr       L88AC               ; $94A0 BD 88 AC
                    lda       #$20                ; $94A3 86 20
                    jsr       L87E8               ; $94A5 BD 87 E8
                    jsr       L8A75               ; $94A8 BD 8A 75
                    bne       L94B0               ; $94AB 26 03
                    inc       $54B9               ; $94AD 7C 54 B9
L94B0               jsr       L8A58               ; $94B0 BD 8A 58
                    inc       $5419               ; $94B3 7C 54 19
                    ldb       $5419               ; $94B6 F6 54 19
                    andb      #$10                ; $94B9 C4 10
                    beq       L947F               ; $94BB 27 C2
                    ldx       #MsgSpaces3         ; $94BD CE B4 E3
                    jsr       PrintMessage        ; $94C0 BD 88 F9
                    ldx       #$5444              ; $94C3 CE 54 44
L94C6               bsr       L94D6               ; $94C6 8D 0E
                    lda       ,x                  ; $94C8 A6 00
                    jsr       L87E8               ; $94CA BD 87 E8
                    inx                           ; $94CD 08
                    cmpx      #$5453              ; $94CE 8C 54 53
                    bls       L94C6               ; $94D1 23 F3
                    jmp       L9468               ; $94D3 7E 94 68

L94D6               jsr       LA6F5               ; $94D6 BD A6 F5
                    tst       $5433               ; $94D9 7D 54 33
                    beq       L9506               ; $94DC 27 28
                    clr       $5433               ; $94DE 7F 54 33
                    lda       $7C03               ; $94E1 B6 7C 03
                    anda      #$7F                ; $94E4 84 7F
                    cmpa      #$13                ; $94E6 81 13
                    bne       L94FB               ; $94E8 26 11
                    ldy       #$7C00              ; $94EA 18 CE 7C 00
L94EE               jsr       KickCOP             ; $94EE BD A7 DB
                    brclr     $00,Y,#$01,L94EE    ; $94F1 18 1F 00 01 F8
                    lda       $7C03               ; $94F6 B6 7C 03
                    anda      #$7F                ; $94F9 84 7F
L94FB               cmpa      #$18                ; $94FB 81 18
                    beq       L9501               ; $94FD 27 02
                    bra       L9506               ; $94FF 20 05

L9501               jsr       LA771               ; $9501 BD A7 71
                    bra       L953F               ; $9504 20 39

L9506               jsr       LA771               ; $9506 BD A7 71
                    rts                           ; $9509 39

L950A               lda       $540B               ; $950A B6 54 0B
                    sta       $5419               ; $950D B7 54 19
                    jsr       L8A46               ; $9510 BD 8A 46
L9513               jsr       L89B4               ; $9513 BD 89 B4
                    jsr       L87E8               ; $9516 BD 87 E8
                    jsr       L8A58               ; $9519 BD 8A 58
                    dec       $5419               ; $951C 7A 54 19
                    bne       L9513               ; $951F 26 F2
                    jmp       COMMON_ISR_RESET    ; $9521 7E 8E E3

L9524               jsr       L89B4               ; $9524 BD 89 B4
                    jsr       L87E8               ; $9527 BD 87 E8
                    jmp       COMMON_ISR_RESET    ; $952A 7E 8E E3

L952D               lda       $540B               ; $952D B6 54 0B
                    jsr       L8991               ; $9530 BD 89 91
                    jmp       COMMON_ISR_RESET    ; $9533 7E 8E E3

L9536               ldx       #MsgRegs            ; $9536 CE B4 25
                    jsr       NewLine             ; $9539 BD 89 86
L953C               jsr       PRINT_MSG_HOOK      ; $953C BD 89 03
L953F               jmp       COMMON_ISR_RESET    ; $953F 7E 8E E3

L9542               ldb       $5404               ; $9542 F6 54 04
                    cmpb      #$0C                ; $9545 C1 0C
                    bne       L9550               ; $9547 26 07
                    ldb       $5419               ; $9549 F6 54 19
                    cmpb      #$06                ; $954C C1 06
                    beq       L9553               ; $954E 27 03
L9550               jsr       L88DB               ; $9550 BD 88 DB
L9553               jsr       L8E3D               ; $9553 BD 8E 3D
                    jsr       L8E8D               ; $9556 BD 8E 8D
                    dec       $5418               ; $9559 7A 54 18
                    beq       L959E               ; $955C 27 40
                    ldx       #D9612              ; $955E CE 96 12
L9561               lda       ,x                  ; $9561 A6 00
                    beq       L95B1               ; $9563 27 4C
                    inx                           ; $9565 08
                    cmpa      $5401               ; $9566 B1 54 01
                    bne       L9561               ; $9569 26 F6
                    lda       $5413               ; $956B B6 54 13
                    ldb       $5404               ; $956E F6 54 04
                    cmpb      #$0C                ; $9571 C1 0C
                    bne       L958A               ; $9573 26 15
                    ldb       $5419               ; $9575 F6 54 19
                    cmpb      #$06                ; $9578 C1 06
                    bne       L958A               ; $957A 26 0E
                    ldd       $5412               ; $957C FC 54 12
                    subd      #$0009              ; $957F 83 00 09
                    std       $549E               ; $9582 FD 54 9E
                    jsr       L95B8               ; $9585 BD 95 B8
                    bra       L959E               ; $9588 20 14

L958A               jsr       L8991               ; $958A BD 89 91
                    tst       RegisterPointer     ; $958D 7D 54 00
                    beq       L959E               ; $9590 27 0C
                    jsr       L8A63               ; $9592 BD 8A 63
                    lda       $5412               ; $9595 B6 54 12
                    jsr       L8991               ; $9598 BD 89 91
                    jsr       L8A58               ; $959B BD 8A 58
L959E               lda       $5401               ; $959E B6 54 01
                    cmpa      #$3D                ; $95A1 81 3D
                    beq       $9608               ; $95A3 27 63
                    cmpa      #$5E                ; $95A5 81 5E
                    beq       $95E6               ; $95A7 27 3D
                    cmpa      #$0D                ; $95A9 81 0D
                    beq       L95F5               ; $95AB 27 48
                    cmpa      #$2E                ; $95AD 81 2E
                    beq       L95B4               ; $95AF 27 03
L95B1               inc       NestLevel           ; $95B1 7C 54 05
L95B4               lda       $5401               ; $95B4 B6 54 01
                    rts                           ; $95B7 39

L95B8               clr       $54A0               ; $95B8 7F 54 A0
L95BB               ldx       $5414               ; $95BB FE 54 14
                    inc       $54A0               ; $95BE 7C 54 A0
                    ldb       $54A0               ; $95C1 F6 54 A0
                    abx                           ; $95C4 3A
                    stx       $5408               ; $95C5 FF 54 08
                    jsr       L89B4               ; $95C8 BD 89 B4
                    ldx       $549E               ; $95CB FE 54 9E
                    ldb       $54A0               ; $95CE F6 54 A0
                    abx                           ; $95D1 3A
                    stx       $5408               ; $95D2 FF 54 08
                    jsr       L8991               ; $95D5 BD 89 91
                    ldb       $54A0               ; $95D8 F6 54 A0
                    cmpb      #$09                ; $95DB C1 09
                    bne       L95BB               ; $95DD 26 DC
                    ldx       $549E               ; $95DF FE 54 9E
                    stx       $5414               ; $95E2 FF 54 14
                    rts                           ; $95E5 39

                    jsr       L8A63               ; $95E6 BD 8A 63
                    dec       $5419               ; $95E9 7A 54 19
                    bpl       L95B4               ; $95EC 2A C6
                    ldb       #$06                ; $95EE C6 06
                    stb       $5419               ; $95F0 F7 54 19
                    bra       L95B4               ; $95F3 20 BF

L95F5               jsr       L8A58               ; $95F5 BD 8A 58
                    ldb       $5419               ; $95F8 F6 54 19
                    incb                          ; $95FB 5C
                    stb       $5419               ; $95FC F7 54 19
                    cmpb      #$06                ; $95FF C1 06
                    ble       L95B4               ; $9601 2F B1
                    clr       $5419               ; $9603 7F 54 19
                    bra       L95B4               ; $9606 20 AC

                    tst       RegisterPointer     ; $9608 7D 54 00
                    beq       L95B4               ; $960B 27 A7
                    jsr       L8A63               ; $960D BD 8A 63
                    bra       L95B4               ; $9610 20 A2

D9612               fcs       '^=.',CR            ; $9612 5E 3D 2E 0D $00

L9617               dec       $5417               ; $9617 7A 54 17
                    bne       L967E               ; $961A 26 62
                    clr       RegisterPointer     ; $961C 7F 54 00

L961F               jsr       NewLine             ; $961F BD 89 86
                    jsr       L88CE               ; $9622 BD 88 CE
                    jsr       L9542               ; $9625 BD 95 42
                    tst       NestLevel           ; $9628 7D 54 05
                    bne       L9681               ; $962B 26 54
                    cmpa      #$2E                ; $962D 81 2E
                    bne       L961F               ; $962F 26 EE
                    bra       L9681               ; $9631 20 4E

L9633               ldb       $5417               ; $9633 F6 54 17
                    bne       L967E               ; $9636 26 46
                    ldb       #$06                ; $9638 C6 06
                    stb       $5419               ; $963A F7 54 19
L963D               jsr       NewLine             ; $963D BD 89 86
                    ldx       #D897F              ; $9640 CE 89 7F
                    ldb       $5419               ; $9643 F6 54 19
                    abx                           ; $9646 3A
                    lda       ,x                  ; $9647 A6 00
                    beq       L965F               ; $9649 27 14
                    jsr       L8878               ; $964B BD 88 78
                    jsr       L87E8               ; $964E BD 87 E8
                    jsr       L9542               ; $9651 BD 95 42
L9654               tst       NestLevel           ; $9654 7D 54 05
                    bne       L9681               ; $9657 26 28
                    cmpa      #$2E                ; $9659 81 2E
                    beq       L9681               ; $965B 27 24
                    bra       L963D               ; $965D 20 DE

L965F               jsr       L8968               ; $965F BD 89 68
                    jsr       L9542               ; $9662 BD 95 42
                    bra       L9654               ; $9665 20 ED

L9667               ldb       $5417               ; $9667 F6 54 17
                    cmpb      #$03                ; $966A C1 03
                    bne       L967E               ; $966C 26 10
L966E               lda       $540D               ; $966E B6 54 0D
                    jsr       L8991               ; $9671 BD 89 91
                    jsr       L8A75               ; $9674 BD 8A 75
                    beq       L9681               ; $9677 27 08
                    jsr       L8A58               ; $9679 BD 8A 58
                    bra       L966E               ; $967C 20 F0

L967E               inc       NestLevel           ; $967E 7C 54 05
L9681               jmp       COMMON_ISR_RESET    ; $9681 7E 8E E3

L9684               dec       $5417               ; $9684 7A 54 17
                    bne       L967E               ; $9687 26 F5
                    psha                          ; $9689 36
                    pshb                          ; $968A 37
                    pshx                          ; $968B 3C
                    ldb       $5409               ; $968C F6 54 09
                    lda       $543B               ; $968F B6 54 3B
                    beq       L969D               ; $9692 27 09
                    clr       $543B               ; $9694 7F 54 3B
                    lda       REGBASE             ; $9697 B6 54 3D
                    sta       $543E               ; $969A B7 54 3E
L969D               lda       REGBASE             ; $969D B6 54 3D
                    jsr       SetXBase            ; $96A0 BD A7 D2
                    stb       REGBASE             ; $96A3 F7 54 3D
                    stb       [INIT,x             ; $96A6 E7 3D
                    pulx                          ; $96A8 38
                    pulb                          ; $96A9 33
                    pula                          ; $96AA 32
                    jmp       COMMON_ISR_RESET    ; $96AB 7E 8E E3

L96AE               jsr       L9720               ; $96AE BD 97 20
                    jsr       LA771               ; $96B1 BD A7 71
                    jmp       COMMON_ISR_RESET    ; $96B4 7E 8E E3

L96B7               ldb       #$01                ; $96B7 C6 01
                    lda       $5401               ; $96B9 B6 54 01
                    cmpa      #$0D                ; $96BC 81 0D
                    beq       L96C4               ; $96BE 27 04
                    jsr       L8E79               ; $96C0 BD 8E 79
                    tab                           ; $96C3 16
L96C4               stb       $5401               ; $96C4 F7 54 01
                    jsr       NewLine             ; $96C7 BD 89 86
                    jsr       LA6F5               ; $96CA BD A6 F5
                    jsr       L9715               ; $96CD BD 97 15
L96D0               bsr       L96FC               ; $96D0 8D 2A
                    ldy       #$7C00              ; $96D2 18 CE 7C 00
                    brclr     $00,Y,#$01,L96D0    ; $96D6 18 1F 00 01 F5
                    brset     $01,Y,#$04,L9729    ; $96DB 18 1E 01 04 49
                    clr       $542F               ; $96E0 7F 54 2F
                    lda       $7C03               ; $96E3 B6 7C 03
                    tab                           ; $96E6 16
                    anda      #$7F                ; $96E7 84 7F
                    cmpa      $5401               ; $96E9 B1 54 01
                    beq       L96AE               ; $96EC 27 C0
L96EE               ldy       #$7C04              ; $96EE 18 CE 7C 04
                    brclr     $00,Y,#$40,L96EE    ; $96F2 18 1F 00 40 F7
                    stb       $7C07               ; $96F7 F7 7C 07
                    bra       L96D0               ; $96FA 20 D4

L96FC               ldy       #$7C04              ; $96FC 18 CE 7C 04
                    brclr     $00,Y,#$01,L9714    ; $9700 18 1F 00 01 0F
                    lda       $7C07               ; $9705 B6 7C 07
                    sta       $7C03               ; $9708 B7 7C 03
L970B               ldy       #$7C00              ; $970B 18 CE 7C 00
                    brclr     $00,Y,#$40,L970B    ; $970F 18 1F 00 40 F7
L9714               rts                           ; $9714 39

L9715               lda       SOME_IO             ; $9715 B6 55 C0
                    anda      #$E7                ; $9718 84 E7
                    ora       #$20                ; $971A 8A 20
                    sta       SOME_IO             ; $971C B7 55 C0
                    rts                           ; $971F 39

L9720               lda       SOME_IO             ; $9720 B6 55 C0
                    anda      #$DF                ; $9723 84 DF
                    sta       SOME_IO             ; $9725 B7 55 C0
                    rts                           ; $9728 39

L9729               lda       #0                  ; $9729 86 00
                    sta       $7C04               ; $972B B7 7C 04
                    lda       #$80                ; $972E 86 80
                    sta       $7C04               ; $9730 B7 7C 04
                    bra       L96D0               ; $9733 20 9B

L9735               inc       NestLevel           ; $9735 7C 54 05
                    jmp       COMMON_ISR_RESET    ; $9738 7E 8E E3

L973B               lda       $5401               ; $973B B6 54 01
                    cmpa      #$0D                ; $973E 81 0D
                    beq       L9735               ; $9740 27 F3
                    cmpa      #$20                ; $9742 81 20
                    bne       L9735               ; $9744 26 EF
                    jsr       L8E79               ; $9746 BD 8E 79
                    jsr       L8E6F               ; $9749 BD 8E 6F
                    cmpa      #$54                ; $974C 81 54
                    bne       L975A               ; $974E 26 0A
                    ldb       $5428               ; $9750 F6 54 28
                    orb       #$01                ; $9753 CA 01
                    stb       $5428               ; $9755 F7 54 28
                    bra       L9769               ; $9758 20 0F

L975A               cmpa      #$48                ; $975A 81 48
                    bne       L9735               ; $975C 26 D7
                    jsr       L9715               ; $975E BD 97 15
                    ldb       $5428               ; $9761 F6 54 28
                    orb       #$02                ; $9764 CA 02
                    stb       $5428               ; $9766 F7 54 28
L9769               rts                           ; $9769 39

L976A               inc       $5418               ; $976A 7C 54 18
L976D               jsr       L8E79               ; $976D BD 8E 79
                    cmpa      #$0D                ; $9770 81 0D
                    beq       L9779               ; $9772 27 05
                    jsr       L87E8               ; $9774 BD 87 E8
                    bra       L976D               ; $9777 20 F4

L9779               jsr       NewLine             ; $9779 BD 89 86
                    rts                           ; $977C 39

L977D               jsr       NewLine             ; $977D BD 89 86
                    com       $5443               ; $9780 73 54 43
                    jsr       L973B               ; $9783 BD 97 3B
                    ldb       $5428               ; $9786 F6 54 28
                    andb      #$01                ; $9789 C4 01
                    bne       L9790               ; $978B 26 03
                    jsr       L976A               ; $978D BD 97 6A
L9790               clr       $5416               ; $9790 7F 54 16
L9793               jsr       L87BE               ; $9793 BD 87 BE
                    lda       $549B               ; $9796 B6 54 9B
                    cmpa      #$53                ; $9799 81 53
                    bne       L9793               ; $979B 26 F6
                    jsr       L87BE               ; $979D BD 87 BE
                    lda       $549B               ; $97A0 B6 54 9B
                    cmpa      #$39                ; $97A3 81 39
                    beq       L97AD               ; $97A5 27 06
                    cmpa      #$31                ; $97A7 81 31
                    bne       L9793               ; $97A9 26 E8
                    bra       L97B0               ; $97AB 20 03

L97AD               inc       $5416               ; $97AD 7C 54 16
L97B0               clr       $542B               ; $97B0 7F 54 2B
                    bsr       L97F5               ; $97B3 8D 40
                    suba      #$03                ; $97B5 80 03
                    sta       $5419               ; $97B7 B7 54 19
                    bsr       L97F5               ; $97BA 8D 39
                    sta       $5408               ; $97BC B7 54 08
                    bsr       L97F5               ; $97BF 8D 34
                    sta       $5409               ; $97C1 B7 54 09
L97C4               dec       $5419               ; $97C4 7A 54 19
                    bmi       L97D3               ; $97C7 2B 0A
                    bsr       L97F5               ; $97C9 8D 2A
                    jsr       L8991               ; $97CB BD 89 91
                    jsr       L8A58               ; $97CE BD 8A 58
                    bra       L97C4               ; $97D1 20 F1

L97D3               ldb       $542B               ; $97D3 F6 54 2B
                    stb       $5419               ; $97D6 F7 54 19
                    bsr       L97F5               ; $97D9 8D 1A
                    tst       $5416               ; $97DB 7D 54 16
                    bne       L97E9               ; $97DE 26 09
                    coma                          ; $97E0 43
                    cmpa      $5419               ; $97E1 B1 54 19
                    beq       L9793               ; $97E4 27 AD
L97E6               inc       NestLevel           ; $97E6 7C 54 05
L97E9               jsr       L9720               ; $97E9 BD 97 20
                    clr       $5443               ; $97EC 7F 54 43
                    clr       $5428               ; $97EF 7F 54 28
                    jmp       COMMON_ISR_RESET    ; $97F2 7E 8E E3

L97F5               clr       $5413               ; $97F5 7F 54 13
                    bsr       L9806               ; $97F8 8D 0C
                    bsr       L9806               ; $97FA 8D 0A
                    adda      $542B               ; $97FC BB 54 2B
                    sta       $542B               ; $97FF B7 54 2B
                    lda       $5413               ; $9802 B6 54 13
                    rts                           ; $9805 39

L9806               jsr       L87BE               ; $9806 BD 87 BE
                    lda       $549B               ; $9809 B6 54 9B
                    jsr       L8E9F               ; $980C BD 8E 9F
                    tst       $542D               ; $980F 7D 54 2D
                    bmi       L97E6               ; $9812 2B D2
                    rts                           ; $9814 39

L9815               ldx       #$5429              ; $9815 CE 54 29
                    jsr       L8A49               ; $9818 BD 8A 49
                    rts                           ; $981B 39

L981C               bsr       L9815               ; $981C 8D F7
                    ldd       #$2032              ; $981E CC 20 32
L9821               ldx       #$5444              ; $9821 CE 54 44
                    abx                           ; $9824 3A
                    sta       ,x                  ; $9825 A7 00
                    decb                          ; $9827 5A
                    bne       L9821               ; $9828 26 F7
                    jsr       NewLine             ; $982A BD 89 86
                    clr       $5418               ; $982D 7F 54 18
                    lda       $5408               ; $9830 B6 54 08
                    jsr       L9A60               ; $9833 BD 9A 60
                    lda       $5409               ; $9836 B6 54 09
                    jsr       L9A60               ; $9839 BD 9A 60
                    clr       $54BC               ; $983C 7F 54 BC
                    jsr       L8B63               ; $983F BD 8B 63
                    jsr       L9A53               ; $9842 BD 9A 53
                    lda       $5416               ; $9845 B6 54 16
                    sta       $5402               ; $9848 B7 54 02
                    sta       $5417               ; $984B B7 54 17
                    inc       $5418               ; $984E 7C 54 18
                    jsr       L9A5D               ; $9851 BD 9A 5D
                    inc       $5418               ; $9854 7C 54 18
                    jsr       L8A58               ; $9857 BD 8A 58
                    dec       $5402               ; $985A 7A 54 02
                    bpl       $9851               ; $985D 2A F2
                    jsr       L9A53               ; $985F BD 9A 53
                    tst       NestLevel           ; $9862 7D 54 05
                    beq       $9879               ; $9865 27 12
                    jsr       L8A63               ; $9867 BD 8A 63
                    tst       $542C               ; $986A 7D 54 2C
                    bpl       $9872               ; $986D 2A 03
                    jsr       L8A63               ; $986F BD 8A 63
                    clr       $5416               ; $9872 7F 54 16
                    ldb       #$42                ; $9875 C6 42
                    bra       L98B0               ; $9877 20 37

                    clr       $5407               ; $9879 7F 54 07
                    tst       $542C               ; $987C 7D 54 2C
                    bpl       $988A               ; $987F 2A 09
                    jsr       L89B4               ; $9881 BD 89 B4
                    sta       $5407               ; $9884 B7 54 07
                    jsr       L8A58               ; $9887 BD 8A 58
                    jsr       L89B4               ; $988A BD 89 B4
                    sta       $5406               ; $988D B7 54 06
                    tab                           ; $9890 16
                    andb      #$0F                ; $9891 C4 0F
                    cmpa      #$5F                ; $9893 81 5F
                    bhi       $98BA               ; $9895 22 23
                    tab                           ; $9897 16
                    lsrb:2                        ; $9898 54 54
                    cmpb      #$07                ; $989A C1 07
                    beq       $98B2               ; $989C 27 14
                    tst       $542C               ; $989E 7D 54 2C
                    bpl       L98AE               ; $98A1 2A 0B
                    cmpa      #$35                ; $98A3 81 35
                    bne       L98AE               ; $98A5 26 07
                    ldb       #$8E                ; $98A7 C6 8E
                    inc       NestLevel           ; $98A9 7C 54 05
                    bra       L9917               ; $98AC 20 69

L98AE               ldb       #$FF                ; $98AE C6 FF
L98B0               bra       L9917               ; $98B0 20 65

                    tab                           ; $98B2 16
                    andb      #$03                ; $98B3 C4 03
                    ldx       #$9A9E              ; $98B5 CE 9A 9E
                    bra       L9914               ; $98B8 20 5A

                    cmpa      #$7F                ; $98BA 81 7F
                    bhi       $98C3               ; $98BC 22 05
                    ldx       #$9AA2              ; $98BE CE 9A A2
                    bra       L9914               ; $98C1 20 51

                    cmpa      #$BF                ; $98C3 81 BF
                    bhi       L98FA               ; $98C5 22 33
                    ldx       #$9AB2              ; $98C7 CE 9A B2
                    cmpb      #$03                ; $98CA C1 03
                    bne       L98DC               ; $98CC 26 0E
                    inc       NestLevel           ; $98CE 7C 54 05
                    lda       $5407               ; $98D1 B6 54 07
                    cmpa      #$1A                ; $98D4 81 1A
                    blo       L9914               ; $98D6 25 3C
                    ldb       #$12                ; $98D8 C6 12
                    bra       L9914               ; $98DA 20 38

L98DC               cmpb      #$0C                ; $98DC C1 0C
                    bne       L98ED               ; $98DE 26 0D
                    lda       $5407               ; $98E0 B6 54 07
                    beq       L9914               ; $98E3 27 2F
                    cmpa      #$CD                ; $98E5 81 CD
                    beq       L9914               ; $98E7 27 2B
                    ldb       #$13                ; $98E9 C6 13
                    bra       L9914               ; $98EB 20 27

L98ED               cmpa      #$8F                ; $98ED 81 8F
                    bne       L9914               ; $98EF 26 23
                    incb                          ; $98F1 5C
                    tst       $542C               ; $98F2 7D 54 2C
                    bpl       L9914               ; $98F5 2A 1D
                    incb                          ; $98F7 5C
                    bra       L9914               ; $98F8 20 1A

L98FA               ldx       #$9AC6              ; $98FA CE 9A C6
                    cmpa      #$CF                ; $98FD 81 CF
                    bne       L9905               ; $98FF 26 04
                    ldb       #$12                ; $9901 C6 12
                    bra       L9914               ; $9903 20 0F

L9905               cmpb      #$0E                ; $9905 C1 0E
                    blo       L9914               ; $9907 25 0B
                    lda       $5407               ; $9909 B6 54 07
                    beq       L9914               ; $990C 27 06
                    cmpa      #$CD                ; $990E 81 CD
                    beq       L9914               ; $9910 27 02
                    incb:2                        ; $9912 5C 5C
L9914               abx                           ; $9914 3A
                    ldb       ,x                  ; $9915 E6 00
L9917               stb       $5404               ; $9917 F7 54 04
                    clr       $5402               ; $991A 7F 54 02
                    clrb                          ; $991D 5F
L991E               ldx       #DA400              ; $991E CE A4 00
                    abx                           ; $9921 3A
                    lda       ,x                  ; $9922 A6 00
                    cmpa      #$0F                ; $9924 81 0F
                    bhi       L992B               ; $9926 22 03
L9928               incb                          ; $9928 5C
                    bra       L991E               ; $9929 20 F3

L992B               anda      #$0F                ; $992B 84 0F
                    sta       $5419               ; $992D B7 54 19
                    lda       $5402               ; $9930 B6 54 02
                    cmpa      $5404               ; $9933 B1 54 04
                    beq       L9960               ; $9936 27 28
                    lda       $542C               ; $9938 B6 54 2C
                    cmpa      #$81                ; $993B 81 81
                    beq       L9942               ; $993D 27 03
                    tsta                          ; $993F 4D
                    bmi       L995B               ; $9940 2B 19
L9942               tst       NestLevel           ; $9942 7D 54 05
                    bne       L995B               ; $9945 26 14
                    ldx       #$A4F3              ; $9947 CE A4 F3
                    tba                           ; $994A 17
                    ldb       $5402               ; $994B F6 54 02
                    abx                           ; $994E 3A
                    tab                           ; $994F 16
                    lda       ,x                  ; $9950 A6 00
                    ldx       #DA400              ; $9952 CE A4 00
                    abx                           ; $9955 3A
                    cmpa      $5406               ; $9956 B1 54 06
                    beq       L9960               ; $9959 27 05
L995B               inc       $5402               ; $995B 7C 54 02
                    bra       L9928               ; $995E 20 C8

L9960               lda       $542C               ; $9960 B6 54 2C
                    cmpa      #$81                ; $9963 81 81
                    bne       L9972               ; $9965 26 0B
                    lda       $5404               ; $9967 B6 54 04
                    cmpa      #$8E                ; $996A 81 8E
                    beq       L996F               ; $996C 27 01
                    incb                          ; $996E 5C
L996F               clr       $5416               ; $996F 7F 54 16
L9972               lda       $5402               ; $9972 B6 54 02
                    sta       $5404               ; $9975 B7 54 04
                    lda       ,x                  ; $9978 A6 00
                    anda      #$0F                ; $997A 84 0F
                    cmpa      $5419               ; $997C B1 54 19
                    bhi       L999F               ; $997F 22 1E
                    ldx       #D9E2C              ; $9981 CE 9E 2C
                    abx                           ; $9984 3A
                    lda       ,x                  ; $9985 A6 00
                    anda      #$7F                ; $9987 84 7F
                    stb       RegisterPointer     ; $9989 F7 54 00
                    ldb       $5419               ; $998C F6 54 19
                    addb      #$16                ; $998F CB 16
                    ldx       #$5444              ; $9991 CE 54 44
                    abx                           ; $9994 3A
                    sta       ,x                  ; $9995 A7 00
                    dec       $5419               ; $9997 7A 54 19
                    bmi       L99A9               ; $999A 2B 0D
                    ldb       RegisterPointer     ; $999C F6 54 00
L999F               decb                          ; $999F 5A
                    ldx       #DA400              ; $99A0 CE A4 00
                    abx                           ; $99A3 3A
                    bra       L9972               ; $99A4 20 CC

L99A6               jmp       L9A3B               ; $99A6 7E 9A 3B

L99A9               ldb       #$1C                ; $99A9 C6 1C
                    stb       $5418               ; $99AB F7 54 18
                    lda       $542C               ; $99AE B6 54 2C
                    cmpa      #$02                ; $99B1 81 02
                    blo       L99A6               ; $99B3 25 F1
                    bne       L99C0               ; $99B5 26 09
                    clr       $5416               ; $99B7 7F 54 16
                    jsr       L9A91               ; $99BA BD 9A 91
                    jmp       L9A3B               ; $99BD 7E 9A 3B

L99C0               jsr       L8A58               ; $99C0 BD 8A 58
                    ldb       $542C               ; $99C3 F6 54 2C
                    andb      #$10                ; $99C6 C4 10
                    beq       L99CF               ; $99C8 27 05
                    lda       #$23                ; $99CA 86 23
                    jsr       L9A78               ; $99CC BD 9A 78
L99CF               tst       $5416               ; $99CF 7D 54 16
                    beq       L99E4               ; $99D2 27 10
                    jsr       L9A8C               ; $99D4 BD 9A 8C
L99D7               jsr       L8A08               ; $99D7 BD 8A 08
                    dec       $5416               ; $99DA 7A 54 16
                    bmi       L9A3B               ; $99DD 2B 5C
                    bsr       L9A60               ; $99DF 8D 7F
                    jsr       L8A58               ; $99E1 BD 8A 58
L99E4               ldb       $542C               ; $99E4 F6 54 2C
                    bitb      #$38                ; $99E7 C5 38
                    beq       L99F9               ; $99E9 27 0E
                    tst       $54BC               ; $99EB 7D 54 BC
                    bne       L99F9               ; $99EE 26 09
                    bitb      #$80                ; $99F0 C5 80
                    beq       L99D7               ; $99F2 27 E3
                    dec       $5416               ; $99F4 7A 54 16
                    bra       L99D7               ; $99F7 20 DE

L99F9               bitb      #$44                ; $99F9 C5 44
                    beq       L9A29               ; $99FB 27 2C
                    lda       #$2C                ; $99FD 86 2C
                    bsr       L9A78               ; $99FF 8D 77
                    lda       #$58                ; $9A01 86 58
                    bitb      #$40                ; $9A03 C5 40
                    beq       L9A18               ; $9A05 27 11
                    inca                          ; $9A07 4C
                    psha                          ; $9A08 36
                    lda       $5406               ; $9A09 B6 54 06
                    cmpa      #$1C                ; $9A0C 81 1C
                    beq       L9A14               ; $9A0E 27 04
                    cmpa      #$1D                ; $9A10 81 1D
                    bne       L9A17               ; $9A12 26 03
L9A14               dec       $5416               ; $9A14 7A 54 16
L9A17               pula                          ; $9A17 32
L9A18               bsr       L9A78               ; $9A18 8D 5E
                    andb      #$BB                ; $9A1A C4 BB
                    stb       $542C               ; $9A1C F7 54 2C
                    bpl       L9A29               ; $9A1F 2A 08
                    dec       $54BC               ; $9A21 7A 54 BC
                    beq       L9A29               ; $9A24 27 03
                    dec       $5416               ; $9A26 7A 54 16
L9A29               lda       $5416               ; $9A29 B6 54 16
                    ble       L9A3B               ; $9A2C 2F 0D
                    psha                          ; $9A2E 36
                    bsr       L9A88               ; $9A2F 8D 57
                    pula                          ; $9A31 32
                    andb      #$02                ; $9A32 C4 02
                    beq       L99D7               ; $9A34 27 A1
                    deca                          ; $9A36 4A
                    bne       L99D7               ; $9A37 26 9E
                    bsr       L9A93               ; $9A39 8D 58
L9A3B               clrb                          ; $9A3B 5F
L9A3C               ldx       #$5444              ; $9A3C CE 54 44
                    abx                           ; $9A3F 3A
                    lda       ,x                  ; $9A40 A6 00
                    jsr       L87E8               ; $9A42 BD 87 E8
                    incb                          ; $9A45 5C
                    cmpb      #$2D                ; $9A46 C1 2D
                    bls       L9A3C               ; $9A48 23 F2
                    ldx       #$54AC              ; $9A4A CE 54 AC
                    jsr       L8A85               ; $9A4D BD 8A 85
                    clr       NestLevel           ; $9A50 7F 54 05
L9A53               ldx       #$5429              ; $9A53 CE 54 29
                    jsr       $8A52               ; $9A56 BD 8A 52
                    rts                           ; $9A59 39

                    jsr       L8A58               ; $9A5A BD 8A 58
L9A5D               jsr       L89B4               ; $9A5D BD 89 B4
L9A60               sta       $5401               ; $9A60 B7 54 01
                    bsr       ToHEX               ; $9A63 8D 07
                    lda       $5401               ; $9A65 B6 54 01
                    anda      #$0F                ; $9A68 84 0F
                    bra       L9A70               ; $9A6A 20 04

ToHEX               lsra:4                        ; $9A6C 44 44 44 44
L9A70               adda      #'0'                ; $9A70 8B 30
                    cmpa      #'9'                ; $9A72 81 39
                    bls       L9A78               ; $9A74 23 02
                    adda      #$07                ; $9A76 8B 07

L9A78               ldx       #$5444              ; $9A78 CE 54 44
                    pshb                          ; $9A7B 37
                    ldb       $5418               ; $9A7C F6 54 18
                    abx                           ; $9A7F 3A
                    sta       ,x                  ; $9A80 A7 00
                    incb                          ; $9A82 5C
                    stb       $5418               ; $9A83 F7 54 18
                    pulb                          ; $9A86 33
                    rts                           ; $9A87 39

L9A88               lda       #','                ; $9A88 86 2C
                    bsr       L9A78               ; $9A8A 8D EC
L9A8C               lda       #'$'                ; $9A8C 86 24
                    bsr       L9A78               ; $9A8E 8D E8
                    rts                           ; $9A90 39

L9A91               bsr       L9A8C               ; $9A91 8D F9
L9A93               lda       $54AC               ; $9A93 B6 54 AC
                    bsr       L9A60               ; $9A96 8D C8
                    lda       $54AD               ; $9A98 B6 54 AD
                    bsr       L9A60               ; $9A9B 8D C3
                    rts                           ; $9A9D 39

;-------------------------------------------------------------------------------
; The following is possibly DATA
;-------------------------------------------------------------------------------

                    bne       $9AB3               ; $9A9E 26 13
                    blo       $9AC6               ; $9AA0 25 24
                    incb                          ; $9AA2 5C
                    fcb       $00                 ; $9AA3 00
                    fcb       $00                 ; $9AA4 00
                    pulb                          ; $9AA5 33
                    asrb                          ; $9AA6 57
                    fcb       $00                 ; $9AA7 00
                    tst       $0E,x               ; $9AA8 6D 0E
                    clv                           ; $9AAA 0A
                    dec       $3A,x               ; $9AAB 6A 3A
                    fcb       $00                 ; $9AAD 00
                    fcb       $45                 ; $9AAE 45
                    eora      #$4B                ; $9AAF 88 4B
                    bgt       $9B32               ; $9AB1 2E 7F
; END OF DATA

                    ins                           ; $9AB3 31
                    com       $8108               ; $9AB4 73 81 08
                    daa                           ; $9AB7 19
                    tsta                          ; $9AB8 4D
                    asl       $4003               ; $9AB9 78 40 03
                    neg       $05,x               ; $9ABC 60 05
                    pshb                          ; $9ABE 37
                    inca                          ; $9ABF 4C
                    negb                          ; $9AC0 50
                    inc       $9091               ; $9AC1 7C 90 91
                    psha                          ; $9AC4 36
                    pulx                          ; $9AC5 38
                    suba      #$32                ; $9AC6 80 32
                    lsr       $0709               ; $9AC8 74 07 09
                    fcb       $1A                 ; $9ACB 1A
                    fcb       $4E                 ; $9ACC 4E
                    rol       $4104               ; $9ACD 79 41 04
                    fcb       $61                 ; $9AD0 61
                    tap                           ; $9AD1 06
                    clra                          ; $9AD2 4F
                    dec       $517D               ; $9AD3 7A 51 7D
                    fcb       $52                 ; $9AD6 52

                    jmp       $7B7C               ; $9AD7 7E 7B 7C

                    lsrb                          ; $9ADA 54
                    asld                          ; $9ADB 05

L9ADC               jmp       COMMON_ISR_RESET    ; $9ADC 7E 8E E3

L9ADF               ldx       #MsgBreak           ; $9ADF CE B4 F3
L9AE2               jsr       NewLine             ; $9AE2 BD 89 86
                    jsr       PrintMessage        ; $9AE5 BD 88 F9
                    inx                           ; $9AE8 08
                    cmpx      #SEG5END            ; $9AE9 8C B5 DD
                    bls       L9AE2               ; $9AEC 23 F4
                    ldx       #MsgBR              ; $9AEE CE B8 00
L9AF1               jsr       NewLine             ; $9AF1 BD 89 86
                    jsr       PrintMessage        ; $9AF4 BD 88 F9
                    inx                           ; $9AF7 08
                    cmpx      #$BBBD              ; $9AF8 8C BB BD
                    bls       L9AF1               ; $9AFB 23 F4
                    bra       L9ADC               ; $9AFD 20 DD

L9AFF               lds       $5414               ; $9AFF BE 54 14
                    ldb       $55FF               ; $9B02 F6 55 FF
                    andb      #$7F                ; $9B05 C4 7F
                    orb       #$01                ; $9B07 CA 01
                    stb       $55FF               ; $9B09 F7 55 FF
                    rti                           ; $9B0C 3B
L9B0D               dec       $5417               ; $9B0D 7A 54 17
                    bne       L9B63               ; $9B10 26 51
                    clr       $542F               ; $9B12 7F 54 2F
L9B15               jsr       L981C               ; $9B15 BD 98 1C
                    clr       $5416               ; $9B18 7F 54 16
                    clr       NestLevel           ; $9B1B 7F 54 05
                    clr       $542C               ; $9B1E 7F 54 2C
                    jsr       L8E3D               ; $9B21 BD 8E 3D
                    jsr       L8E79               ; $9B24 BD 8E 79
                    cmpa      #$0D                ; $9B27 81 0D
                    bne       L9B34               ; $9B29 26 09
L9B2B               lda       $5417               ; $9B2B B6 54 17
                    inca                          ; $9B2E 4C
                    jsr       L8A5A               ; $9B2F BD 8A 5A
                    bra       L9B15               ; $9B32 20 E1

L9B34               cmpa      #$2E                ; $9B34 81 2E
                    beq       L9B6D               ; $9B36 27 35
                    dec       $5418               ; $9B38 7A 54 18
                    clr       $5419               ; $9B3B 7F 54 19
                    clr       $5404               ; $9B3E 7F 54 04
                    clr       $5430               ; $9B41 7F 54 30
                    ldb       #$FF                ; $9B44 C6 FF
L9B46               jsr       L8E79               ; $9B46 BD 8E 79
                    jsr       L8E6F               ; $9B49 BD 8E 6F
L9B4C               incb                          ; $9B4C 5C
                    ldx       #DA400              ; $9B4D CE A4 00
                    abx                           ; $9B50 3A
                    lda       ,x                  ; $9B51 A6 00
                    cmpa      #$0F                ; $9B53 81 0F
                    bls       L9B5C               ; $9B55 23 05
                    anda      #$0F                ; $9B57 84 0F
                    inc       $5404               ; $9B59 7C 54 04
L9B5C               cmpa      $5419               ; $9B5C B1 54 19
                    beq       L9B70               ; $9B5F 27 0F
                    bhi       L9B4C               ; $9B61 22 E9
L9B63               lda       #$07                ; $9B63 86 07
                    jsr       L87E8               ; $9B65 BD 87 E8
                    clr       NestLevel           ; $9B68 7F 54 05
                    bra       L9B15               ; $9B6B 20 A8

L9B6D               jmp       COMMON_ISR_RESET    ; $9B6D 7E 8E E3

L9B70               ldx       #D9E2C              ; $9B70 CE 9E 2C
                    abx                           ; $9B73 3A
                    stb       $549A               ; $9B74 F7 54 9A
                    lda       ,x                  ; $9B77 A6 00
                    beq       L9B63               ; $9B79 27 E8
                    anda      #$7F                ; $9B7B 84 7F
                    cmpa      $5401               ; $9B7D B1 54 01
                    bne       L9B4C               ; $9B80 26 CA
                    lda       ,x                  ; $9B82 A6 00
                    bmi       L9B8B               ; $9B84 2B 05
                    inc       $5419               ; $9B86 7C 54 19
                    bra       L9B46               ; $9B89 20 BB

L9B8B               ldx       #DA400              ; $9B8B CE A4 00
                    abx                           ; $9B8E 3A
                    lda       ,x                  ; $9B8F A6 00
                    lsra:4                        ; $9B91 44 44 44 44
                    sta       RegisterPointer     ; $9B95 B7 54 00
                    cmpa      #$04                ; $9B98 81 04
                    bne       L9BC7               ; $9B9A 26 2B
                    jsr       L8E79               ; $9B9C BD 8E 79
                    jsr       L8E6F               ; $9B9F BD 8E 6F
                    cmpa      #$20                ; $9BA2 81 20
                    bne       L9BAB               ; $9BA4 26 05
                    dec       $5418               ; $9BA6 7A 54 18
                    bra       L9BC7               ; $9BA9 20 1C

L9BAB               cmpa      #$41                ; $9BAB 81 41
                    beq       L9BC1               ; $9BAD 27 12
                    cmpa      #$42                ; $9BAF 81 42
                    beq       L9BBD               ; $9BB1 27 0A
                    cmpa      #$44                ; $9BB3 81 44
                    beq       L9BB9               ; $9BB5 27 02
                    bra       L9B63               ; $9BB7 20 AA

L9BB9               incb                          ; $9BB9 5C
                    inc       $5404               ; $9BBA 7C 54 04
L9BBD               incb                          ; $9BBD 5C
                    inc       $5404               ; $9BBE 7C 54 04
L9BC1               incb                          ; $9BC1 5C
                    inc       $5404               ; $9BC2 7C 54 04
                    bra       L9B8B               ; $9BC5 20 C4

L9BC7               ldb       $5404               ; $9BC7 F6 54 04
                    decb                          ; $9BCA 5A
                    ldx       #$A4F3              ; $9BCB CE A4 F3
                    abx                           ; $9BCE 3A
                    lda       ,x                  ; $9BCF A6 00
                    sta       $5404               ; $9BD1 B7 54 04
                    jsr       L8E79               ; $9BD4 BD 8E 79
                    cmpa      #$2E                ; $9BD7 81 2E
                    beq       L9BDF               ; $9BD9 27 04
                    cmpa      #$0D                ; $9BDB 81 0D
                    bne       L9BF0               ; $9BDD 26 11
L9BDF               lda       RegisterPointer     ; $9BDF B6 54 00
                    deca                          ; $9BE2 4A
                    beq       L9BEB               ; $9BE3 27 06
                    cmpa      #$08                ; $9BE5 81 08
                    beq       L9BEB               ; $9BE7 27 02
                    bra       L9BF4               ; $9BE9 20 09

L9BEB               dec       $5418               ; $9BEB 7A 54 18
                    bra       L9BF7               ; $9BEE 20 07

L9BF0               cmpa      #$20                ; $9BF0 81 20
                    beq       L9BF7               ; $9BF2 27 03
L9BF4               jmp       L9B63               ; $9BF4 7E 9B 63

L9BF7               ldb       RegisterPointer     ; $9BF7 F6 54 00
                    aslb                          ; $9BFA 58
                    ldx       #$9C01              ; $9BFB CE 9C 01
                    abx                           ; $9BFE 3A
                    ldx       ,x                  ; $9BFF EE 00
                    jmp       ,x                  ; $9C01 6E 00

; The following is table of subroutine entries

D9C03               fdb       L9DA1               ; $9C03 9D A1
                    fdb       L9CDB               ; $9C05 9C DB
                    fdb       L9C9E               ; $9C07 9C 9E
                    fdb       L9CDB               ; $9C09 9C DB
                    fdb       L9DC2_Hook          ; $9C0B 9D 9C
                    fdb       L9CE5               ; $9C0D 9C E5
                    fdb       L9CE5               ; $9C0F 9C E5
                    fdb       L9C18               ; $9C11 9C 18
                    fdb       L9E03               ; $9C13 9E 03

L9C15               jmp       L9B63               ; $9C15 7E 9B 63

L9C18               ldy       #$54BE              ; $9C18 18 CE 54 BE
                    jsr       L89B4               ; $9C1C BD 89 B4
                    sta       $5495               ; $9C1F B7 54 95
                    jsr       L8E8D               ; $9C22 BD 8E 8D
                    tst       $5412               ; $9C25 7D 54 12
                    bne       L9C92               ; $9C28 26 68
                    jsr       L9D37               ; $9C2A BD 9D 37
                    bne       $9C3C               ; $9C2D 26 0D
                    inc       $5418               ; $9C2F 7C 54 18
                    lda       $5404               ; $9C32 B6 54 04
                    ora       #$0C                ; $9C35 8A 0C
                    sta       $5404               ; $9C37 B7 54 04
                    bra       L9C3F               ; $9C3A 20 03

                    dec       $5418               ; $9C3C 7A 54 18
L9C3F               lda       $5404               ; $9C3F B6 54 04
                    bsr       L9C77               ; $9C42 8D 33
                    bsr       L9C74               ; $9C44 8D 2E
                    jsr       L8E8D               ; $9C46 BD 8E 8D
                    tst       $5412               ; $9C49 7D 54 12
                    bne       L9C92               ; $9C4C 26 44
                    bsr       L9C74               ; $9C4E 8D 24
                    ldb       $549A               ; $9C50 F6 54 9A
                    cmpb      #$41                ; $9C53 C1 41
                    beq       $9C6C               ; $9C55 27 15
                    cmpb      #$1E                ; $9C57 C1 1E
                    beq       $9C6C               ; $9C59 27 11
                    jsr       L8E8D               ; $9C5B BD 8E 8D
                    clra                          ; $9C5E 4F
                    jsr       $9CB2               ; $9C5F BD 9C B2
                    tst       NestLevel           ; $9C62 7D 54 05
                    bne       L9C92               ; $9C65 26 2B
                    jsr       L8A63               ; $9C67 BD 8A 63
                    bsr       L9C74               ; $9C6A 8D 08
                    inc       $5430               ; $9C6C 7C 54 30
                    bsr       L9C80               ; $9C6F 8D 0F
                    jmp       L9DA4               ; $9C71 7E 9D A4

L9C74               lda       $5413               ; $9C74 B6 54 13
L9C77               sta       ,Y                  ; $9C77 18 A7 00
                    iny                           ; $9C7A 18 08
                    jsr       L8A58               ; $9C7C BD 8A 58
                    rts                           ; $9C7F 39

L9C80               jsr       L8A63               ; $9C80 BD 8A 63
                    dey                           ; $9C83 18 09
                    lda       ,Y                  ; $9C85 18 A6 00
                    jsr       L8991               ; $9C88 BD 89 91
                    cmpy      #$54BE              ; $9C8B 18 8C 54 BE
                    bne       L9C80               ; $9C8F 26 EF
                    rts                           ; $9C91 39

L9C92               jsr       L9A53               ; $9C92 BD 9A 53
                    lda       $5495               ; $9C95 B6 54 95
                    jsr       L8991               ; $9C98 BD 89 91
L9C9B               jmp       L9C15               ; $9C9B 7E 9C 15

L9C9E               inc       $5416               ; $9C9E 7C 54 16
                    jsr       L8E8D               ; $9CA1 BD 8E 8D
                    jsr       L9CAF               ; $9CA4 BD 9C AF
                    tst       NestLevel           ; $9CA7 7D 54 05
                    bne       L9C9B               ; $9CAA 26 EF
                    jmp       L9DA4               ; $9CAC 7E 9D A4

L9CAF               lda       $5416               ; $9CAF B6 54 16
                    inca                          ; $9CB2 4C
                    jsr       L8A5A               ; $9CB3 BD 8A 5A
                    ldd       $5412               ; $9CB6 FC 54 12
                    subd      $5408               ; $9CB9 B3 54 08
                    bcc       $9CCB               ; $9CBC 24 0D
                    cmpa      #$FF                ; $9CBE 81 FF
                    beq       $9CC5               ; $9CC0 27 03
                    inc       NestLevel           ; $9CC2 7C 54 05
                    tstb                          ; $9CC5 5D
                    bmi       $9CD7               ; $9CC6 2B 0F
                    inc       NestLevel           ; $9CC8 7C 54 05
                    tsta                          ; $9CCB 4D
                    beq       $9CD1               ; $9CCC 27 03
                    inc       NestLevel           ; $9CCE 7C 54 05
                    tstb                          ; $9CD1 5D
                    bpl       $9CD7               ; $9CD2 2A 03
                    inc       NestLevel           ; $9CD4 7C 54 05
                    stb       $5413               ; $9CD7 F7 54 13
                    rts                           ; $9CDA 39

L9CDB               lda       $5404               ; $9CDB B6 54 04
                    suba      #$10                ; $9CDE 80 10
                    sta       $5404               ; $9CE0 B7 54 04
                    bra       L9CEF               ; $9CE3 20 0A

L9CE5               jsr       L8E79               ; $9CE5 BD 8E 79
                    cmpa      #$23                ; $9CE8 81 23
                    beq       L9D34               ; $9CEA 27 48
                    dec       $5418               ; $9CEC 7A 54 18
L9CEF               inc       $5416               ; $9CEF 7C 54 16
                    jsr       L8E8D               ; $9CF2 BD 8E 8D
                    lda       #$10                ; $9CF5 86 10
                    tst       $54BD               ; $9CF7 7D 54 BD
                    ble       L9D0A               ; $9CFA 2F 0E
                    inc       $5416               ; $9CFC 7C 54 16
                    ldb       RegisterPointer     ; $9CFF F6 54 00
                    cmpb      #$04                ; $9D02 C1 04
                    bne       L9D08               ; $9D04 26 02
                    suba      #$10                ; $9D06 80 10
L9D08               adda      #$20                ; $9D08 8B 20
L9D0A               adda      $5404               ; $9D0A BB 54 04
                    sta       $5404               ; $9D0D B7 54 04
                    lda       $5401               ; $9D10 B6 54 01
                    cmpa      #$2C                ; $9D13 81 2C
                    beq       L9D1A               ; $9D15 27 03
                    jmp       L9DA4               ; $9D17 7E 9D A4

L9D1A               jsr       L9D37               ; $9D1A BD 9D 37
                    bne       L9DC2_Hook          ; $9D1D 26 7D
                    ldb       RegisterPointer     ; $9D1F F6 54 00
                    cmpb      #$04                ; $9D22 C1 04
                    beq       L9DA1               ; $9D24 27 7B
                    lda       $5404               ; $9D26 B6 54 04
                    adda      #$10                ; $9D29 8B 10
                    sta       $5404               ; $9D2B B7 54 04
                    jsr       L8E79               ; $9D2E BD 8E 79
                    jmp       L9DB7               ; $9D31 7E 9D B7

L9D34               jmp       L9E0F               ; $9D34 7E 9E 0F

L9D37               jsr       L8E79               ; $9D37 BD 8E 79
                    jsr       L8E6F               ; $9D3A BD 8E 6F
                    cmpa      #$58                ; $9D3D 81 58
                    bne       L9D53               ; $9D3F 26 12
                    tst       $5412               ; $9D41 7D 54 12
                    beq       L9D49               ; $9D44 27 03
L9D46               jmp       L9C15               ; $9D46 7E 9C 15

L9D49               bsr       L9D66               ; $9D49 8D 1B
                    bne       L9D51               ; $9D4B 26 04
                    lda       #$1A                ; $9D4D 86 1A
                    bra       L9D8A               ; $9D4F 20 39

L9D51               clra                          ; $9D51 4F
                    rts                           ; $9D52 39

L9D53               cmpa      #$59                ; $9D53 81 59
                    bne       AnRTS               ; $9D55 26 20
                    tst       $5412               ; $9D57 7D 54 12
                    bne       L9D46               ; $9D5A 26 EA
                    lda       #$18                ; $9D5C 86 18
                    bsr       L9D78               ; $9D5E 8D 18
                    bne       L9D8A               ; $9D60 26 28
                    lda       #$CD                ; $9D62 86 CD
                    bra       L9D8A               ; $9D64 20 24

;*******************************************************************************

L9D66               proc
                    ldb       $549A               ; $9D66 F6 54 9A
                    cmpb      #$59                ; $9D69 C1 59
                    beq       AnRTS               ; $9D6B 27 0A

;*******************************************************************************

L9D6D               proc
                    cmpb      #$5B                ; $9D6D C1 5B
                    beq       AnRTS               ; $9D6F 27 06
                    cmpb      #$89                ; $9D71 C1 89
                    beq       AnRTS               ; $9D73 27 02
                    cmpb      #$CD                ; $9D75 C1 CD
AnRTS               rts                           ; $9D77 39

;*******************************************************************************

L9D78               proc
                    ldb       $549A               ; $9D78 F6 54 9A
                    cmpb      #$5A                ; $9D7B C1 5A
                    beq       Exit@@              ; $9D7D 27 0A
                    cmpb      #$88                ; $9D7F C1 88
                    beq       Exit@@              ; $9D81 27 06
                    cmpb      #$CC                ; $9D83 C1 CC
                    beq       Exit@@              ; $9D85 27 02
L9D87               cmpb      #$59                ; $9D87 C1 59
Exit@@              rts                           ; $9D89 39

;*******************************************************************************

L9D8A               proc
                    psha                          ; $9D8A 36
                    jsr       L9A53               ; $9D8B BD 9A 53
                    pula                          ; $9D8E 32
                    jsr       L8991               ; $9D8F BD 89 91
                    jsr       L8A58               ; $9D92 BD 8A 58
                    lda       #$80                ; $9D95 86 80
                    sta       $542C               ; $9D97 B7 54 2C
                    clra                          ; $9D9A 4F
                    rts                           ; $9D9B 39

;*******************************************************************************

L9DC2_Hook          bra       L9DC2               ; $9D9C 20 24

L9D9E               dec       $542F               ; $9D9E 7A 54 2F
L9DA1               jsr       L8E79               ; $9DA1 BD 8E 79
L9DA4               ldb       $549A               ; $9DA4 F6 54 9A
                    bsr       L9D6D               ; $9DA7 8D C4
                    beq       L9DB3               ; $9DA9 27 08
                    bsr       L9D87               ; $9DAB 8D DA
                    bne       L9DB7               ; $9DAD 26 08
                    lda       #$1A                ; $9DAF 86 1A
                    bra       L9DB5               ; $9DB1 20 02

L9DB3               lda       #$18                ; $9DB3 86 18
L9DB5               bsr       L9D8A               ; $9DB5 8D D3
L9DB7               lda       $5401               ; $9DB7 B6 54 01
                    cmpa      #$0D                ; $9DBA 81 0D
                    beq       L9DC5               ; $9DBC 27 07
                    cmpa      #$2E                ; $9DBE 81 2E
                    beq       L9D9E               ; $9DC0 27 DC
L9DC2               jmp       L9B63               ; $9DC2 7E 9B 63

L9DC5               tst       $5430               ; $9DC5 7D 54 30
                    bne       L9DF2               ; $9DC8 26 28
                    jsr       L9A53               ; $9DCA BD 9A 53
                    tst       $542C               ; $9DCD 7D 54 2C
                    bpl       L9DD5               ; $9DD0 2A 03
                    jsr       L8A58               ; $9DD2 BD 8A 58
L9DD5               lda       $5404               ; $9DD5 B6 54 04
L9DD8               jsr       L8991               ; $9DD8 BD 89 91
                    tst       NestLevel           ; $9DDB 7D 54 05
                    bne       L9E00               ; $9DDE 26 20
                    jsr       L8A58               ; $9DE0 BD 8A 58
                    ldx       #$5412              ; $9DE3 CE 54 12
                    dec       $5416               ; $9DE6 7A 54 16
                    bmi       L9DF2               ; $9DE9 2B 07
                    bne       L9DEE               ; $9DEB 26 01
                    inx                           ; $9DED 08
L9DEE               lda       ,x                  ; $9DEE A6 00
                    bra       L9DD8               ; $9DF0 20 E6

L9DF2               jsr       L9A53               ; $9DF2 BD 9A 53
                    jsr       L981C               ; $9DF5 BD 98 1C
                    tst       $542F               ; $9DF8 7D 54 2F
                    bne       L9E00               ; $9DFB 26 03
                    jmp       L9B2B               ; $9DFD 7E 9B 2B

L9E00               jmp       COMMON_ISR_RESET    ; $9E00 7E 8E E3

L9E03               lda       #$18                ; $9E03 86 18
                    jsr       L8991               ; $9E05 BD 89 91
                    lda       #$80                ; $9E08 86 80
                    sta       $542C               ; $9E0A B7 54 2C
L9E0D               bra       L9DA1               ; $9E0D 20 92

L9E0F               jsr       L8E8D               ; $9E0F BD 8E 8D
                    ldb       RegisterPointer     ; $9E12 F6 54 00
                    eorb      #$07                ; $9E15 C8 07
                    beq       L9E1E               ; $9E17 27 05
                    tst       $5412               ; $9E19 7D 54 12
                    bne       L9DC2               ; $9E1C 26 A4
L9E1E               dec       $5418               ; $9E1E 7A 54 18
                    inc       $5416               ; $9E21 7C 54 16
                    tstb                          ; $9E24 5D
                    bne       L9E2A               ; $9E25 26 03
                    inc       $5416               ; $9E27 7C 54 16
L9E2A               bra       L9E0D               ; $9E2A 20 E1

;*******************************************************************************
; Possibly DATA follows

D9E2C               fcb       $41                 ; $9E2C 41
                    fcb       $42                 ; $9E2D 42
                    cmpb      #$D8                ; $9E2E C1 D8
                    adcb      $44                 ; $9E30 D9 44
                    coma                          ; $9E32 43
                    cmpb      #$C2                ; $9E33 C1 C2
                    lsra                          ; $9E35 44
                    cmpb      #$C2                ; $9E36 C1 C2
                    andb      #$4E                ; $9E38 C4 4E
                    lsra                          ; $9E3A 44
                    cmpb      #$C2                ; $9E3B C1 C2
                    comb                          ; $9E3D 53
                    ldd       #$C1C2              ; $9E3E CC C1 C2
                    andb      #$D2                ; $9E41 C4 D2
                    cmpb      #$C2                ; $9E43 C1 C2
                    fcb       $42                 ; $9E45 42
                    coma                          ; $9E46 43
                    addd      #$D34C              ; $9E47 C3 D3 4C
                    sbcb      $45                 ; $9E4A D2 45
                    cmpb      $47                 ; $9E4C D1 47
                    bitb      #$D4                ; $9E4E C5 D4
                    asla                          ; $9E50 48
                    adcb      #$D3                ; $9E51 C9 D3
                    rola                          ; $9E53 49
                    lsrb                          ; $9E54 54
                    cmpb      #$C2                ; $9E55 C1 C2
                    inca                          ; $9E57 4C
                    bitb      #$CF                ; $9E58 C5 CF
                    addd      $D4                 ; $9E5A D3 D4
                    tsta                          ; $9E5C 4D
                    adcb      #$4E                ; $9E5D C9 4E
                    bitb      #$50                ; $9E5F C5 50
                    ldd       #$52C1              ; $9E61 CC 52 C1
                    ldx       #$434C              ; $9E64 CE 43 4C
                    sbcb      $53                 ; $9E67 D2 53
                    fcb       $45                 ; $9E69 45
                    andb      $53                 ; $9E6A D4 53
                    fcb       $45                 ; $9E6C 45
                    andb      $D2                 ; $9E6D D4 D2
                    rorb                          ; $9E6F 56
                    addd      #$D343              ; $9E70 C3 D3 43
                    fcb       $42                 ; $9E73 42
                    cmpb      #$4C                ; $9E74 C1 4C
                    addd      #$C9D6              ; $9E76 C3 C9 D6
                    sbcb      $C1                 ; $9E79 D2 C1
                    sbcb      #$4D                ; $9E7B C2 4D
                    negb                          ; $9E7D 50
                    cmpb      #$C2                ; $9E7E C1 C2
                    clra                          ; $9E80 4F
                    fcb       $CD                 ; $9E81 CD
                    cmpb      #$C2                ; $9E82 C1 C2
                    negb                          ; $9E84 50
                    andb      #$D8                ; $9E85 C4 D8
                    adcb      $44                 ; $9E87 D9 44
                    fcb       $41                 ; $9E89 41
                    cmpb      #$45                ; $9E8A C1 45
                    addd      #$C1C2              ; $9E8C C3 C1 C2
                    addd      $D8                 ; $9E8F D3 D8
                    adcb      $45                 ; $9E91 D9 45
                    clra                          ; $9E93 4F
                    fcb       $52                 ; $9E94 52
                    cmpb      #$C2                ; $9E95 C1 C2
                    rora                          ; $9E97 46
                    coma                          ; $9E98 43
                    sbcb      #$44                ; $9E99 C2 44
                    rola                          ; $9E9B 49
                    ldb       $49                 ; $9E9C D6 49
                    lsra                          ; $9E9E 44
                    rola                          ; $9E9F 49
                    ldb       $4E                 ; $9EA0 D6 4E
                    addd      #$C1C2              ; $9EA2 C3 C1 C2
                    addd      $D8                 ; $9EA5 D3 D8
                    adcb      $4A                 ; $9EA7 D9 4A
                    tsta                          ; $9EA9 4D
                    subb      $53                 ; $9EAA D0 53
                    sbcb      $4C                 ; $9EAC D2 4C
                    lsra                          ; $9EAE 44
                    fcb       $41                 ; $9EAF 41
                    cmpb      #$C2                ; $9EB0 C1 C2
                    andb      #$D3                ; $9EB2 C4 D3
                    eorb      $D9                 ; $9EB4 D8 D9
                    comb                          ; $9EB6 53
                    ldd       #$C1C2              ; $9EB7 CC C1 C2
                    andb      #$D2                ; $9EBA C4 D2
                    cmpb      #$C2                ; $9EBC C1 C2
                    andb      #$4D                ; $9EBE C4 4D
                    fcb       $55                 ; $9EC0 55
                    ldd       #$4E45              ; $9EC1 CC 4E 45
                    fcb       $C7                 ; $9EC4 C7
                    cmpb      #$C2                ; $9EC5 C1 C2
                    clra                          ; $9EC7 4F
                    subb      $4F                 ; $9EC8 D0 4F
                    fcb       $52                 ; $9ECA 52
                    fcb       $41                 ; $9ECB 41
                    cmpb      #$C2                ; $9ECC C1 C2
                    negb                          ; $9ECE 50
                    comb                          ; $9ECF 53
                    asla                          ; $9ED0 48
                    cmpb      #$C2                ; $9ED1 C1 C2
                    eorb      $D9                 ; $9ED3 D8 D9
                    fcb       $55                 ; $9ED5 55
                    inca                          ; $9ED6 4C
                    cmpb      #$C2                ; $9ED7 C1 C2
                    eorb      $D9                 ; $9ED9 D8 D9
                    fcb       $52                 ; $9EDB 52
                    clra                          ; $9EDC 4F
                    ldd       #$C1C2              ; $9EDD CC C1 C2
                    sbcb      $C1                 ; $9EE0 D2 C1
                    sbcb      #$54                ; $9EE2 C2 54
                    adcb      #$D3                ; $9EE4 C9 D3
                    comb                          ; $9EE6 53
                    fcb       $42                 ; $9EE7 42
                    cmpb      #$43                ; $9EE8 C1 43
                    cmpb      #$C2                ; $9EEA C1 C2
                    fcb       $45                 ; $9EEC 45
                    addd      #$C9D6              ; $9EED C3 C9 D6
                    lsrb                          ; $9EF0 54
                    fcb       $41                 ; $9EF1 41
                    cmpb      #$C2                ; $9EF2 C1 C2
                    andb      #$4F                ; $9EF4 C4 4F
                    subb      $D3                 ; $9EF6 D0 D3
                    eorb      $D9                 ; $9EF8 D8 D9
                    fcb       $55                 ; $9EFA 55
                    fcb       $42                 ; $9EFB 42
                    cmpb      #$C2                ; $9EFC C1 C2
                    andb      #$57                ; $9EFE C4 57
                    adcb      #$54                ; $9F00 C9 54
                    fcb       $41                 ; $9F02 41
                    sbcb      #$D0                ; $9F03 C2 D0
                    fcb       $42                 ; $9F05 42
                    cmpb      #$45                ; $9F06 C1 45
                    comb                          ; $9F08 53
                    andb      $50                 ; $9F09 D4 50
                    cmpb      #$53                ; $9F0B C1 53
                    andb      $C1                 ; $9F0D D4 C1
                    sbcb      #$D8                ; $9F0F C2 D8
                    adcb      $58                 ; $9F11 D9 58
                    addd      $59                 ; $9F13 D3 59
                    addd      $57                 ; $9F15 D3 57
                    fcb       $41                 ; $9F17 41
                    adcb      #$58                ; $9F18 C9 58
                    asra                          ; $9F1A 47
                    lsra                          ; $9F1B 44
                    eorb      $D9                 ; $9F1C D8 D9
                    fcb       $00                 ; $9F1E 00
                    nop                           ; $9F1F 01
; END OF DATA

;*******************************************************************************
                    #SEG4
;*******************************************************************************
; DATA

DA400               fcb       $00                 ; $A400 00
                    fcb       $01                 ; $A401 01
                    fcb       $12,$12,$92,$01     ; $A402 12 12 92 01
                    fcb       $02                 ; $A406 02
                    fcb       $63,$63             ; $A407 63 63
                    fcb       $02                 ; $A409 02
                    fcb       $63,$63             ; $A40A 63 63
                    fcb       $73,$01,$02         ; $A40C 73 01 02
                    fcb       $63,$63,$01,$42     ; $A40F 63 63 01 42
                    fcb       $13,$13,$13,$42     ; $A413 13 13 13 42
                    fcb       $13,$13,$00,$01     ; $A417 13 13 00 01
                    fcb       $32,$32,$02         ; $A41B 32 32 02
                    fcb       $83,$01,$32,$01     ; $A41E 83 01 32 01
                    fcb       $32,$32,$01,$32     ; $A422 32 32 01 32
                    fcb       $32,$01,$02,$63     ; $A426 32 01 02 63
                    fcb       $63,$01,$32,$32     ; $A42C 63 01 32 32
                    fcb       $32,$32,$01,$32     ; $A42E 32 32 01 32
                    fcb       $01,$32,$01,$32     ; $A432 01 32 01 32
                    fcb       $01,$32,$32,$02     ; $A436 01 32 32 02
                    fcb       $03,$84,$02,$03     ; $A43A 03 84 02 03
                    fcb       $84,$01,$02,$83     ; $A43E 84 01 02 83
                    fcb       $32,$01,$32,$32     ; $A442 32 01 32 32
                    fcb       $00,$01,$12,$01     ; $A446 00 01 12 01
                    fcb       $12,$12,$12,$42     ; $A44A 12 12 12 42
                    fcb       $13,$13,$01,$02     ; $A44E 13 13 01 02
                    fcb       $63,$63,$01,$42     ; $A452 63 63 01 42
                    fcb       $13,$13,$01,$72     ; $A456 13 13 01 72
                    fcb       $72,$72,$00,$01     ; $A45A 72 72 00 01
                    fcb       $12,$01,$42,$13     ; $A45E 12 01 42 13
                    fcb       $13,$12,$12,$92     ; $A462 13 12 12 92
                    fcb       $00,$01,$02,$63     ; $A466 00 01 02 63
                    fcb       $63,$00,$01,$52     ; $A46A 63 00 01 52
                    fcb       $01,$02             ; $A46E 01 02
                    fcb       $13,$00,$01,$02     ; $A470 13 00 01 02
                    fcb       $13,$01,$42,$13     ; $A474 13 01 42 13
                    fcb       $13,$12,$12,$92     ; $A478 13 12 12 92
                    fcb       $00,$01,$42,$01     ; $A47C 00 01 42 01
                    bhi       $A482               ; $A480 22 00
                    nop                           ; $A482 01
                    idiv                          ; $A483 02
                    com       $63,x               ; $A484 63 63
                    fcb       $72                 ; $A486 72
                    fcb       $72                 ; $A487 72
                    fcb       $72                 ; $A488 72
                    fcb       $72                 ; $A489 72
                    nop                           ; $A48A 01
                    fcb       $42                 ; $A48B 42
                    brclr     $13,#$13,$A4D2      ; $A48C 13 13 13 42
                    brclr     $13,#$13,$A494      ; $A490 13 13 13 00
                    nop                           ; $A494 01
                    brset     $00,#$01,$A4DB      ; $A495 12 00 01 42
                    brclr     $13,#$01,$A4AF      ; $A499 13 13 01 12
                    fcb       $00                 ; $A49D 00
                    nop                           ; $A49E 01
                    idiv                          ; $A49F 02
                    com       $63,x               ; $A4A0 63 63
                    fcb       $00                 ; $A4A2 00
                    nop                           ; $A4A3 01
                    idiv                          ; $A4A4 02
                    brclr     $13,#$13,$A43C      ; $A4A5 13 13 13 93
                    nop                           ; $A4A9 01
                    idiv                          ; $A4AA 02
                    brclr     $13,#$13,$A442      ; $A4AB 13 13 13 93
                    fcb       $00                 ; $A4AF 00
                    nop                           ; $A4B0 01
                    fcb       $42                 ; $A4B1 42
                    brclr     $13,#$42,$A4C9      ; $A4B2 13 13 42 13
                    brclr     $01,#$12,$A4CC      ; $A4B6 13 01 12 12
                    fcb       $00                 ; $A4BA 00
                    nop                           ; $A4BB 01
                    brset     $02,#$63,$A523      ; $A4BC 12 02 63 63
                    nop                           ; $A4C0 01
                    brset     $12,#$12,$A4C6      ; $A4C1 12 12 12 01
                    idiv                          ; $A4C5 02
                    bls       $A4EB               ; $A4C6 23 23
                    bhi       $A4CC               ; $A4C8 22 02
                    brclr     $22,#$22,$A4F0      ; $A4CA 13 22 22 22
                    nop                           ; $A4CE 01
                    idiv                          ; $A4CF 02
                    com       $63,x               ; $A4D0 63 63
                    com       $0112               ; $A4D2 73 01 12
                    fcb       $00                 ; $A4D5 00
                    nop                           ; $A4D6 01
                    brset     $12,#$01,$A4ED      ; $A4D7 12 12 01 12
                    nop                           ; $A4DB 01
                    idiv                          ; $A4DC 02
                    brclr     $01,#$12,$A4E2      ; $A4DD 13 01 12 01
                    fcb       $42                 ; $A4E1 42
                    brclr     $13,#$12,$A478      ; $A4E2 13 13 12 92
                    nop                           ; $A4E6 01
                    brset     $01,#$92,$A4EB      ; $A4E7 12 01 92 00
                    nop                           ; $A4EB 01
                    brset     $00,#$01,$A4F2      ; $A4EC 12 00 01 02
                    brclr     $93,#$00,$A50F      ; $A4F0 13 93 00 1B
                    abx                           ; $A4F4 3A
                    abx                           ; $A4F5 3A
                    adca      #$C9                ; $A4F6 89 C9
                    adda      #$CB                ; $A4F8 8B CB
                    addd      #$84C4              ; $A4FA C3 84 C4
                    asl       $48,x               ; $A4FD 68 48
                    aslb                          ; $A4FF 58
                    asld                          ; $A500 05
                    asr       $47,x               ; $A501 67 47
                    asrb                          ; $A503 57
                    bcc       $A52B               ; $A504 24 25
                    bclr      $27,#$2C            ; $A506 15 27 2C
                    bgt       $A52D               ; $A509 2E 22
                    bcc       $A492               ; $A50B 24 85
                    bitb      #$2F                ; $A50D C5 2F
                    bcs       $A534               ; $A50F 25 23
                    blt       $A53E               ; $A511 2D 2B
                    bne       $A53F               ; $A513 26 2A
                    bra       LA538               ; $A515 20 21

          ;This area is probably data or some table

                    brclr     $12,#$14,$A4A8      ; $A517 13 12 14 8D
                    bvc       $A546               ; $A51B 28 29
                    cba                           ; $A51D 11
                    clc                           ; $A51E 0C
                    cli                           ; $A51F 0E
                    clv                           ; $A520 0A
                    clr       $4F,x               ; $A521 6F 4F
                    clrb                          ; $A523 5F
                    cmpa      #$C1                ; $A524 81 C1
                    com       $43,x               ; $A526 63 43
                    comb                          ; $A528 53
                    subd      #$8C8C              ; $A529 83 8C 8C
                    daa                           ; $A52C 19
                    dec       $4A,x               ; $A52D 6A 4A
                    decb                          ; $A52F 5A
                    des                           ; $A530 34
                    dex                           ; $A531 09
                    dex                           ; $A532 09
                    eora      #$C8                ; $A533 88 C8
                    fcb       $41                 ; $A535 41
                    fdiv                          ; $A536 03
                    idiv                          ; $A537 02

LA538               inc       $4C,x               ; $A538 6C 4C
                    incb                          ; $A53A 5C
                    ins                           ; $A53B 31
                    inx:2                         ; $A53C 08 08
                    jmp       $9D,x               ; $A53E 6E 9D

                    lda       #$C6                ; $A540 86 C6
                    ldd       #$8ECE              ; $A542 CC 8E CE
                    ldx       #$6848              ; $A545 CE 68 48
                    aslb                          ; $A548 58
                    asld                          ; $A549 05
                    lsr       $44,x               ; $A54A 64 44
                    lsrb                          ; $A54C 54
                    lsrd                          ; $A54D 04
                    mul                           ; $A54E 3D
                    neg       $40,x               ; $A54F 60 40
                    negb                          ; $A551 50
                    nop                           ; $A552 01
                    ora       #$CA                ; $A553 8A CA
                    psha                          ; $A555 36
                    pshb                          ; $A556 37
                    pshx:2                        ; $A557 3C 3C
                    puld                          ; $A559 32 33
                    pulx:2                        ; $A55B 38 38
                    rol       $49,x               ; $A55D 69 49
                    rolb                          ; $A55F 59
                    ror       $46,x               ; $A560 66 46
                    rorb                          ; $A562 56
                    rti                           ; $A563 3B
                    rts                           ; $A564 39

          ;This area is probably data or some table

                    sba                           ; $A565 10
                    sbca      #$C2                ; $A566 82 C2
                    sec                           ; $A568 0D
                    sei                           ; $A569 0F
                    sev                           ; $A56A 0B
                    sta       $D7                 ; $A56B 97 D7
                    std       $CF                 ; $A56D DD CF
                    sts       $DF                 ; $A56F 9F DF
                    stx       $80                 ; $A571 DF 80
                    subb      #$83                ; $A573 C0 83
                    swi                           ; $A575 3F
                    tab                           ; $A576 16
                    tap                           ; $A577 06
                    tba                           ; $A578 17
                    fcb       $00                 ; $A579 00
                    tpa                           ; $A57A 07
                    tst       $4D,x               ; $A57B 6D 4D
                    tstb                          ; $A57D 5D
                    tsx                           ; $A57E 30
                    tsx                           ; $A57F 30
                    txs                           ; $A580 35
                    txs                           ; $A581 35
                    wai                           ; $A582 3E
                    xgdx                          ; $A583 8F
                    xgdx                          ; $A584 8F

;*******************************************************************************

LA585               proc
                    dec       $5417               ; $A585 7A 54 17
                    bne       Done@@              ; $A588 26 23
                    ldx       #BAUDS              ; $A58A CE A5 B3
Loop@@              ldd       ,x                  ; $A58D EC 00
                    beq       Done@@              ; $A58F 27 1C
                    cpd       $5412               ; $A591 1A B3 54 12
                    beq       OK@@                ; $A595 27 04
                    inx:2                         ; $A597 08 08
                    bra       Loop@@              ; $A599 20 F2

OK@@                xgdx                          ; $A59B 8F
                    subd      #BAUDS              ; $A59C 83 A5 B3
                    lsrb                          ; $A59F 54
                    addb      #$03                ; $A5A0 CB 03
                    stb       $5432               ; $A5A2 F7 54 32
                    orb       #$40                ; $A5A5 CA 40
                    stb       $7C05               ; $A5A7 F7 7C 05
                    jmp       COMMON_ISR_RESET    ; $A5AA 7E 8E E3

Done@@              inc       NestLevel           ; $A5AD 7C 54 05
                    jmp       COMMON_ISR_RESET    ; $A5B0 7E 8E E3

;-------------------------------------------------------------------------------
; The following lines are data for the various baud rates
;-------------------------------------------------------------------------------

BAUDS               dw        $0150               ; $A5B3 01 50
                    dw        $0300               ; $A5B5 03 00
                    dw        $0600               ; $A5B7 06 00
                    dw        $1200               ; $A5B9 12 00
                    dw        $2000               ; $A5BB 20 00
                    dw        $2400               ; $A5BD 24 00
                    dw        $4800               ; $A5BF 48 00
                    dw        $1800               ; $A5C1 18 00
                    dw        $9600               ; $A5C3 96 00
                    dw        $0192               ; $A5C5 01 92
                    fcb       $00                 ; $A5C7 00          Marks end of table

;*******************************************************************************

LA5C8               proc
                    ldd       $540A               ; $A5C8 FC 54 0A
                    subd      $5408               ; $A5CB B3 54 08
                    addd      $540E               ; $A5CE F3 54 0E
                    std       $540A               ; $A5D1 FD 54 0A
                    ldd       $540E               ; $A5D4 FC 54 0E
                    std       $5408               ; $A5D7 FD 54 08
                    rts                           ; $A5DA 39

;*******************************************************************************

LA5DB               proc
                    jsr       L8757               ; $A5DB BD 87 57
                    ldx       $5408               ; $A5DE FE 54 08
                    tst       $549E               ; $A5E1 7D 54 9E
                    beq       Loop@@              ; $A5E4 27 06
                    ldd       $540C               ; $A5E6 FC 54 0C
                    std       $5408               ; $A5E9 FD 54 08
Loop@@              ldd       ,x                  ; $A5EC EC 00
                    pshx                          ; $A5EE 3C
                    jsr       L8991               ; $A5EF BD 89 91
                    jsr       L8A58               ; $A5F2 BD 8A 58
                    pulx                          ; $A5F5 38
                    cmpx      $540A               ; $A5F6 BC 54 0A
                    beq       Done@@              ; $A5F9 27 03
                    inx                           ; $A5FB 08
                    bra       Loop@@              ; $A5FC 20 EE

Done@@              jmp       COMMON_ISR_RESET    ; $A5FE 7E 8E E3

;*******************************************************************************
;                      PROGRAM STARTS HERE
;*******************************************************************************

Start               proc
                    lds       #STACKTOP           ; $A601 8E 55 B0

                    ldx       #REGS               ; $A604 CE 10 00
                    bclr      [CONFIG,x,#$02      ; $A607 1D 3F 02  Disable On-Chip ROM

                    clr       SOME_IO             ; $A60A 7F 55 C0
                    lda       #$02                ; $A60D 86 02
                    sta       SOME_IO             ; $A60F B7 55 C0

                    ldx       #246                ; $A612 CE 00 F6
                    stx       $5414               ; $A615 FF 54 14

                    lda       #208                ; $A618 86 D0
                    sta       1,x                 ; $A61A A7 01

                    clr       $543B               ; $A61C 7F 54 3B
                    clr       REGBASE             ; $A61F 7F 54 3D
                    clr       $543E               ; $A622 7F 54 3E
                    clr       $543F               ; $A625 7F 54 3F
                    clr       $5440               ; $A628 7F 54 40
                    clr       $5441               ; $A62B 7F 54 41
                    clr       $5442               ; $A62E 7F 54 42
                    clr       $5443               ; $A631 7F 54 43

                    lda       #5                  ; $A634 86 05
                    sta       $0F                 ; $A636 97 0F
                    lda       $0F                 ; $A638 96 0F
                    cmpa      #5                  ; $A63A 81 05
                    bne       LA658               ; $A63C 26 1A

                    inc       REGBASE             ; $A63E 7C 54 3D
                    inc       $543E               ; $A641 7C 54 3E
                    inc       $543F               ; $A644 7C 54 3F
                    lda       REGBASE             ; $A647 B6 54 3D
                    jsr       SetXBase            ; $A64A BD A7 D2

                    brclr     [CONFIG,x,#$02,LA66B ; $A64D 1F 3F 02 1A
                    lda       #12                 ; $A651 86 0C
                    sta       $5441               ; $A653 B7 54 41
                    bra       LA674               ; $A656 20 1C

LA658               lda       REGBASE             ; $A658 B6 54 3D
                    jsr       SetXBase            ; $A65B BD A7 D2
                    brclr     [HPRIO,x,#$01,LA674 ; $A65E 1F 3C 01 12
                    inc       $5440               ; $A662 7C 54 40
                    lda       #$04                ; $A665 86 04
                    sta       $3F,x               ; $A667 A7 3F
                    bra       LA674               ; $A669 20 09

LA66B               brclr     $38,x,#$04,LA674    ; $A66B 1F 38 04 05
                    lda       #12                 ; $A66F 86 0C
                    sta       $5442               ; $A671 B7 54 42

LA674               ldb       #$04                ; $A674 C6 04
                    stb       $55FF               ; $A676 F7 55 FF

                    clr       $5428               ; $A679 7F 54 28
                    clr       RegisterPointer     ; $A67C 7F 54 00

                    jsr       L8A82               ; $A67F BD 8A 82
                    ldx       #$7C00              ; $A682 CE 7C 00
                    ldb       #77                 ; $A685 C6 4D
                    bsr       LA6D6               ; $A687 8D 4D
                    jsr       LA6F5               ; $A689 BD A6 F5
                    lda       $7C00               ; $A68C B6 7C 00
                    lda       $7C03               ; $A68F B6 7C 03
                    jsr       KickCOP             ; $A692 BD A7 DB
                    brclr     $00,x,#$01,$A692    ; $A695 1F 00 01 F9
                    ldb       3,x                 ; $A699 E6 03
                    jsr       LA771               ; $A69B BD A7 71
                    lda       #$4D                ; $A69E 86 4D
                    lsrb                          ; $A6A0 54
                    bcs       LA6C2               ; $A6A1 25 1F
                    lsrb                          ; $A6A3 54
                    bcs       LA6C0               ; $A6A4 25 1A
                    lsrb:2                        ; $A6A6 54 54
                    bcs       LA6BE               ; $A6A8 25 14
                    lsrb:4                        ; $A6AA 54 54 54 54
                    bcs       LA6BC               ; $A6AE 25 0C

                    ldy       #0                  ; $A6B0 18 CE 00 00
LA6B4               dey                           ; $A6B4 18 09
                    iny                           ; $A6B6 18 08
                    dey                           ; $A6B8 18 09
                    bra       LA6B4               ; $A6BA 20 F8

LA6BC               suba      #2                  ; $A6BC 80 02
LA6BE               suba      #2                  ; $A6BE 80 02
LA6C0               suba      #1                  ; $A6C0 80 01
LA6C2               anda      #$0F                ; $A6C2 84 0F
                    sta       $5431               ; $A6C4 B7 54 31
                    sta       $5432               ; $A6C7 B7 54 32
                    jsr       LA801               ; $A6CA BD A8 01
                    ldx       #MsgVersion         ; $A6CD CE B4 00
                    jsr       NewLine             ; $A6D0 BD 89 86
                    jmp       LA846               ; $A6D3 7E A8 46

;*******************************************************************************

LA6D6               proc
                    jsr       LA6F5               ; $A6D6 BD A6 F5

                    lda       #$80                ; $A6D9 86 80
                    sta       $7C01               ; $A6DB B7 7C 01

                    lda       #$E0                ; $A6DE 86 E0
                    nop:7                         ; $A6E0 01 01 01 01 01 01 01
                    sta       $7C01               ; $A6E7 B7 7C 01

                    clra                          ; $A6EA 4F
                    sta       $7C01               ; $A6EB B7 7C 01
                    stb       $7C01               ; $A6EE F7 7C 01

                    jsr       LA771               ; $A6F1 BD A7 71
                    rts                           ; $A6F4 39

;*******************************************************************************

LA6F5               proc
                    pshx                          ; $A6F5 3C
                    psha                          ; $A6F6 36
                    pshb                          ; $A6F7 37

                    lda       REGBASE             ; $A6F8 B6 54 3D
                    jsr       SetXBase            ; $A6FB BD A7 D2
                    lda       [INIT,x             ; $A6FE A6 3D
                    cmpa      REGBASE             ; $A700 B1 54 3D
                    beq       Go@@                ; $A703 27 06
                    lda       $543E               ; $A705 B6 54 3E
                    jsr       SetXBase            ; $A708 BD A7 D2
Go@@                lda       $543F               ; $A70B B6 54 3F
                    cmpa      #$01                ; $A70E 81 01
                    beq       Skip@@              ; $A710 27 3A

                    lda       $5440               ; $A712 B6 54 40
                    bne       PulExitBAX          ; $A715 26 56
                    bset      $38,x,#$20          ; $A717 1C 38 20
                    brset     $38,x,#$20,PulExitBAX  ; $A71A 1E 38 20 4F
                    lda       $5A,x               ; $A71E A6 5A
                    sta       $5434               ; $A720 B7 54 34
                    ldd       $5C,x               ; $A723 EC 5C
                    std       $5435               ; $A725 FD 54 35
                    lda       $5B,x               ; $A728 A6 5B
                    sta       $5437               ; $A72A B7 54 37
                    lda       $5B,x               ; $A72D A6 5B
                    ora       #$08                ; $A72F 8A 08
                    sta       $5B,x               ; $A731 A7 5B
                    lda       #$30                ; $A733 86 30
                    sta       $5A,x               ; $A735 A7 5A
                    ldd       #$0F01              ; $A737 CC 0F 01
                    std       $5C,x               ; $A73A ED 5C
                    ldd       $5E,x               ; $A73C EC 5E
                    std       $5438               ; $A73E FD 54 38
                    ldd       $56,x               ; $A741 EC 56
                    std       $543A               ; $A743 FD 54 3A
                    clrd                          ; $A746 4F 5F
                    std       $56,x               ; $A748 ED 56
                    bra       PulExitBAX          ; $A74A 20 21

Skip@@              lda       $5441               ; $A74C B6 54 41
                    beq       PulExitBAX          ; $A74F 27 1C
                    lda       $5C,x               ; $A751 A6 5C
                    sta       $5434               ; $A753 B7 54 34
                    ora       #$0C                ; $A756 8A 0C
                    sta       $5C,x               ; $A758 A7 5C
                    lda       $5D,x               ; $A75A A6 5D
                    sta       $5437               ; $A75C B7 54 37
                    ora       #$08                ; $A75F 8A 08
                    sta       $5D,x               ; $A761 A7 5D
                    ldd       $5E,x               ; $A763 EC 5E
                    std       $5435               ; $A765 FD 54 35
                    ldd       #$7C06              ; $A768 CC 7C 06
                    std       $5E,x               ; $A76B ED 5E

PulExitBAX          pulb                          ; $A76D 33
                    pula                          ; $A76E 32
                    pulx                          ; $A76F 38
                    rts                           ; $A770 39

;*******************************************************************************

LA771               proc
                    pshx                          ; $A771 3C
                    psha                          ; $A772 36
                    pshb                          ; $A773 37
                    lda       REGBASE             ; $A774 B6 54 3D
                    jsr       SetXBase            ; $A777 BD A7 D2
                    lda       [INIT,x             ; $A77A A6 3D
                    cmpa      REGBASE             ; $A77C B1 54 3D
                    beq       LA787               ; $A77F 27 06
                    lda       $543E               ; $A781 B6 54 3E
                    jsr       SetXBase            ; $A784 BD A7 D2
LA787               lda       $543F               ; $A787 B6 54 3F
                    cmpa      #$01                ; $A78A 81 01
                    beq       LA7B7               ; $A78C 27 29
                    lda       $5440               ; $A78E B6 54 40
                    bne       PulExitBAX          ; $A791 26 DA
                    brclr     $38,x,#$20,$A79C    ; $A793 1F 38 20 05
                    bclr      $38,x,#$20          ; $A797 1D 38 20
                    bra       PulExitBAX          ; $A79A 20 D1

                    lda       $5434               ; $A79C B6 54 34
                    sta       $5A,x               ; $A79F A7 5A
                    lda       $5437               ; $A7A1 B6 54 37
                    std       $5B,x               ; $A7A4 ED 5B
                    ldd       $5435               ; $A7A6 FC 54 35
                    std       $5C,x               ; $A7A9 ED 5C
                    ldd       $5438               ; $A7AB FC 54 38
                    std       $5E,x               ; $A7AE ED 5E
                    ldd       $543A               ; $A7B0 FC 54 3A
                    std       $56,x               ; $A7B3 ED 56
                    bra       PulExitBAX          ; $A7B5 20 B6

LA7B7               lda       $5441               ; $A7B7 B6 54 41
                    beq       PulExitBAX          ; $A7BA 27 B1
                    lda       $5434               ; $A7BC B6 54 34
                    sta       $5C,x               ; $A7BF A7 5C
                    lda       $5437               ; $A7C1 B6 54 37
                    sta       $5D,x               ; $A7C4 A7 5D
                    ldd       $5435               ; $A7C6 FC 54 35
                    std       $5E,x               ; $A7C9 ED 5E
                    bra       PulExitBAX          ; $A7CB 20 A0

                    ldd       $543A               ; $A7CD FC 54 3A
                    std       $56,x               ; $A7D0 ED 56

;*******************************************************************************

SetXBase            proc
                    pshb                          ; $A7D2 37
                    clrb                          ; $A7D3 5F
                    asld:4                        ; $A7D4 05 05 05 05
                    xgdx                          ; $A7D8 8F
                    pulb                          ; $A7D9 33
                    rts                           ; $A7DA 39

;*******************************************************************************

KickCOP             proc
                    psha                          ; $A7DB 36
                    pshb                          ; $A7DC 37
                    pshx                          ; $A7DD 3C

                    lda       $543B               ; $A7DE B6 54 3B
                    bne       Skip@@              ; $A7E1 26 0D
                    lda       $543E               ; $A7E3 B6 54 3E
                    jsr       SetXBase            ; $A7E6 BD A7 D2
                    ldd       #$55AA              ; $A7E9 CC 55 AA
                    sta       [COPRST,x           ; $A7EC A7 3A
                    stb       [COPRST,x           ; $A7EE E7 3A

Skip@@              lda       REGBASE             ; $A7F0 B6 54 3D
                    jsr       SetXBase            ; $A7F3 BD A7 D2
                    ldd       #$55AA              ; $A7F6 CC 55 AA
                    sta       [COPRST,x           ; $A7F9 A7 3A
                    stb       [COPRST,x           ; $A7FB E7 3A

                    pulx                          ; $A7FD 38
                    pulb                          ; $A7FE 33
                    pula                          ; $A7FF 32
                    rts                           ; $A800 39

;*******************************************************************************

LA801               proc
                    jsr       LA6F5               ; $A801 BD A6 F5
                    ldy       #$7C00              ; $A804 18 CE 7C 00
                    bset      $01,Y,#$80          ; $A808 18 1C 01 80
                    ldy       #$7C04              ; $A80C 18 CE 7C 04
                    bset      $01,Y,#$80          ; $A810 18 1C 01 80
                    lda       #$E0                ; $A814 86 E0
                    sta       $7C01               ; $A816 B7 7C 01
                    sta       $7C05               ; $A819 B7 7C 05
                    ldy       #$7C00              ; $A81C 18 CE 7C 00
                    bclr      $01,Y,#$80          ; $A820 18 1D 01 80
                    ldy       #$7C04              ; $A824 18 CE 7C 04
                    bclr      $01,Y,#$80          ; $A828 18 1D 01 80
                    lda       #$40                ; $A82C 86 40
                    tab                           ; $A82E 16
                    ora       $5431               ; $A82F BA 54 31
                    sta       $7C01               ; $A832 B7 7C 01
                    tba                           ; $A835 17
                    ora       $5432               ; $A836 BA 54 32
                    sta       $7C05               ; $A839 B7 7C 05
                    lda       $7C00               ; $A83C B6 7C 00
                    lda       $7C03               ; $A83F B6 7C 03
                    jsr       LA771               ; $A842 BD A7 71
                    rts                           ; $A845 39

;*******************************************************************************

LA846               proc
                    ldb       $5428               ; $A846 F6 54 28
                    andb      #$FD                ; $A849 C4 FD
                    stb       $5428               ; $A84B F7 54 28
                    clr       NestLevel           ; $A84E 7F 54 05
                    clr       $5427               ; $A851 7F 54 27
                    ldb       #$04                ; $A854 C6 04
                    stb       $55FF               ; $A856 F7 55 FF
                    clrd                          ; $A859 4F 5F
                    std       $541D               ; $A85B FD 54 1D
                    std       $54B7               ; $A85E FD 54 B7
                    std       $541B               ; $A861 FD 54 1B
                    jmp       L953C               ; $A864 7E 95 3C

LA867               jmp       LA91B               ; $A867 7E A9 1B

;*******************************************************************************

SWI_Handler         proc
                    ldb       $55FF               ; $A86A F6 55 FF
                    andb      #$FE                ; $A86D C4 FE
                    stb       $55FF               ; $A86F F7 55 FF
                    sts       $5414               ; $A872 BF 54 14
                    sts       $549E               ; $A875 BF 54 9E
                    lds       #STACKTOP           ; $A878 8E 55 B0
                    jsr       L8A2A               ; $A87B BD 8A 2A
                    jsr       L8A63               ; $A87E BD 8A 63
                    jsr       L8A46               ; $A881 BD 8A 46
                    lda       #$0C                ; $A884 86 0C
                    jsr       L8AA3               ; $A886 BD 8A A3
                    beq       LA8AA               ; $A889 27 1F
                    jsr       L89B4               ; $A88B BD 89 B4
                    cmpa      #$3F                ; $A88E 81 3F
                    beq       LA867               ; $A890 27 D5
                    lda       REGBASE             ; $A892 B6 54 3D
                    jsr       SetXBase            ; $A895 BD A7 D2
                    lda       [INIT,x             ; $A898 A6 3D
                    cmpa      REGBASE             ; $A89A B1 54 3D
                    bne       LA8A2               ; $A89D 26 03
                    inc       $543B               ; $A89F 7C 54 3B
LA8A2               ldb       $55FF               ; $A8A2 F6 55 FF
                    orb       #$20                ; $A8A5 CA 20
                    stb       $55FF               ; $A8A7 F7 55 FF
LA8AA               ldb       $55FF               ; $A8AA F6 55 FF
                    andb      #$10                ; $A8AD C4 10
                    beq       LA8C6               ; $A8AF 27 15
                    ldb       #$0C                ; $A8B1 C6 0C
                    lda       #$0A                ; $A8B3 86 0A
                    jsr       L8AF9               ; $A8B5 BD 8A F9
                    ldx       #$54AC              ; $A8B8 CE 54 AC
                    jsr       L8A85               ; $A8BB BD 8A 85
                    ldb       $55FF               ; $A8BE F6 55 FF
                    andb      #$F7                ; $A8C1 C4 F7
                    stb       $55FF               ; $A8C3 F7 55 FF
LA8C6               ldb       $55FF               ; $A8C6 F6 55 FF
                    andb      #$08                ; $A8C9 C4 08
                    beq       LA8D0               ; $A8CB 27 03
                    jsr       L8AF6               ; $A8CD BD 8A F6
LA8D0               jsr       L8A32               ; $A8D0 BD 8A 32
                    jsr       L8A4F               ; $A8D3 BD 8A 4F
                    ldb       $55FF               ; $A8D6 F6 55 FF
                    andb      #$20                ; $A8D9 C4 20
                    bne       LA910               ; $A8DB 26 33
                    ldb       #$04                ; $A8DD C6 04
                    stb       $55FF               ; $A8DF F7 55 FF
                    jsr       L8AA1               ; $A8E2 BD 8A A1
                    bne       LA939               ; $A8E5 26 52
                    ldd       $541D               ; $A8E7 FC 54 1D
                    decd                          ; $A8EA 83 00 01
                    bcs       LA905               ; $A8ED 25 16
                    ldd       $5429               ; $A8EF FC 54 29
                    subd      $5408               ; $A8F2 B3 54 08
                    bne       LA900               ; $A8F5 26 09
                    ldd       $541D               ; $A8F7 FC 54 1D
                    decd                          ; $A8FA 83 00 01
                    std       $541D               ; $A8FD FD 54 1D
LA900               ldd       $541D               ; $A900 FC 54 1D
                    bne       LA936               ; $A903 26 31
LA905               ldx       #MsgBRKPT           ; $A905 CE B4 15
                    jsr       NewLine             ; $A908 BD 89 86
                    bra       LA919               ; $A90B 20 0C

LA90D               jmp       LA846               ; $A90D 7E A8 46

LA910               jsr       LA801               ; $A910 BD A8 01
                    jsr       NewLine             ; $A913 BD 89 86
                    ldx       #MsgAbort           ; $A916 CE B4 1D
LA919               bra       LA90D               ; $A919 20 F2

LA91B               ldd       $5414               ; $A91B FC 54 14
                    subd      #9                  ; $A91E 83 00 09
                    std       $549E               ; $A921 FD 54 9E
                    jsr       L95B8               ; $A924 BD 95 B8
                    jsr       L8A0F               ; $A927 BD 8A 0F
                    jsr       L8A32               ; $A92A BD 8A 32
                    jmp       L9AFF               ; $A92D 7E 9A FF

LA930               clr       $5427               ; $A930 7F 54 27
                    jmp       L8AC4               ; $A933 7E 8A C4

LA936               jmp       L9093               ; $A936 7E 90 93

LA939               tst       $5427               ; $A939 7D 54 27
                    bne       LA930               ; $A93C 26 F2
                    jsr       L981C               ; $A93E BD 98 1C
                    ldx       #SEG5END            ; $A941 CE B5 DD
                    ldd       $541B               ; $A944 FC 54 1B
                    decd                          ; $A947 83 00 01
                    std       $541B               ; $A94A FD 54 1B
                    bne       LA951               ; $A94D 26 02
                    bra       LA90D               ; $A94F 20 BC

LA951               jsr       L8906               ; $A951 BD 89 06
                    jmp       L9096               ; $A954 7E 90 96

;*******************************************************************************

COMMON_ISR          proc
                    ldx       #MsgIntInMonError   ; $A957 CE B4 A2
                    jsr       PrintMessage        ; $A95A BD 88 F9
                    jmp       COMMON_ISR_RESET    ; $A95D 7E 8E E3

;*******************************************************************************
                    #SEG5                         ;SECTION 5 (MESSAGE BLOCK)
;*******************************************************************************

MsgVersion          fcs       CR,LF,'  EVSbug11 Rev 2.5'
MsgBRKPT            fcs       '  Brkpt'
MsgAbort            fcs       '  Abort'
MsgRegs             fcs       '  Regs'
errNOTBLANK         fcs       'MC68HC11 NOT BLANK'
errNOVERIFY         fcs       'MC68HC11 DOES NOT VERIFY'
MsgVerifyOK         fcs       'VERIFY COMPLETE'
MsgCR               fcs       CR
MsgContinue         fcs       CR,LF,'ENTER RETURN TO CONTINUE'

;WARNING: The following line should be missing (possibly related to the FCC bug below)

                    fcb       0

MsgReadComplete     fcs       'READ COMPLETE'

MsgReadError        fcs       'ERROR IN READ'
MsgIntInMonError    fcs       'INTERRUPT OCCURRED IN MONITOR MAP'
MsgIllEntryError    fcs       ' ILLEGAL/INSUFFICIENT ENTRY',CR,LF

MsgSpaces4          fcc       ' '
MsgSpaces3          fcc       ' '
MsgSpaces2          fcc       ' '
MsgSpaces1          fcs       ' '

MsgBadMemoryError   fcs       ' BAD MEMORY'

MsgBreak            fcs       'BREAK = Abort command, CTRL-A = Exit transparent mode, CTRL-H = Backspace, '

msgCTRLS            fcs       'CTRL-S = Freeze screen, CTRL-X = Cancel command line'
msgASM              fcs       'ASM <START ADDR>- Single line assembler/disassembler'
msgBF               fcs       'BF <START ADDR> <END ADDR> <DATA>- Block fill memory'

SEG5END             equ       *-1

;WARNING: There is a gap in the help screen filled with erased state bytes ($FF)
;         This is partly responsible for the weird help display.
;         It should be fixed after source is "fully" reversed.

;*******************************************************************************
                    #SEG6                         ;SECTION 6 (MESSAGE BLOCK)
;*******************************************************************************

                    @hint     MsgBR
MsgBR               fcs       'BR [<ADDR1 - ADDR5>]- Set 1 to 5 breakpoints'
                    @hint     MsgBULK
MsgBULK             fcs       'BULK <ADDR> - Bulk erase slave MCU EEPROM or CONFIG'
                    @hint     MsgCHCK
MsgCHCK             fcs       'CHCK <START ADDR> [<END ADDR>]- Blank check slave MCU EEPROM. Blank byte = FF'

          ;WARNING: The following line has a bug in the original source code, it
          ;         should have been FCS, not FCC
          ;         This is partly responsible for the weird help display.

MsgCopy             fcc       'COPY <START ADDR> [<END ADDR> <USR MAP START>]-COPY slave MCU EEPROM to User Map'
                    @hint     MsgERASE
MsgERASE            fcs       'ERASE <START ADDR> [<END ADDR>]- Byte erase slave MCU EEPROM'
                    @hint     MsgG
MsgG                fcs       'G [<START ADDR>]- Go to user map and execute program'
                    @hint     MsgLOAD
MsgLOAD             fcs       'LOAD <PORT> [=<TEXT>]- Download (H)ost or (T)erminal port to user map'
                    @hint     MsgINIT
MsgINIT             fcs       'INIT <DATA> - Enter users init register value'
                    @hint     MsgMD
MsgMD               fcs       'MD <START ADDR> [<END ADDR>]- Memory display'
                    @hint     MsgMM
MsgMM               fcs       'MM <ADDRESS>- Memory modify'
                    @hint     MsgNOBR
MsgNOBR             fcs       'NOBR [<ADDR1 - ADDR5>]- Remove breakpoints'
                    @hint     MsgP
MsgP                fcs       'P [<COUNT>]- Proceed 1-FFFF times through a breakpoint'
                    @hint     MsgPROG
MsgPROG             fcs       'PROG <START ADDR> [<END ADDR> <DATA>]-Program slave MCU EEPROM'
                    @hint     MsgRD
MsgRD               fcs       'RD- Register display'
                    @hint     MsgRM
MsgRM               fcs       'RM- Register modify'
                    @hint     MsgSPEED
MsgSPEED            fcs       'SPEED <BAUD RATE>- Select host baud rate'
                    @hint     MsgT
MsgT                fcs       'T [<COUNT>]- Trace 1-FFFF instructions'
                    @hint     MsgTM
MsgTM               fcs       'TM [<EXIT CHARACTER>]- Transparent mode'
                    @hint     MsgVERF
MsgVerf             fcs       'VERF <START ADDR> [<END ADDR> <USR MAP>]- Verify slave MCU EEPROM to user map'

;*******************************************************************************
; Form a 7-bit ASCII string terminated with last character's msb set.
; This format uses one less byte than an ASCIZ string, but cannot handle
; characters with ASCII codes of 128 or above.

Str7                macro     String
                    mreq      1:String
                    mset      1,~@~
                    mstr      1
          #if :1 < 3
                    merror    Null string not allowed
          #endif
                    mset      2
          #if :1-2 > 1
                    mset      2,\@~1.2.{:1-3}~\@,
          #endif
                    mset      3,\@~1.{:1-1}.1~\@|$80
                    fcc       ~2~~3~
                    endm

; Table of command names with last byte's BIT7 set

                    @Str7     ASM                 ; $BBBE 41 53 CD
                    @Str7     BF                  ; $BBC1 42 C6
                    @Str7     BR                  ; $BBC3 42 D2
                    @Str7     G                   ; $BBC4 C7
                    @Str7     LOAD                ; $BBC5 4C 4F 41 C4
                    @Str7     MD                  ; $BBCA 4D C4
                    @Str7     MM                  ; $BBCC 4D CD
                    @Str7     NOBR                ; $BBCE 4E 4F 42 D2
                    @Str7     P                   ; $BBD2 D0
                    @Str7     PROG                ; $BBD3 50 52 4F C7
                    @Str7     RD                  ; $BBD7 52 C4
                    @Str7     READ                ; $BBD9 52 45 41 C4
                    @Str7     RM                  ; $BBDD 52 CD
                    @Str7     T                   ; $BBDF D4
                    @Str7     TM                  ; $BBE2 54 CD
                    @Str7     HELP                ; $BBE2 48 45 4C D0
                    @Str7     VERF                ; $BBE6 56 45 52 C6
                    @Str7     CHCK                ; $BBEA 43 48 43 CB
                    @Str7     SPEED               ; $BBEE 53 50 45 45 C4
                    @Str7     ERASE               ; $BBF3 45 52 41 53 C5
                    @Str7     COPY                ; $BBF8 43 4F 50 D9
                    @Str7     BULK                ; $BBFC 42 55 4C CB
                    @Str7     INIT                ; $BC00 49 4E 49 D4

                    long      $99859200           ; $BC04 99 85 92 00 (?CTRL-Y -E -R)

;*******************************************************************************
                    #SEG7                         ;SECTION 7
;*******************************************************************************

Initialize          proc
                    ldx       #REGS               ; $BC10 CE 10 00
                    clr       [BPROT,x            ; $BC13 6F 35       Enable EEPROM writes
                    bset      [HPRIO,x,#$20       ; $BC15 1C 3C 20

                    ldx       #Head               ; $BC18 CE 00 E0
Loop@@              bsr       GetChar             ; $BC1B 8D 18
                    bsr       PutChar             ; $BC1D 8D 21
                    sta       ,x                  ; $BC1F A7 00
                    inx                           ; $BC21 08
                    cmpx      #BUF6+BUF6_LEN-1    ; $BC22 8C 00 E5
                    bne       Loop@@              ; $BC25 26 F4

                    tab                           ; $BC27 16
                    tsta                          ; $BC28 4D
                    bmi       LBC49               ; $BC29 2B 1E
                    beq       LBC49               ; $BC2B 27 1C
                    deca                          ; $BC2D 4A
                    beq       LBC73               ; $BC2E 27 43
                    deca                          ; $BC30 4A
                    beq       BulkErase.Hook      ; $BC31 27 3C
                    bra       LBC82               ; $BC33 20 4D

;*******************************************************************************

GetChar             proc
                    ldb       SCSR                ; $BC35 F6 10 2E
                    andb      #$20                ; $BC38 C4 20
                    beq       GetChar             ; $BC3A 27 F9
                    lda       SCDR                ; $BC3C B6 10 2F
                    rts                           ; $BC3F 39

;*******************************************************************************

PutChar             proc
                    tst       SCSR                ; $BC40 7D 10 2E
                    bpl       PutChar             ; $BC43 2A FB
                    sta       SCDR                ; $BC45 B7 10 2F
                    rts                           ; $BC48 39

;*******************************************************************************

LBC49               proc
                    ldx       >Head               ; $BC49 FE 00 E0
Loop@@              lda       #$FF                ; $BC4C 86 FF
                    cmpa      ,x                  ; $BC4E A1 00
                    bne       LBC5B               ; $BC50 26 09
                    cmpx      >Tail               ; $BC52 BC 00 E2
                    beq       LBC5A               ; $BC55 27 03
                    inx                           ; $BC57 08
                    bra       Loop@@              ; $BC58 20 F2

LBC5A               deca                          ; $BC5A 4A
LBC5B               bsr       PutChar             ; $BC5B 8D E3
                    tstb                          ; $BC5D 5D
                    bmi       LBC68               ; $BC5E 2B 08
                    cmpa      #$FE                ; $BC60 81 FE
                    beq       LBC86               ; $BC62 27 22
                    bsr       GetChar             ; $BC64 8D CF
                    beq       LBC86               ; $BC66 27 1E
LBC68               bsr       GetChar             ; $BC68 8D CB
                    bsr       PutChar             ; $BC6A 8D D4
                    bsr       LBC84               ; $BC6C 8D 16
                    wai                           ; $BC6E 3E
BulkErase.Hook      bra       BulkErase           ; $BC6F 20 47

LBC71               bsr       GetChar             ; $BC71 8D C2
LBC73               ldx       >Head               ; $BC73 FE 00 E0
LBC76               lda       ,x                  ; $BC76 A6 00
                    bsr       PutChar             ; $BC78 8D C6
                    cmpx      >Tail               ; $BC7A BC 00 E2
                    beq       LBC68               ; $BC7D 27 E9
                    inx                           ; $BC7F 08
                    bra       LBC76               ; $BC80 20 F4

;-------------------------------------------------------------------------------
; Long branch "hook" table
;-------------------------------------------------------------------------------

LBC82               bra       EraseBytes          ; $BC82 20 21
LBC84               bra       Delay4ms            ; $BC84 20 48

;*******************************************************************************

LBC86               proc
                    ldx       >Head               ; $BC86 FE 00 E0
Loop@@              bsr       GetChar             ; $BC89 8D AA
                    bsr       PutChar             ; $BC8B 8D B3
                    sta       >Buffer             ; $BC8D B7 00 E6
                    ora       #$08                ; $BC90 8A 08
                    sta       ,x                  ; $BC92 A7 00
                    lda       >Buffer             ; $BC94 B6 00 E6
                    ldb       #$22                ; $BC97 C6 22
                    bsr       BurnEEPROM          ; $BC99 8D 3C
                    cmpx      >Tail               ; $BC9B BC 00 E2
                    beq       LBC71               ; $BC9E 27 D1
                    inx                           ; $BCA0 08
                    bra       Loop@@              ; $BCA1 20 E6

;*******************************************************************************

LBCA3               bra       LBC49               ; $BCA3 20 A4

;*******************************************************************************
; Purpose: Erase a series of EEPROM bytes
; Input  :
; Output :

EraseBytes          proc
                    ldx       >Head               ; $BCA5 FE 00 E0
                    lda       #ERASED_STATE       ; $BCA8 86 FF
                    sta       ,x                  ; $BCAA A7 00
Loop@@              ldb       #eeByteErase        ; $BCAC C6 16
                    bsr       BurnEEPROM          ; $BCAE 8D 27
                    inx                           ; $BCB0 08
                    cmpx      >Tail               ; $BCB1 BC 00 E2
                    bls       Loop@@              ; $BCB4 23 F6
                    bra       LBCA3               ; $BCB6 20 EB

;*******************************************************************************
; Purpose: Bulk erase all EEPROM
; Input  :
; Output :

BulkErase           proc
                    ldx       >Head               ; $BCB8 FE 00 E0
                    lda       #ERASED_STATE       ; $BCBB 86 FF
                    sta       ,x                  ; $BCBD A7 00
                    ldb       #eeBulkErase        ; $BCBF C6 06
                    bsr       BurnEEPROM          ; $BCC1 8D 14
                    bra       LBCA3               ; $BCC3 20 DE

;-------------------------------------------------------------------------------

                    #Cycles   6
Delay10ms           proc
                    ldy       #3072               ; $BCC5 18 CE 0C 00
                    #Cycles
Loop@@              dey                           ; $BCC9 18 09
                    bne       Loop@@              ; $BCCB 26 FC
                    #temp     :cycles
                    rts                           ; $BCCD 39

                    @ShowDelay 3072

;-------------------------------------------------------------------------------

                    #Cycles   6
Delay4ms            proc
                    ldy       #1143               ; $BCCE 18 CE 04 77
                    #Cycles
Loop@@              dey                           ; $BCD2 18 09
                    bne       Loop@@              ; $BCD4 26 FC
                    #temp     :cycles
                    rts                           ; $BCD6 39

                    @ShowDelay 1143

;*******************************************************************************
; Purpose: Common EEPROM routine for various actions
; Input  : A = Value to write to EEPROM
;        : B = Code for PPROG
; Output : None

BurnEEPROM          proc
                    stb       PPROG               ; $BCD7 F7 10 3B
                    sta       ,x                  ; $BCDA A7 00
                    inc       PPROG               ; $BCDC 7C 10 3B
                    bitb      #$20                ; $BCDF C5 20
                    beq       Final@@             ; $BCE1 27 04
                    bsr       Delay4ms            ; $BCE3 8D E9
                    bra       Done@@              ; $BCE5 20 02

Final@@             bsr       Delay10ms           ; $BCE7 8D DC
Done@@              clr       PPROG               ; $BCE9 7F 10 3B
                    ldb       #$80                ; $BCEC C6 80
                    rts                           ; $BCEE 39

;*******************************************************************************

GetTimedChar        proc
                    ldy       #-1                 ; $BCEF 18 CE FF FF
Loop@@              ldb       SCSR                ; $BCF3 F6 10 2E
                    andb      #$20                ; $BCF6 C4 20
                    bne       Get@@               ; $BCF8 26 0E
                    jsr       KickCOP             ; $BCFA BD A7 DB
                    mul:6                         ; $BCFD 3D 3D 3D 3D 3D 3D
                    tsta                          ; $BD03 4D
                    dey                           ; $BD04 18 09
                    bne       Loop@@              ; $BD06 26 EB
Get@@               lda       SCDR                ; $BD08 B6 10 2F
                    rts                           ; $BD0B 39

;*******************************************************************************

InitSCI             proc
                    lda       #$22                ; $BD0C 86 22
                    sta       BAUD                ; $BD0E B7 10 2B

                    lda       SOME_IO             ; $BD11 B6 55 C0
                    ora       #$40                ; $BD14 8A 40
                    anda      #$FD                ; $BD16 84 FD
                    sta       SOME_IO             ; $BD18 B7 55 C0

                    jsr       Delay10ms           ; $BD1B BD BC C5

                    ora       #$01                ; $BD1E 8A 01
                    sta       SOME_IO             ; $BD20 B7 55 C0

                    ldb       #$0C                ; $BD23 C6 0C
                    stb       SCCR2               ; $BD25 F7 10 2D

                    ora       #$80                ; $BD28 8A 80
                    sta       SOME_IO             ; $BD2A B7 55 C0

                    ora       #$04                ; $BD2D 8A 04
                    sta       SOME_IO             ; $BD2F B7 55 C0

                    ora       #$02                ; $BD32 8A 02
                    sta       SOME_IO             ; $BD34 B7 55 C0

                    lda       #$FF                ; $BD37 86 FF
                    jsr       PutChar_Hook        ; $BD39 BD 84 12
                    nop                           ; $BD3C 01

                    ldx       #Initialize         ; $BD3D CE BC 10
Loop@@              lda       ,x                  ; $BD40 A6 00
                    jsr       PutChar_Hook        ; $BD42 BD 84 12
                    jsr       GetTimedChar_Hook   ; $BD45 BD 84 0C
                    inx                           ; $BD48 08
                    cmpx      #$BD0F              ; $BD49 8C BD 0F
                    bls       Loop@@              ; $BD4C 23 F2

                    ldx       #$0FFF              ; $BD4E CE 0F FF
Delay@@             jsr       KickCOP             ; $BD51 BD A7 DB
                    dex                           ; $BD54 09
                    bne       Delay@@             ; $BD55 26 FA

                    lda       #$0C                ; $BD57 86 0C
                    sta       SCCR2               ; $BD59 B7 10 2D

                    lda       SCSR                ; $BD5C B6 10 2E
                    lda       SCDR                ; $BD5F B6 10 2F
                    rts                           ; $BD62 39

;*******************************************************************************

LDB63_Device        proc
                    lda       SOME_IO             ; $BD63 B6 55 C0
                    anda      #%11111101          ; $BD66 84 FD
                    sta       SOME_IO             ; $BD68 B7 55 C0

                    jsr       Delay10ms           ; $BD6B BD BC C5
                    anda      #%11111011          ; $BD6E 84 FB
                    sta       SOME_IO             ; $BD70 B7 55 C0

                    anda      #%01111111          ; $BD73 84 7F
                    sta       SOME_IO             ; $BD75 B7 55 C0

                    clr       SCCR2               ; $BD78 7F 10 2D

                    anda      #%10111111          ; $BD7B 84 BF
                    ora       #$02                ; $BD7D 8A 02
                    sta       SOME_IO             ; $BD7F B7 55 C0

                    anda      #%11111110          ; $BD82 84 FE
                    sta       SOME_IO             ; $BD84 B7 55 C0
                    rts                           ; $BD87 39

;*******************************************************************************

                    #VECTORS

                    dw        COMMON_ISR          ; $BFD6 A9 57
                    dw        COMMON_ISR          ; $BFD8 A9 57
                    dw        COMMON_ISR          ; $BFDA A9 57
                    dw        COMMON_ISR          ; $BFDC A9 57
                    dw        COMMON_ISR          ; $BFDE A9 57
                    dw        COMMON_ISR          ; $BFE0 A9 57
                    dw        COMMON_ISR          ; $BFE2 A9 57
                    dw        COMMON_ISR          ; $BFE4 A9 57
                    dw        COMMON_ISR          ; $BFE6 A9 57
                    dw        COMMON_ISR          ; $BFE8 A9 57
                    dw        COMMON_ISR          ; $BFEA A9 57
                    dw        COMMON_ISR          ; $BFEC A9 57
                    dw        COMMON_ISR          ; $BFEE A9 57
                    dw        COMMON_ISR          ; $BFF0 A9 57
                    dw        COMMON_ISR          ; $BFF2 A9 57
                    dw        COMMON_ISR          ; $BFF4 A9 57
                    dw        SWI_Handler         ; $BFF6 A8 6A
                    dw        Start               ; $BFF8 A6 01
                    dw        COMMON_ISR          ; $BFFA A9 57
                    dw        COMMON_ISR          ; $BFFC A9 57
          #ifdef F1
RESET_VECTOR        dw        StartF1
          #else
RESET_VECTOR        dw        Start               ; $BFFE A6 01
          #endif
                    end       :s19crc

                    #Message  ................: 9320 bytes, RAM:.... 7, CRC: $C20E

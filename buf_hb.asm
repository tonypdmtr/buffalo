;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
; Buffalo for the Handy Board
; Optimized and modified for ASM11 by Tony Papadimitriou <tonyp@acm.org>

                    #ExtraOn

VERSION             equ       210                 ; version as x.xx

CR                  equ       $0D
LF                  equ       $0A

;
;  HC11 - Serial Equates
;
SPCR                equ       $1028               ; SPI Control Register
SPSR                equ       $1029
SPDR                equ       $102A               ; SCI Data Register
BAUD                equ       $102B               ; SCI Baud Rate Control Register
SCCR1               equ       $102C               ; SCI Control Register 1
SCCR2               equ       $102D               ; SCI Control Register 2
SCSR                equ       $102E               ; SCI Status Register
SCDR                equ       $102F               ; SCI Data
TDRE                equ       $80                 ; Transmit Data Register Empty
TRENA               equ       $0C                 ; Transmit, Receive ENAble
RDRF                equ       $20                 ; Receive Data Register Full
PD_WOM              equ       $20
B9600               equ       $B0
;
;  HC11 - PORT Equates
;
PORTA               equ       $00                 ; Port A data register
PORTB               equ       $04                 ; Port B data register
PORTC               equ       $03                 ; Port C latched data register
PORTD               equ       $08                 ; Port D data register
PORTE               equ       $0A                 ; Port E data register
DDRC                equ       $07                 ; Data Direction register for port C
HPRIO               equ       $3C                 ; Highest Priority Interrupt and misc.
;
;  HC11 - Timer / Speaker Equates
;
PORTA1              equ       $1000               ; Port A data register
TCNT                equ       $100E               ; Actual free running timer count
TOC5                equ       $101E               ; OC5 timer compare value
TCTL1               equ       $1020               ; Timer count control
TFLG1               equ       $1023               ; Timer compare flag
PERIOD              equ       $3000               ; half period for 10mS (100Hz)
TIME                equ       $0012               ; counter to set duration correct
FCM                 equ       %00001000           ; OC1 flag set & clear mask
PA3_M               equ       %11110111           ; mask for PA3 in Port A
SET_T               equ       %11111100           ; mask to make lower 2 bits zero
;
;  HC11 - Interrupt Equates
;
IRQ                 equ       $FFF2
XIRQ                equ       $FFF4
SWI                 equ       $FFF6
ILOP                equ       $FFF8
COPF                equ       $FFFA
COPC                equ       $FFFC
;
;  My Display Equates
;
LCDtmpX             equ       $0D
LCDtmpA             equ       $0F
LCDrout             equ       $10
PROCHR              equ       '>'
; ________________________________________________________________________________
;
;  Stack Pointer Location
;
SPLOC               equ       $BFEF
; ________________________________________________________________________________
;
;  Monitor JSR Locations
;
PROMPT              equ       $C050
GET                 equ       $C053
PUT                 equ       $C056
CLRDISP             equ       $C059
SENDCH              equ       $C05C
SENDSTR             equ       $C05F
DISNUM              equ       $C062
SOUND               equ       $C065
;
;  Monitor BUFFERS Locations
;
DISBUF              equ       $C000               ; $C000 - $C01F
KEY                 equ       $C030
DUR                 equ       $C031
FREQ                equ       $C032
;
; ________________________________________________________________________________
;
                    org       $BFFE               ; Reset Vector
                    fdb       $C050               ; org start of Monitor
; ________________________________________________________________________________
;
; DISPLAY BUFFER
;
                    org       $C000
;
Line1               fcc       '################'
Line2               fcc       '################'
Lend                fcb       $00
; ________________________________________________________________________________
;
;  BUFFER TABLE
;
                    org       $C030
                    fcb       $00                 ; KEY location - $C030
                    fcb       $0A                 ; DUR location - $C031
                    fcb       $0A                 ; FREQ location - $C032
; ________________________________________________________________________________
;
;  JUMP TABLE
;
                    org       $C050
                    !jmp      Start               ; PROMPT location - $C050
                    !jmp      Getky               ; GET location - $C053
                    !jmp      WrtDis              ; PUT location - $C056
                    !jmp      ClrDis              ; CLRDISP location - $C059
                    !jmp      PutCh               ; SENDCH location - $C05C
                    !jmp      OutStrg             ; SENDSTR loaction - $C05F
                    !jmp      PNum                ; DISNUM location - $C062
                    !jmp      MNoise              ; SOUND location - $C065
; ________________________________________________________________________________
;
;  Start of Prog - PROMPT
;
Start               lds       #SPLOC              ; Set Stack Pointer
                    ldx       #Start              ; Set up Interruputs to Prompt
                    stx       IRQ
                    stx       XIRQ
                    stx       SWI
                    stx       ILOP
                    stx       COPF
                    stx       COPC

                    ldx       #$1000              ; Set up Serial for 9600 bps transmission
                    bclr      [SPCR,x,PD_WOM
                    lda       #B9600
                    sta       [BAUD,x
                    lda       #TRENA
                    sta       [SCCR2,x
; ________________________________________________________________________________
;
;  Set up Display
;
                    jsr       CPYprt              ; Copy Display routine to Zero Page

                    ldd       #$0C                ; Display On / Cursor Off / Flash Off
                    jsr       LCDrout

                    ldd       #$38                ; Two Line Display
                    jsr       LCDrout

                    ldd       #$03                ; Home and Clear Display
                    jsr       LCDrout
; ________________________________________________________________________________
;
                    clr       $7000               ; Turn off Motor Outputs
; ________________________________________________________________________________
;
; Copy HB_Buffalo Version to Buffer and Display
;
Versn               ldx       #Line1
                    ldy       #VersionMsg
CpyV                lda       ,y
                    sta       ,x
                    iny
                    inx
                    cpx       #Lend
                    bne       CpyV
; ________________________________________________________________________________
;
;  Display Version message to Terminal
;
                    jsr       WrtDis
                    ldx       #Line1              ; buffalo message
                    jsr       OutStrg
                    jsr       OutCRLF
; ________________________________________________________________________________
;
;  Input User Commands from Serial Line  ######### MAIN LOOP ############
;
MAIN                lds       #SPLOC              ; Initialize SP every time
                    jsr       OutCRLF
                    lda       #PROCHR             ; prompt user '>'
                    jsr       PutCh
                    jsr       ClrDis
                    clrb
MAIN1               pshb
                    jsr       GetCh               ; Read terminal
                    jsr       PutCh
                    cmpa      #$08
                    bne       MAIN2               ; jump if not bckspc
                    ldx       #Line1

;  NEED CLEAR BUFFER ON BACKSPACE

                    pulb
                    abx                           ; pointer into buffer
                    lda       #' '
                    sta       ,x
                    tstb
                    beq       MAIN                ; jump if buffer empty
                    decb
                    bra       MAIN1

MAIN2               cmpa      #CR
                    beq       COMDO               ; jump if Return
                    ldx       #Line1
                    pulb
                    abx                           ; pointer into buffer
                    sta       ,x
                    incb
                    cmpb      #32
                    bne       MAIN1               ; jump if not long
                    ldx       #LngMsg             ; Print Long Message
                    jsr       OutStrg
                    bra       MAIN

; ________________________________________________________________________________

;

; Do Command

;

CmdNm               fcb       0                   ; Num Commands
CmdWd               fcc       'XX'                ; Word
CmdA1               fcc       '00'                ; Add1
CmdA2               fcc       '00'                ; Add2 or Data
CmdD                fcc       '00'                ; Data
CmdAd               fcc       '00'                ; Address Store

COMDO               jsr       OutCRLF
                    ldx       #Line1              ; Convert commands to UpperCase
UpCas               lda       ,x
                    cmpa      #'a'
                    blt       UpCas1              ; jump if < a
                    cmpa      #'z'
                    bgt       UpCas1              ; jump if > z
                    suba      #$20
                    sta       ,x
UpCas1              inx
                    cpx       #Lend
                    bne       UpCas

                    jsr       WrtDis              ; Print command to Display

                    clr       CmdNm               ; State No Commands (yet)
                    ldx       #Line1              ; Parse Command
                    bsr       SkipB
                    sta       CmdWd               ; Store Cmd Char1
                    lda       1,x
                    sta       CmdWd+1             ; Store Cmd Char1
                    inc       CmdNm               ; Got First Command So Num Commands =1
                    inx:2
                    bsr       SkipW
                    bsr       GetNum              ; Get First Addess
                    ldd       CmdAd
                    std       CmdA1
                    inc       CmdNm               ; Got First Command So Num Commands =2
                    bsr       SkipB
                    bsr       GetNum
                    ldd       CmdAd
                    std       CmdA2
                    inc       CmdNm               ; Got Second Command So Num Commands =3
                    bsr       SkipB
                    bsr       GetNum
                    ldd       CmdAd
                    std       CmdD
                    inc       CmdNm               ; Got Third Command So Num Commands =4
                    bra       DetCom

SkipW               lda       ,x                  ; Skip Remaining Words then WhiteSpace
                    cmpa      #','
                    beq       SkipB
                    cmpa      #' '
                    beq       SkipB
                    cmpa      #$09                ; tab
                    beq       SkipB
                    inx
                    cpx       #Lend
                    bne       SkipW
                    bra       DetCom

SkipB               lda       ,x                  ; Skip White Space
                    cmpa      #$2C                ; comma
                    beq       NeCh
                    cmpa      #$20                ; space
                    beq       NeCh
                    cmpa      #$09                ; tab
                    beq       NeCh
                    rts

NeCh                inx
                    cpx       #Lend
                    bne       SkipB
                    bra       DetCom

GetNum              clrb
                    stb       CmdAd
                    stb       CmdAd+1
GetA                lda       ,x
                    cmpa      #'0'
                    blt       EGet                ; jump if < 0
                    cmpa      #'9'
                    bgt       Get1                ; jump if > 9
                    incb                          ; Is Num INC digits
                    suba      #'0'
                    bra       AddIt

Get1                cmpa      #'A'                ; jump if < A
                    blt       EGet
                    cmpa      #'F'
                    bgt       EGet
                    suba      #55
                    incb                          ; Is Num INC digits
AddIt               asl       CmdAd+1
                    rol       CmdAd
                    asl       CmdAd+1
                    rol       CmdAd
                    asl       CmdAd+1
                    rol       CmdAd
                    asl       CmdAd+1
                    rol       CmdAd
                    adda      CmdAd+1
                    sta       CmdAd+1
                    inx
                    bra       GetA

EGet                cmpb      #04
                    bgt       DetCom
                    tstb
                    beq       DetCom
                    rts

DetCom              lda       CmdNm               ; Check that more than Zero Commands
                    jeq       Versn
                    lda       CmdWd               ; Check If Load
                    cmpa      #'L'
                    bne       Next1
                    lda       CmdWd+1
                    cmpa      #'O'
                    bne       Next1
                    jsr       LOAD
                    jmp       MAIN

Next1               lda       CmdWd               ; Check If BFILL
                    cmpa      #'B'
                    bne       Next2
                    lda       CmdWd+1
                    cmpa      #'F'
                    bne       Next2
                    jsr       BFILL
                    jmp       MAIN

Next2               lda       CmdWd               ; Check If CALL
                    cmpa      #'C'
                    bne       Next3
                    lda       CmdWd+1
                    cmpa      #'A'
                    bne       Next3
                    jsr       CALL
                    jmp       MAIN

Next3               lda       CmdWd               ; Check If GO
                    cmpa      #'G'
                    bne       Next4
                    lda       CmdWd+1
                    cmpa      #'O'
                    bne       Next4
                    jsr       GO
                    jmp       MAIN

Next4               lda       CmdWd               ; Check If Move / MD / MM
                    cmpa      #'M'
                    bne       HlpEd
                    lda       CmdWd+1
                    cmpa      #'O'                ; Check If MO
                    bne       Next5
                    jsr       MOVE
                    jmp       MAIN

Next5               cmpa      #'D'                ; Check If MD
                    bne       Next6
                    jsr       MEMD
                    jmp       MAIN

Next6               cmpa      #'M'                ; Check If MM
                    bne       HlpEd
                    jsr       MMOD
                    jmp       MAIN

HlpEd               bsr       HELP
                    jmp       MAIN

; ________________________________________________________________________________

;

; Do Help Command

;

HDm                 fcc       'UNKNOWN COMMAND '
HDm1                fcc       ' DISPLAYING HELP'
HELP                ldx       #HDm
                    ldy       #Line1
HLP1                lda       ,x
                    sta       ,y
                    inx
                    iny
                    cpy       #Lend
                    bne       HLP1
                    jsr       WrtDis
                    ldx       #HELPMSG
                    jmp       OutStrg             ; print help screen to Terminal

HELPMSG             fcc       'HANDYBOARD BUFFALO',CR,LF
                    fcc       '  LOAD                          = Load S-records.',CR,LF
                    fcc       '  BF <addr1> <addr2> [<data>]   = Block fill.',CR,LF
                    fcc       '  MOVE <Sadr1> <Dadr2> [<num>]  = Block move.',CR,LF
                    fcc       '  MD <addr1> [<addr2>]          = Memory dump.',CR,LF
                    fcc       '  MM <addr> <data>              = Memory modify.',CR,LF
                    fcc       '  CALL <addr>                   = Call user subroutine.',CR,LF
                    fcs       '  GO <addr>                     = Execute user code.',CR,LF
; ________________________________________________________________________________
;
; Do Load Command
;
MSG12               fcs       'Checksum error'
MSG14               fcs       'Receiver error'
MSG11               fcs       'Completed Load'
;
SHFTREG             rmb       2                   ; input shift register
AUTOLF              rmb       1                   ; auto lf flag for i/o
TMP1                fcb       0                   ; main,hexbin,buffarg,termarg
TMP2                fcb       0
TMP3                fcb       0
TMP4                fcb       0
PTR3                rmb       2

LOAD                clr       TMP2                ; 0=load
                    clr       TMP3                ; clear error flag
LOAD10              jsr       GetCh               ; Read terminal
                    cmpa      #'S'
                    bne       LOAD10              ; jump if not S
                    jsr       OutCRLF
                    lda       #'S'
                    jsr       PutCh
LOAD12              jsr       GetCh               ; Read terminal
                    cmpa      #'9'
                    beq       LOAD90              ; jump if S9 record
                    cmpa      #'1'
                    bne       LOAD10              ; jump if not S1
                    jsr       PutCh
                    clr       TMP4                ; clear checksum
                    bsr       BYTE
                    ldb       SHFTREG+1
                    subb      #2                  ; b = byte count
                    bsr       BYTE
                    bsr       BYTE
                    ldx       SHFTREG             ; x = base address
                    dex
LOAD20              bsr       BYTE                ; get next byte
                    inx
                    decb                          ; check byte count
                    beq       LOAD30              ; of b=0, go do checksum
                    tst       TMP3
                    bne       LOAD10              ; jump if error flagged
                    tst       TMP2
                    bne       LOAD21              ; jump if verify
                    lda       SHFTREG+1
                    sta       ,x
LOAD21              cmpa      ,x                  ; verify ram location
                    beq       LOAD20              ; jump if ram ok
                    lda       #$02
                    sta       TMP3                ; indicate rom error
                    stx       PTR3                ; save error address
                    bra       LOAD20              ; finish download

; calculate checksum

LOAD30              tst       TMP3
                    bne       LOAD10              ; jump if error already
                    lda       TMP4
                    inca                          ; do checksum
                    beq       LOAD10              ; jump if s1 record okay
                    lda       #$03
                    sta       TMP3                ; indicate checksum error
                    bra       LOAD10

LOAD90              jsr       PutCh
                    bsr       BYTE
                    ldb       SHFTREG+1           ; b = byte count
LOAD91              bsr       BYTE
                    decb
                    bne       LOAD91              ; loop until end of record
                    inc       AUTOLF              ; turn on autolf
                    ldx       #MSG11              ; "done" default msg
                    lda       TMP3
                    cmpa      #$02
                    bne       LOAD92              ; jump not rom error
                    ldx       #PTR3
                    jsr       OUT2BSP             ; address of rom error
                    bra       LOAD95

LOAD92              cmpa      #$01
                    bne       LOAD93              ; jump not rcv error
                    ldx       #MSG14              ; "rcv error"
                    bra       LOAD94

LOAD93              cmpa      #$03
                    bne       LOAD94              ; jump not checksum error
                    ldx       #MSG12              ; "checksum error"
LOAD94              jsr       OutStrg
                    jsr       OutCRLF
LOAD95              rts

BYTE                pshb
                    pshx
BYTE0               jsr       GetCh               ; Read terminal
                    jsr       PutCh
                    bsr       HEXBIN
BYTE1               jsr       GetCh               ; Read terminal
                    jsr       PutCh
                    bsr       HEXBIN
                    lda       SHFTREG+1
                    adda      TMP4
                    sta       TMP4                ; add to checksum
                    pulx
                    pulb
                    rts

HEXBIN              psha
                    pshb
                    pshx
                    cmpa      #'a'
                    blt       UpCas2              ; jump if < a
                    cmpa      #'z'
                    bgt       UpCas2              ; jump if > z
                    suba      #$20
UpCas2              cmpa      #'0'
                    blt       HEXNOT              ; jump if a < $30
                    cmpa      #'9'
                    ble       HEXNMB              ; jump if 0-9
                    cmpa      #'A'
                    blt       HEXNOT              ; jump if $39> a <$41
                    cmpa      #'F'
                    bgt       HEXNOT              ; jump if a > $46
                    adda      #$9                 ; convert $A-$F
HEXNMB              anda      #$0F                ; convert to binary
                    ldx       #SHFTREG
                    ldb       #4
HEXSHFT             asl       1,x                 ; 2 byte shift through
                    rol       ,x                  ; carry bit
                    decb
                    bgt       HEXSHFT             ; shift 4 times
                    ora       1,x
                    sta       1,x
                    bra       HEXRTS

HEXNOT              inc       TMP1                ; indicate not hex
HEXRTS              pulx
                    pulb
                    pula
                    rts

; ________________________________________________________________________________

;

; Do BF Command

;

LdMs1               fcs       'Missing or Incorrect Addresses for Block Fill'
BFILL               lda       CmdNm
                    cmpa      #03
                    bge       BFO1
                    ldx       #LdMs1
                    jmp       OutStrg             ; Print Error Message

BFO1                clrb
                    cmpa      #04
                    bne       BFO3
                    ldb       CmdD+1
BFO3                ldx       CmdA1
BFO2                stb       ,x
                    inx
                    cpx       CmdA2
                    bne       BFO2
                    rts

; ________________________________________________________________________________

;

; Do MOVE Command

;

ErrMM               fcs       'Missing or Incorrect Addresses for Memory Move'
MOVE                lda       CmdNm
                    cmpa      #03
                    bge       MMO1
                    ldx       #ErrMM
                    jmp       OutStrg             ; Print Error Message

MMO1                ldb       #01
                    cmpa      #04
                    bne       MMO3
                    ldb       CmdD+1
MMO3                ldx       CmdA1
                    ldy       CmdA2
MMO2                lda       ,x
                    sta       ,y
                    inx
                    iny
                    decb
                    bne       MMO2
                    rts

; ________________________________________________________________________________

;

; Do MemDump Command

;

StM                 fcc       '00'
StE                 fcc       '00'
ErrMD               fcs       'Missing or Incorrect Address for Memory Dump'
MEMD                ldx       CmdA1
                    stx       StM
                    lda       CmdNm
                    cmpa      #01
                    bne       MEM1
                    ldx       #ErrMD
                    jmp       OutStrg             ; Print Error Message

MEM1                cmpa      #02
                    bne       MEM2
                    ldy       CmdA1
                    ldb       #$0F
                    aby
                    sty       StE
                    bra       GoPM

MEM2                ldx       CmdA2
                    stx       StE
GoPM                ldx       #StM
                    jsr       OUT2BSP
                    lda       #'-'
                    jsr       PutCh
                    lda       #' '
                    jsr       PutCh
                    ldx       StM
                    ldy       #$10
AgPM                jsr       OUT1BSP
                    dey
                    bne       AgPM
                    jsr       OutCRLF
                    ldd       StM
                    addd      #16
                    std       StM
                    cpd       StE
                    ble       GoPM
                    rts

; ________________________________________________________________________________

;

; Do MemModify Command

;

LdMs4               fcs       'Missing or Incorrect Address / Data for Memory Modify'
MMOD                lda       CmdNm
                    cmpa      #03
                    beq       MM1
                    ldx       #LdMs4
                    jmp       OutStrg             ; Print Error Message

MM1                 ldx       #CmdA1
                    jsr       OUT2BSP
                    lda       #':'
                    jsr       PutCh
                    lda       #' '
                    jsr       PutCh
                    ldx       CmdA1
                    jsr       OUT1BSP
                    lda       #'-'
                    jsr       PutCh
                    lda       #'>'
                    jsr       PutCh
                    lda       #' '
                    jsr       PutCh
                    lda       CmdA2+1
                    ldx       CmdA1
                    sta       ,x
                    jsr       OUT1BSP
                    jmp       OutCRLF

; ________________________________________________________________________________

;

; Do CALL Command

;

LdMs5               fcs       'Missing or Incorrect Address for CALL'
CALL                lda       CmdNm
                    cmpa      #02
                    bge       COO1
                    ldx       #LdMs5
                    jmp       OutStrg             ; Print Error Message

COO1                ldx       CmdA1
                    jmp       ,x

; ________________________________________________________________________________

;

; Do GO Command

;

LdMs6               fcs       'Missing or Incorrect Address for GO'
GO                  lda       CmdNm
                    cmpa      #02
                    bge       GOO1
                    ldx       #LdMs6
                    bra       OutStrg             ; Print Error Message

GOO1                lds       #$BFEF              ; Initialize SP every time
                    ldx       CmdA1
                    jmp       ,x

; ________________________________________________________________________________
;
; Get and Put Character [with wait] (AccA) to/from Serial Line
;

GetCh               lda       SCSR
                    anda      #RDRF
                    beq       GetCh
                    lda       SCDR
                    rts

PutCh               ldb       SCSR
                    andb      #TDRE
                    beq       PutCh
                    sta       SCDR
                    rts

Getky               ldx       #DPrt               ; Anounce to terminal
                    bsr       OutStrg
wtky                lda       SCSR                ; Get Character
                    anda      #RDRF
                    beq       wtky
                    lda       SCDR
                    sta       KEY
                    bsr       PutCh               ; Write Character back to terminal
                    cmpa      #'0'                ; Check if 0 - 9
                    blt       STR0
                    cmpa      #'9'
                    bgt       HexU
                    suba      #'0'
                    rts

HexU                cmpa      #'A'                ; Check if A - F
                    blt       STR0
                    cmpa      #'F'
                    bgt       HexL
                    suba      #55
                    rts

HexL                cmpa      #'a'                ; Check if a - f
                    blt       STR0
                    cmpa      #'f'
                    bgt       STR0
                    suba      #87
                    rts

STR0                clra
OUTS3               rts

DPrt                fcs       'Data = '

OutStrg             bsr       OutCRLF
OUTS0               lda       ,x                  ; read char into acc A
                    beq       OUTS3               ; jump if End
                    bsr       PutCh               ; output character
                    inx
                    bra       OUTS0               ; jump if no input

OutCRLF             lda       #CR                 ; Carriage Return
                    bsr       PutCh               ; output acc A
                    lda       #LF
                    bra       PutCh               ; output Line Feed

OUTLHLF             lsra:4                        ; shift data to right
OUTRHLF             anda      #$0F                ; mask top half
                    adda      #'0'                ; convert to ascii
                    cmpa      #'9'
                    ble       OUTA                ; jump if 0-9
                    adda      #$07                ; convert to hex A-F
OUTA                bra       PutCh               ; output character

OUT1BYT             psha
                    lda       ,x                  ; get data in a
                    psha                          ; save copy
                    bsr       OUTLHLF             ; output left half
                    pula                          ; retrieve copy
                    bsr       OUTRHLF             ; output right half
                    pula
                    inx
                    rts

OUT2BSP             bsr       OUT1BYT             ; do first byte
OUT1BSP             bsr       OUT1BYT             ; do next byte
OUTSPAC             lda       #$20                ; output a space
                    jmp       PutCh

; ________________________________________________________________________________

;

;  Write the Byte value in AccA to the buffer (by X Reg) as two letters ie '0C'

;

StAv                fcb       0
PNum                sta       StAv                ; High Nibble
                    rora:4
                    anda      #$0F
                    cmpa      #09
                    ble       NoAd1
                    adda      #07
NoAd1               adda      #'0'
                    sta       $00,x
                    lda       StAv                ; Low Nibble
                    anda      #$0F
                    cmpa      #09
                    ble       NoAd2
                    adda      #07
NoAd2               adda      #'0'
                    sta       $01,x
                    rts

; ________________________________________________________________________________

;

; Sound Speaker at tone FREQ for length DUR

;

MNoise              ldd       DUR
                    psha                          ; preserve these values
                    pshb
                    mul
                    xgdy                          ; duration counter now in index Y
                    ldb       FREQ
                    bne       OKAY
                    ldb       #$01
OKAY                clra
                    xgdx                          ; freq now in index X
                    ldd       #PERIOD
                    idiv                          ; index X contains delay for correct frequency
                    stx       DUR

                    lda       PORTA1              ; initiliase timer
                    anda      #PA3_M              ; make PA3 not under OC1 control
                    sta       PORTA1
                    lda       TCTL1
                    anda      #SET_T
                    inca                          ; bit0 =1 , bit1 =0: toggle mode for PA3
                    sta       TCTL1

                    lda       #FCM                ; clear OC5 flag
                    sta       TFLG1

                    ldd       TCNT                ; set current count+period in output compare reg
                    addd      DUR
                    std       TOC5
; wait until count is reached
AGAIN               ldx       #TIME
LP20                lda       TFLG1               ; read flags
                    anda      #FCM                ; isolate OC5 flag
                    beq       LP20                ; if not set then wait

                    lda       #FCM                ; if flag set then ...............
                    sta       TFLG1               ; clear OC5 flag
                    ldd       TOC5
                    addd      DUR                 ; update OC5 register
                    std       TOC5
                    dex
                    bne       LP20                ; go again for 10 cycle loop
                    dey
                    bne       AGAIN

                    lda       TCTL1               ; turn buzzer off
                    anda      #SET_T
                    sta       TCTL1
                    pulb                          ; restore settings
                    pula
                    std       DUR
;           RTS
; ________________________________________________________________________________
;
; Write Display Buffer to Screen
;
WrtDis              ldd       #$03                ; Home and Clear Display
                    jsr       LCDrout
                    ldx       #Line1
PL1                 lda       #$02                ; Tell to Print
                    ldb       $00,x
                    jsr       LCDrout
                    inx
                    cpx       #Line2
                    bne       PL1

                    ldx       #24
NewL                lda       #$02                ; Tell to Get to Next Line
                    ldb       #40
                    jsr       LCDrout
                    dex
                    bne       NewL

                    ldx       #Line2
PL2                 lda       #$02                ; Tell to Print
                    ldb       $00,x
                    jsr       LCDrout
                    inx
                    cpx       #Lend
                    bne       PL2
                    rts

; ________________________________________________________________________________

;

; Clear Display Buffer

;

ClrDis              lda       #32
                    ldx       #Line1
Clr1                sta       ,x
                    inx
                    cpx       #Lend
                    bne       Clr1
                    rts

; ________________________________________________________________________________
;
; Reserve Data for Display Buffer
;

VersionMsg          fcc       'HandyBoard      Buffalo     v{VERSION(2)}'
LngMsg              fcs       'Line is too Long!'
; ________________________________________________________________________________
;
;  Copy print routine to Zero Page
;
CPYprt              ldx       #SCRbeg
                    ldy       #LCDrout
LCDloop             lda       ,x
                    sta       ,y
                    inx
                    iny
                    cpx       #SCRend
                    bne       LCDloop
                    rts

; ________________________________________________________________________________
;
;   Print Routine:  A - Command, B - Data
;     Copied to Zero Page memory and Run there
;

SCRbeg              sei                           ; disable interrupts
                    stx       LCDtmpX
                    ldx       #$1000
                    bclr      HPRIO,x,%00100000   ; put into single chip mode
                    bclr      PORTA,x,%00010000   ; turn off LCD E line
                    sta       LCDtmpA             ; Temp A store
                    clr       DDRC,x              ; make port C input
LCDBsy              lda       #1
                    sta       PORTB,x             ; read operation from LCD (AKF-added ',x')
                    bset      PORTA,x,%00010000   ; frob LCD on
                    lda       PORTC,x             ; get status
                    bclr      PORTA,x,%00010000   ; frob LCD off
                    anda      #$80                ; bit 7 is busy flag
                    bne       LCDBsy
                    lda       #$FF
                    sta       DDRC,x              ; make port C output
                    lda       LCDtmpA             ; Temp A store
                    sta       PORTB,x             ; high byte is control
                    stb       PORTC,x             ; low byte is data
                    bset      PORTA,x,%00010000
                    bclr      PORTA,x,%00010000   ; frob LCD
                    bset      HPRIO,x,%00100000   ; put into expanded chip mode
                    ldx       LCDtmpX
                    cli                           ; enable interrupts
                    rts                           ; return to monitor command loop

SCRend              fcb       0

;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
;* 68HC11 Monitor V1.3                                                CRC: $8E5B
;*
;* By : Sylvain Bissonnette
;*      8382 De Fougeray
;*      Anjou, Que.
;*      H1K 1K6
;*******************************************************************************
;* 98.09.15 Adapted to ASM11 and English by Tony G. Papadimitriou <tonyp@acm.org>
;*          Shortened messages to fit even in M68HC811E2 and optimized code in
;*          several places. INCAR changed to return UPPER always.  Also, fixed
;*          a couple or so bugs.
;* 02.04.30 Further optimizations and minor corrections.
;*******************************************************************************

#ifdef ?
  #Hint +---------------------------------------------------
  #Hint | Available conditionals (for use with -Dx option)
  #Hint +---------------------------------------------------
  #Hint | DEBUG: for SIM11x runs (faster bps, etc.)
  #Hint | GREEK: for Greek language messages
  #Hint | E2...: Target is 811E2 MCU (default)
  #Hint | F1...: Target is ASPiSYS F1 Board (32K+32K memory)
  #Hint | WSI..: Target is WSI DK68HC11 Development Kit
  #Hint | MHZ:<value>: 8 or 16 for corresponding crystal
  #Hint +---------------------------------------------------
  #Fatal Run ASM11 -Dx (where x is any of the above)
#endif
                    #ListOff
                    #Uses     mcu.inc             ;found in ASM11 distribution
                    #ListOn

PROMPTCH            def       '>'

#ifdef DEBUG
                    #Hint     Assembling in DEBUG mode (do NOT burn device)

BaudRate            def       $00                 ;max bps for SIM11E
#endif
          #if MHZ = 8
BaudRate            def       $30                 ;9600 bps for normal run
          #else if MHZ = 16
BaudRate            def       $31                 ;9600 bps for normal run
          #else ifdef __WSI__
BaudRate            def       $04                 ;9600 bps for normal run
          #endif

;*******************************************************************************
                    #RAM
;*******************************************************************************

; Usage registers

REGCCR              rmb       1
REGB                rmb       1
REGA                rmb       1
REGX                rmb       2
REGY                rmb       2
REGPC               rmb       2
REGST               rmb       2

; Buffer getkey

QIN                 rmb       2
BUFFER              rmb       4

PRESCALE            rmb       1                   ;PRESCALER VALUE
RESTART             rmb       1                   ;RESTART VALUE
STARTUP             rmb       1                   ;RESET VARIABLE

                    org       $C1                 ; Pseudo-vector registers

PSSCI               rmb       3
PSSPI               rmb       3
PSPAI               rmb       3
PSPAO               rmb       3
PSTO                rmb       3
PSTOC5              rmb       3
PSTOC4              rmb       3
PSTOC3              rmb       3
PSTOC2              rmb       3
PSTOC1              rmb       3
PSTIC3              rmb       3
PSTIC2              rmb       3
PSTIC1              rmb       3
PSRTI               rmb       3
PSIRQ               rmb       3
PSXIRQ              rmb       3
PSSWI               rmb       3
PSIOT               rmb       3
PSCOPF              rmb       3
PSCOPC              rmb       3
PSRESET             rmb       3

;*******************************************************************************
                    #ROM
;*******************************************************************************

Start               proc
                    lda       #$08                ; RESET ON STOP CLOCK
                    sta       OPTION

                    lda       #$AA
                    cmpa      STARTUP
                    bne       Go@@
                    sta       STARTUP

                    lda       PRESCALE            ; SET THE PRESCALER FOR 64xE
                    anda      #%11
                    sta       TMSK2

Go@@                sei                           ; Mask IRQ & FIRQ
                    @SetChipSelects

          #ifdef __WSI__
                    bsr       SetupWSI
          #endif
                    cls                           ; Allow STOPs
                    clr       RESTART             ; Met RESTART a $00 quand power on
                    bsr       INIVEC              ; Initialization of the pseudo-vectors
                    bsr       INISCI              ; Initialization of the ACIA for communication
                    jsr       SNDCRLF
                    bsr       MENU                ; Display the menu

MainLoop            bsr       WAITCOM             ; Wait for a command
                    bra       MainLoop

;*******************************************************************************
;* Wait for a command from the terminal and execute it
;*******************************************************************************

WAITCOM             proc
                    jsr       PROMPT
                    jsr       INCAR

                    cmpa      #'S'                ; Download S19
                    jeq       S1S9

                    cmpa      #'G'                ; Execute program in RAM
                    jeq       EXEC

                    cmpa      #'R'                ; Reset system
                    beq       Start

                    cmpa      #'M'                ; Display Command Menu
                    beq       Menu@@

                    cmpa      #'C'                ; Continue after an SWI
                    beq       Continue@@

                    cmpa      #'E'                ; Examine memory
                    jeq       MEMEX

                    cmpa      #'P'                ; Adjust the prescaler
                    jeq       PSCALE

                    rts

Menu@@              clr       RESTART
                    bra       MENU

Continue@@          jsr       SNDCRLF
                    ldx       #GOMSG1
                    jmp       SNDMSG_CRLF

;*******************************************************************************
;* Menu
;*******************************************************************************

MENU                proc
                    ldx       #MENUMSG            ; Display the copyright
                    jsr       SNDMSG

                    lda       #$AA
                    cmpa      RESTART
                    beq       Done@@

                    sta       RESTART
                    ldx       #MENUMSG2
                    jmp       SNDMSG_CRLF         ; Display the command menu
Done@@              equ       :AnRTS

;*******************************************************************************
;* Initialization of the ACIA for communication 9600 bps, 8 bit, no parity, 1 sb
;*
;* Reg aff: A,CCR
;*******************************************************************************

INISCI              proc
                    lda       #BaudRate
                    sta       BAUD
                    clr       SCCR1               ; 8 BIT, NO WAKE UP, 1 STOP BIT
                    lda       #%1100              ; NO INTERRUPT, TX ENABLE, RX ENABLE
                    sta       SCCR2
                    rts

;*******************************************************************************
;* Initialization of the pseudo-vectors
;*******************************************************************************

INIVEC              proc
                    ldx       #VECTAB
                    ldy       #PSSCI
Loop@@              cmpx      #VECTAB_END
                    beq       Done@@
                    lda       ,x
                    sta       ,y
                    inx
                    iny
                    bra       Loop@@
Done@@              equ       :AnRTS

;*******************************************************************************
;* SWI Interrupt Handler
;*******************************************************************************

SWI_Handler         proc
                    bsr       Copy

Loop@@              jsr       SNDCRLF
                    ldx       #SWIMSG
                    jsr       SNDMSG
                    jsr       INCAR
                    cmpa      #'C'
                    bne       Loop@@
                    rti

;*******************************************************************************
; Used by SWI_Handler (above) and IOT_Handler (below)

Copy                proc
                    tsx
                    dex:2                         ;cancel out the return address
                    ldy       #REGCCR
Loop@@              lda       ,x
                    sta       ,y
                    inx
                    iny
                    cmpy      #REGST
                    bne       Loop@@
                    stx       REGST
                    bra       SWIRES

;*******************************************************************************
;* IOT Interrupt Handler
;*******************************************************************************

IOT_Handler         proc
                    bsr       Copy

                    tsx                           ; Adjust the return address
                    ldd       PC_,x               ;BUG FIXED.  Was: LDB #8
                    incd                          ;                 ABX
                    std       PC_,x               ;                 INC ,x  (increments only lower PC)
                    rti

;*******************************************************************************

SWIRES              proc
                    jsr       SNDCRLF             ; Advance line
                    ldx       #SWMSG1
                    jsr       SNDMSG_CRLF

                    ldx       #SWMSG2             ; REG A
                    jsr       SNDMSG
                    lda       REGA
                    jsr       OUT2HA

                    ldx       #SWMSG3             ; REG B
                    jsr       SNDMSG
                    lda       REGB
                    jsr       OUT2HA

                    ldx       #SWMSG4             ; REG X
                    jsr       SNDMSG
                    ldx       REGX
                    jsr       OUT4HX

                    ldx       #SWMSG5             ; REG Y
                    jsr       SNDMSG
                    ldx       REGY
                    jsr       OUT4HX

                    ldx       #SWMSG6             ; REG S
                    jsr       SNDMSG
                    ldx       REGST
                    jsr       OUT4HX

                    ldx       #SWMSG7             ; REG PC
                    jsr       SNDMSG
                    ldx       REGPC
                    jsr       OUT4HX

                    jsr       SNDCRLF_TWICE

                    ldx       #SWMSG8             ; REG CC
                    jsr       SNDMSG_CRLF
                    ldx       #SWMSG9
                    jsr       SNDMSG
                    ldb       REGCCR
                    clrx
Loop@@              clra
                    lslb

                    adca      #'0'
                    jsr       PutChar

                    lda       #' '
                    jsr       PutChar

                    inx
                    cmpx      #8
                    bne       Loop@@
                    jsr       SNDCRLF_TWICE
                    ldx       #SWMSG10
                    jsr       SNDMSG
                    tsx                           ;Display the Stack Pointer
                    ldb       #8                  ;not counting the stacked frame
                    abx
                    jsr       OUT4HX
                    jmp       SNDCRLF

;*******************************************************************************
;* Interrupt Messages
;*******************************************************************************

SCI                 proc
                    ldx       #SCIMSG
                    bra       MESSINT

SPI                 proc
                    ldx       #SPIMSG
                    bra       MESSINT

PAI                 proc
                    ldx       #PAIMSG
                    bra       MESSINT

PAO                 proc
                    ldx       #PAOMSG
                    bra       MESSINT

TOV                 proc
                    ldx       #TOVMSG
                    bra       MESSINT

TOC5!               proc
                    ldx       #TOC5MSG
                    bra       MESSINT

TOC4!               proc
                    ldx       #TOC4MSG
                    bra       MESSINT

TOC3!               proc
                    ldx       #TOC3MSG
                    bra       MESSINT

TOC2!               proc
                    ldx       #TOC2MSG
                    bra       MESSINT

TOC1!               proc
                    ldx       #TOC1MSG
                    bra       MESSINT

TIC3!               proc
                    ldx       #TIC3MSG
                    bra       MESSINT

TIC2!               proc
                    ldx       #TIC2MSG
                    bra       MESSINT

TIC1!               proc
                    ldx       #TIC1MSG
                    bra       MESSINT

RTI_INT             proc
                    ldx       #RTIMSG
                    bra       MESSINT

IRQ                 proc
                    ldx       #IRQMSG
                    bra       MESSINT

XIRQ                proc
                    ldx       #XIRQMSG
                    bra       MESSINT

SWII                proc
                    ldx       #SWIIMSG
                    bra       MESSINT2

IOTT                proc
                    ldx       #IOTTMSG
                    bra       MESSINT3

COPF                proc
                    ldx       #COPFMSG
                    bra       MESSINT4

COPC                proc
                    ldx       #COPCMSG
                    bra       MESSINT4

RES                 proc
                    ldx       #RESMSG

MESSINT             bsr       CRMSGCR
                    rti

;-------------------------------------------------------------------------------

CRMSGCR             jsr       SNDCRLF
                    jmp       SNDMSG_CRLF

MESSINT2            bsr       CRMSGCR
                    jmp       SWI_Handler

MESSINT3            bsr       CRMSGCR
                    jmp       IOT_Handler

MESSINT4            bsr       CRMSGCR
                    jmp       Start

;*******************************************************************************
;* Examine memory
;*******************************************************************************

MEMEX               proc
                    jsr       SNDCRLF
                    ldx       #EXMSG1
                    jsr       SNDMSG
                    jsr       GET4HX

Loop@@              jsr       SNDCRLF
                    jsr       OUT4HX

                    lda       #':'
                    bsr       PutChar

                    lda       ,x
                    bsr       OUT2HA

GetChar@@           jsr       INCAR

                    cmpa      #'+'
                    beq       Plus@@

                    cmpa      #'-'
                    beq       Minus@@

                    cmpa      #'/'
                    beq       Divide@@

                    cmpa      #CR
                    bne       GetChar@@

                    jsr       INCAR
                    jmp       MENU

Plus@@              inx
                    bra       Loop@@

Minus@@             dex
                    bra       Loop@@

Divide@@            lda       #' '
                    bsr       PutChar

                    lda       #'/'
                    bsr       PutChar

                    jsr       GET2HA
                    sta       ,x
                    bra       Loop@@

;*******************************************************************************
;* Adjust the prescaler
;*******************************************************************************

PSCALE              proc
                    ldx       #PSCALEM
                    bsr       SNDCRLF
                    bsr       SNDMSG
                    jsr       GET1HA
                    sta       PRESCALE

                    cls
Halt@@              sei
                    stop
                    bra       Halt@@              ;Needed to avoid run-away

;*******************************************************************************

PROMPT              proc
                    lda       #CR
                    bsr       PutChar

                    lda       #PROMPTCH           ; Display Prompt Character
;                   bra       PutChar

;*******************************************************************************
;* Send RegA to the serial port
;*
;* Reg aff:CCR
;*******************************************************************************

PutChar             proc
                    cmpa      #LF
                    bne       PutChar@@

                    lda       #CR
                    bsr       PutChar@@

                    lda       #LF

PutChar@@           tst       SCSR
                    bpl       PutChar@@
                    sta       SCDR
                    rts

;*******************************************************************************
;* Routine to transmit the number in RegA to the terminal in ASCII format.
;*
;* Reg aff:CCR
;*******************************************************************************

OUT2HA              proc
                    psha:2
                    lsra:4
                    jsr       HEXASC              ;convert
                    bsr       PutChar             ;transmit the MSB
                    pula
                    anda      #$0F
                    jsr       HEXASC              ;convert
                    bsr       PutChar             ;transmit the LSB
                    pula
                    rts

;*******************************************************************************
;* Routine to transmit the number in RegB to the terminal in ASCII format.
;*
;* Reg aff:CCR
;*******************************************************************************

OUT2HB              proc
                    psha
                    tba
                    bsr       OUT2HA
                    pula
                    rts

;*******************************************************************************
;* Routine to transmit the number in RegX to the terminal in ASCII format.
;*
;* Reg aff:CCR
;*******************************************************************************

OUT4HX              proc
                    pshd
                    pshx
                    xgdx
                    bsr       OUT2HA
                    tba
                    bsr       OUT2HA
                    pulx
                    puld
                    rts

;*******************************************************************************
;* Transmit a Carriage Return
;*
;* Reg aff: CCR
;*******************************************************************************

SNDCRLF_TWICE       proc
                    bsr       SNDCRLF
;                   bra       SNDCRLF

;*******************************************************************************

SNDCRLF             proc
                    psha
                    lda       #LF
                    bsr       PutChar
                    pula
                    rts

;*******************************************************************************
;* Send a string through the serial port
;*
;* X= String address
;*
;* Reg aff:A,x,CC
;*******************************************************************************

SNDMSG              proc
Loop@@              lda       ,x
                    beq       Done@@
                    inx
                    bsr       PutChar
                    bra       Loop@@
Done@@              equ       :AnRTS

;*******************************************************************************

SNDMSG_CRLF         proc
                    bsr       SNDMSG
                    bra       SNDCRLF

;*******************************************************************************
;* LOOKCAR - Probe serial port, and load character if there is one
;*
;* C=1, no character
;* C=0, character in RegA
;*
;* Reg Aff: A,CCR
;*******************************************************************************

LOOKCAR             proc
                    lda       SCSR
                    bita      #$20
                    beq       Fail@@              ;If no char waiting, get out
                    lda       SCDR
                    clc                           ;Character received
                    rts

Fail@@              sec
                    rts

;*******************************************************************************
;* INCAR - Check serial port and return character if one found
;*
;* Reg Aff: A,CCR
;*******************************************************************************

INCAR               proc
                    bsr       LOOKCAR
                    bcs       INCAR
;                   bra       UPPER

;*******************************************************************************
;* Convert to uppercase
;*******************************************************************************

UPPER               proc
                    cmpa      #'a'
                    blo       Done@@

                    cmpa      #'z'
                    bhi       Done@@

                    suba      #'a'-'A'
Done@@              rts

;*******************************************************************************
;* Routine pour lire un chiffre
;* hexa --> LSB de acc.A
;*
;* Reg aff:A,CCR
;*******************************************************************************

IN1HA               proc
                    bsr       INCAR
                    jmp       ASCHEX

;*******************************************************************************
;* Routine pour lire un nombre
;* hexa --> acc.A
;*
;* Reg aff:A,CCR
;*******************************************************************************

IN2HA               proc
                    pshx
                    bsr       IN1HA
                    asla:4
                    psha
                    bsr       IN1HA
                    tsx
                    ora       ,x
                    ins
                    pulx
                    rts

;*******************************************************************************
;* Routine qui attend du terminal une nombre hexa. de 16 bits (4 touches)
;* Le nombre est plac‚ dans le reg.X
;*
;* Reg aff:X,CCR
;*******************************************************************************

IN4HX               proc
                    pshd
                    bsr       IN2HA
                    psha
                    bsr       IN2HA
                    tab
                    pula
                    xgdx
                    puld
                    rts

;*******************************************************************************
;* Routine pour lire un chiffre hexadecimal provenant du terminal.
;* par.d'entree: acc.A = qte de car.
;*                       a recevoir.
;*                       maximum = 4
;* par.de sortie:BUFFER contient
;*               les car. recus
;*******************************************************************************

GETHEX              proc
                    pshx
                    pshb
                    ldx       #BUFFER
                    tab
                    abx
                    stx       QIN
                    ldx       #BUFFER             ;adr. du dernier car a recevoir.
Loop@@              bsr       INCAR
                    cmpa      #BS                 ;BACKSPACE ?
                    bne       CR@@
                    cpx       #BUFFER
                    beq       Loop@@
                    dex
                    jsr       PutChar             ;echo the backspace
                    bra       Loop@@

CR@@                cmpa      #CR                 ;RETURN ?
                    bne       Decode@@
                    cpx       QIN                 ;test si recu tous les car.
                    bne       Loop@@              ;non, attend les autres car.

                    pulb
                    pulx
                    rts

Decode@@            cmpa      #'0'                ;decode si le car. est hexa.
                    blo       Loop@@

                    cmpa      #'F'
                    bhi       Loop@@

                    cmpa      #'9'
                    bls       Q@@

                    cmpa      #'A'
                    blo       Loop@@              ;not hex, restart

Q@@                 cmpx      QIN                 ;si tous les car. sont recus,
                    beq       Loop@@              ;attend la touche RETURN

                    jsr       PutChar             ;echo char
                    suba      #'0'                ;convert ASCII-->hex
                    cmpa      #9
                    bls       Save@@
                    suba      #'A'-'0'-10         ;adjust for codes A-F
Save@@              sta       ,x                  ;save the digit in the buffer
                    inx
                    bra       Loop@@              ;attend prochain car.

;*******************************************************************************
;* Routine pour lire un chiffre
;* hexa --> LSB de acc. A
;*
;* Reg Aff: A,CCR
;*******************************************************************************

GET1HA              proc
                    lda       #1
                    bsr       GETHEX
                    lda       BUFFER
                    rts

;*******************************************************************************
;* Assemble 2 chiffre hexa dans le buffer et les places dans A
;*******************************************************************************

ASS2HEX             proc
                    lda       ,x
                    asla:4
                    psha
                    lda       1,x
                    inx:2
                    pshx
                    tsx
                    ora       2,x
                    pulx
                    ins
                    rts

;*******************************************************************************
;* Routine pour lire un nombre hexa
;* --> acc. A
;*
;* Reg Aff: A,CCR
;*******************************************************************************

GET2HA              proc
                    pshx
                    lda       #2
                    bsr       GETHEX
                    ldx       #BUFFER
                    bsr       ASS2HEX
                    pulx
                    rts

;*******************************************************************************
;* Routine qui attend du terminal un nombre hexa de 16 bits (4 touches)
;* le nombre est placer dans le reg. X
;*
;* Reg Aff: X,CCR
;*******************************************************************************

GET4HX              proc
                    pshd
                    clra
                    bsr       GETHEX
                    ldx       #BUFFER
                    bsr       ASS2HEX
                    psha
                    bsr       ASS2HEX
                    tab
                    pula
                    xgdx
                    puld
                    rts

;*******************************************************************************
;* Convertion de ASCII->HEXA
;*******************************************************************************

ASCHEX              proc
                    suba      #'0'
                    cmpa      #9
                    bls       Done@@
                    suba      #'A'-'0'-10
Done@@              rts

;*******************************************************************************
;* Conversion routine HEXA-->ASCII
;*******************************************************************************

HEXASC              proc
                    adda      #'0'
                    cmpa      #'9'
                    bls       Done@@
                    adda      #'A'-'0'-10
Done@@              rts

;*******************************************************************************
;* Transfer a S19 file
;*******************************************************************************

S1S9                proc
                    jsr       INCAR
                    cmpa      #'0'
                    beq       S1@@
                    cmpa      #'1'
                    beq       S1@@
                    cmpa      #'9'
                    beq       S9@@
                    rts

S1@@                jsr       IN2HA
                    tab
                    subb      #3
                    jsr       IN4HX

Loop@@              jsr       IN2HA
                    sta       ,x
                    inx
                    decb
                    bne       Loop@@
                    jmp       IN2HA

S9@@                jsr       IN4HX
                    jmp       IN4HX

;*******************************************************************************
;* Execute a program in RAM
;*******************************************************************************

EXEC                proc
                    jsr       SNDCRLF
                    ldx       #EXECMSG
                    jsr       SNDMSG
                    bsr       GET4HX
                    ldy       #Start
                    pshy
                    pshx
                    rts

MENUMSG             fcb       ASCII_FF
                    fcc       'ΙΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝ»',LF
          #ifdef F1
                    fcc       'Ί 68HC11 F1 MONITOR 1.3a  Ί',LF
          #else
                    fcc       'Ί 68HC11 EVBU MONITOR 1.3aΊ',LF
          #endif

          #ifdef GREEK
                    #Message  Greek Language

                    fcc       'Ί Από: Συλβέν Μπισσοννέττ Ί',LF
                    fcc       'Ί Mod: Τώνη Παπαδημητρίου Ί',LF
                    fcs       'ΘΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΌ',LF,LF
          #else
                    #Message  English Language

                    fcc       'Ί By: Sylvain Bissonnette Ί',LF
                    fcc       'Ί Mod: Tony Papadimitriou Ί',LF
                    fcs       'ΘΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΌ',LF,LF
          #endif

          #ifdef GREEK
MENUMSG2            fcc       'G: Εκτέλεση κώδικα στην RAM',LF
                    fcc       'R: Επανεκκίνηση συστήματος',LF
                    fcc       'M: Παρουσίαση Μενού',LF
                    fcc       'C: Συνέχιση μετά από SWI',LF
                    fcc       'E: Ελεγχος μνήμης',LF
                    fcc       'P: Ρύθμιση prescaler',LF
                    fcs       'S19 μεταφορά απευθείας',LF
          #else
MENUMSG2            fcc       'G: Run code in RAM',LF
                    fcc       'R: Reset system',LF
                    fcc       'M: Display Menu',LF
                    fcc       'C: Continue after SWI',LF
                    fcc       'E: Examine memory',LF
                    fcc       'P: Adjust prescaler',LF
                    fcs       'S19 upload directly',LF
          #endif

VECTAB              jmp       SCI
                    jmp       SPI
                    jmp       PAI
                    jmp       PAO
                    jmp       TOV
                    jmp       TOC5!
                    jmp       TOC4!
                    jmp       TOC3!
                    jmp       TOC2!
                    jmp       TOC1!
                    jmp       TIC3!
                    jmp       TIC2!
                    jmp       TIC1!
                    jmp       RTI_INT
                    jmp       IRQ
                    jmp       XIRQ
                    jmp       SWII
                    jmp       IOTT
                    jmp       COPF
                    jmp       COPC
                    jmp       Start
VECTAB_END

          #ifdef GREEK
EXECMSG             fcs       'Εκτέλεση από $'
GOMSG1              fcs       "** Αδύνατη η συνέχιση χωρίς προηγούμενο SWI **"
SWIMSG              fcs       'C: Συνέχιση',LF
          #else
EXECMSG             fcs       'Run from $'
GOMSG1              fcs       "** Can't continue without a prior SWI **"
SWIMSG              fcs       'C: Continue',LF
          #endif

SWMSG1              fcc       ' R E G I S T E R S',LF
                    fcs       '-------------------',LF
SWMSG2              fcs       ' A='
SWMSG3              fcs       ' B='
SWMSG4              fcs       ' X='
SWMSG5              fcs       ' Y='
SWMSG6              fcs       ' S='
SWMSG7              fcs       ' PC='
SWMSG8              fcs       ' S X H I N Z V C'
SWMSG9              fcs       ' '
SWMSG10             fcs       ' STACK = '

          #ifdef GREEK
EXMSG1              fcc       '* ΕΞΕΤΑΣΗ ΜΝΗΜΗΣ *',LF,LF
                    fcc       '+ ή - κίνηση <- και -> στην μνήμη',LF
                    fcc       '/ για τροποποίηση περιεχομένων',LF
                    fcc       '<ENTER> για έξοδο',LF
                    fcs       'ΔΙΕΥΘΥΝΣΗ?'
          #else
EXMSG1              fcc       '* EXAMINE MEMORY *',LF,LF
                    fcc       '+ or - move <- and -> in memory',LF
                    fcc       '/ to modify contents',LF
                    fcc       '<ENTER> to exit',LF
                    fcs       'ADDRESS?'
          #endif

          #ifdef GREEK
PSCALEM             fcc       'Διαίρεση με: Πλήκτρο',LF
          #else
PSCALEM             fcc       'Divide by: KeyIn',LF
          #endif
                    fcc       ' 1: 0',LF
                    fcc       ' 4: 1',LF
                    fcc       ' 8: 2',LF
                    fcs       '16: 3',LF,'>'

SCIMSG              fcs       'SCI'
SPIMSG              fcs       'SPI'
PAIMSG              fcs       'PAI'
PAOMSG              fcs       'PAO'
TOVMSG              fcs       'TO'
TOC5MSG             fcs       'TOC5'
TOC4MSG             fcs       'TOC4'
TOC3MSG             fcs       'TOC3'
TOC2MSG             fcs       'TOC2'
TOC1MSG             fcs       'TOC1'
TIC3MSG             fcs       'TIC3'
TIC2MSG             fcs       'TIC2'
TIC1MSG             fcs       'TIC1'
RTIMSG              fcs       'RTI'
IRQMSG              fcs       'IRQ'
XIRQMSG             fcs       'XIRQ'
SWIIMSG             fcs       'SWI'
IOTTMSG             fcs       'ILLOP'
COPFMSG             fcs       'COP Fail'
COPCMSG             fcs       'Clock Monitor Fail'
RESMSG              fcs       'RESET'

;*******************************************************************************
;* Table of user-callable routines
;*******************************************************************************

                    org       $FFC0-36            ; Top of RAM less bytes for vectors (WAS: $FF00)

                    dw        PutChar  ; 00
                    dw        OUT2HA   ; 03
                    dw        OUT2HB   ; 06
                    dw        OUT4HX   ; 09
                    dw        LOOKCAR  ; 0C
                    dw        INCAR    ; 0F
                    dw        IN1HA    ; 10
                    dw        IN2HA    ; 13
                    dw        IN4HX    ; 16
                    dw        GET1HA   ; 19
                    dw        GET2HA   ; 1C
                    dw        GET4HX   ; 1F
                    dw        SNDCRLF  ; 20
                    dw        SNDMSG   ; 23
                    dw        Start    ; 26
                    dw        Start    ; 29
                    dw        Start    ; 2C
                    dw        Start    ; 2F

                    #VECTORS

                    dw        PSSCI     ; FFD6
                    dw        PSSPI     ; FFD8
                    dw        PSPAI     ; FFDA
                    dw        PSPAO     ; FFDC
                    dw        PSTO      ; FFDE
                    dw        PSTOC5    ; FFE0
                    dw        PSTOC4    ; FFE2
                    dw        PSTOC3    ; FFE4
                    dw        PSTOC2    ; FFE6
                    dw        PSTOC1    ; FFE8
                    dw        PSTIC3    ; FFEA
                    dw        PSTIC2    ; FFEC
                    dw        PSTIC1    ; FFEE
                    dw        PSRTI     ; FFF0
                    dw        PSIRQ     ; FFF2
                    dw        PSXIRQ    ; FFF4
                    dw        PSSWI     ; FFF6
                    dw        PSIOT     ; FFF8
                    dw        PSCOPF    ; FFFA
                    dw        Start     ; FFFC
                    dw        Start     ; FFFE

                    end       :s19crc

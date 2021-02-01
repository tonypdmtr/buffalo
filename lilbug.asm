;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
; COMPILED APR 78 BY DIAMOND LIL FOR M6801
; Compiled Feb 2016 by tonyp@acm.org
;*******************************************************************************

; FOR VDG, SET VDG = 0,
; FOR NON-VDG, SET VDG = 1
VDG                 equ       1

; FOR ON-CHIP IO, SET CHIPIO = 0
; FOR ACIA TEST SET CHIPIO = 1
CHIPIO              equ       0

          #ifz      VDG
                    #Message  6801 MONITOR WITH VDG MODS
VDGORG              equ       $100
          #else
                    #Message  6801 DEBUG MONITOR
          #endif

; NAM LILBUG
; COMPILED APR 78 BY DIAMOND LIL FOR M6801

;*************** COMMANDS ****************
; L   LOAD A PROGRAM FROM TAPE
; L <OFFSET>  LOAD FROM TAPE WITH AN OFFSET
; V   VERIFY THAT A PROGRAM WAS PROPERLY LOADED
; V <OFFSET>  VERIFY PROGRAM LOADED WITH AN OFFSET
; D X,y DISPLAY MEMORY FROM X TO Y
; P X,y PUNCH CONTENTS OF MEMORY FROM X TO Y
; M X    MEMORY EXAMINE/MODIFY
;       <DATA> CHANGE 1 BYTE IN MEMORY TO <DATA>
;       LF  INCR POINTER, PR ADR AND VALUE OF NEW PNTR
;       SP  INCR PNTR, PR NEW VALUE ON SAME LINE
;       ,   INCR PNTR, NO PR OF ADR OR VALUE
;       UA  DECR PNTR, PR ADR AND VALUE AT PNTR
;       /   PR ADR AND VALUE OF CURRENT PNTR
;       CR  END MEMORY EXAMINE COMMAND
; X/  SAME AS M X, X MUST START W/ 0-9, MAY NEED LEADING 0
; /   PR ADR AND VALUE OF LOC LAST REF WITH MEM/EXAM
; O X Y CALCULATE RELATIVE OFFSET FOR BRANCH INSTR
; B   DISPLAY ALL BREAKPOINTS
; B - DELETE ALL BREAKPOINTS
; B X ENTER BREAKPOINT AT ADR X
; B -X DELETE BREAKPOINT AT ADR X
; G X EXECUTE USER PROG STARTING AT ADR X
; G   EXECUTE USER PROG STARTING AT CURRENT PC
; R  DISPLAY/CHANGE USER'S PROGRAM REGS
; .  TRACE 1 INSTRUCTION
; T X  TRACE X INSTRUCTIONS
; C EXECUTE USER'S CODE AS SUBR, RTS TO MONITOR
; C X XQT USER'S CODE AS SUBR START AT ADR X
; HI SET HIGH SPEED - 120 CPS FOR ON-CHIP IO
; HY SET HIGHER YET SPEED, FOR CRT - 9600 BD
; CONTROL X - TERMINATE D OR T PRINT
; CONTROL W - WAIT DURING D OR T PRT, ANY CHAR
;    CAUSES CONTINUATION OF PRINT

PIABD               equ       $8012               ; VDG PIA
ACIAS               equ       $8CF4
ACIAD               equ       $8CF5
CNTLW               equ       $17                 ; WAIT CHARACTER
CNTLX               equ       $18                 ; ABORT CHARACTER
; ON-CHIP CLOCK EQUATES
P2DDR               equ       $01                 ; PORT 2 DATA DIRECTION REG
CLOCK               equ       $09                 ; TIMER 1
TCSR                equ       $08                 ; TIMER CONTROL STATUS REG
OCREG               equ       $0B                 ; OUTPUT COMPARE REG

; ON-CHIP IO EQUATES

RMCR                equ       $010                ; RATE & MODE CONTROL
TRCS                equ       $011                ; TRANSMIT/RECEIVE CNTRL STAT REG
RECEV               equ       $012                ; READ REG
TRANS               equ       $013                ; TRANSMIT REG
                                                  ; MODE SELECT WORD
MODE                equ       $03                 ; UPPER 3 BITS = MODE
PTMADR              equ       $E000               ; DEFAULT ADDRESS FOR PTM

;*******************************************************************************
; RAM STORAGE
;*******************************************************************************

LOWRAM              equ       $80                 ; USED FOR STK OVFLOW CHK
                    #RAM
                    org       $CF
STACK               rmb       1                   ; STK PNTR WILL RUN UP TOWARD USER CODE

CT                  rmb       1                   ; INPUT CHAR CT
CKSUM               equ       CT,1                ; USED BY LOAD/VERF
STRTX               rmb       2                   ; INPUT CHAR PTR (ON SATCK)
NEXTX               rmb       2                   ; NEXT TABLE PTR
; CHRNL - UPPER 6 BITS-# NULLS AFTER CR
;         LOW 2 BITS-# NULLS AFTER CHAR
CHRNL               rmb       1                   ; NUM NULLS AFTER CHAR
BBLK                rmb       1                   ; BULK STORE BLK + NXT 4 LOC
PNTR                rmb       2                   ; OPEN ADR
TEMPA               rmb       2
TEMP                rmb       1                   ; TEMP AND OVFL MUST FOLLOW TEMPA
OVFL                rmb       1                   ; OVERFLOW FLAG
SAVSTK              rmb       2                   ; PC
                    rmb       2                   ; X
                    rmb       1                   ; A
                    rmb       1                   ; B
                    rmb       1                   ; CC
SPSAVE              rmb       2                   ; STK
NUMBP               equ       4                   ; NUMBER OF BREAKPOINTS
BKADR               rmb       NUMBP*2             ; BRKPNT ADDRESS
OPCODE              rmb       NUMBP
BRKFLG              rmb       1                   ; BRKPNT IN
NTRACE              rmb       2                   ; TRACE N INSTR
EXONE               rmb       1                   ; XQT 1 INSTR FLAG
OUTSW               rmb       1                   ; ECHO FLAG
CALLF               rmb       1                   ; FLAG FOR C CMD
PTM                 rmb       2                   ; PTM ADDRESS
FCTPTR              rmb       2                   ; POINTER TO FUNCTION TABLE
IOPTR               rmb       2                   ; IO TABLE POINTER
VECPTR              rmb       2                   ; VECTOR TABLE POINTER

;*******************************************************************************
                    #ROM
;*******************************************************************************

          #ifz VDG
                    org       $B00
          #else
                    org       $F800
          #endif

;*******************************************************************************
; JUMP TABLE TO SUBROUTINES
;*******************************************************************************

EX.NMI              !jmp      M.NMI               ; NMI VECTOR FOR PTM
IN.NMI              !jmp      C.NMI               ; NMI VECTOR FOR INTERNAL RESOURCES
INCHNP              !jmp      INCH1               ; INPUT 1 CHAR W/ NO PARITY
OUTCH               !jmp      OUTCH1              ; OUTPUT 1 CHAR W/PADDING
                    !jmp      PDATA1              ; PRINT DATA STRING
                    !jmp      PDATA               ; PR CR/LF, DATA STRING
                    !jmp      OUT2HS              ; PR 2 HEX + SP (X)
                    !jmp      OUT4HS              ; PR 4 HEX + SP (X)
                    !jmp      PCRLF               ; PRINT CR/LF
                    !jmp      SPACE               ; PRINT A SPACE
STRT                !jmp      Start               ; RESTART ADDRESS
IN.SWI              !jmp      M.SWI               ; SWI VECTOR

;***** FUNCTION JUMP TABLE *****
; BESIDES THIS INTERNAL COMMAND TABLE THERE MAY
;   BE AN EXTERNAL TABLE OF THE SAME FORMAT
;   'FCTPTR' POINTS TO THE TABLE TO
;   BE SEARCHED FIRST. WITH EXERNAL VECTORS,
;   THE USER CAN DEFINE THE RESET VECTOR
;   AND DO HIS OWN INITIALIZATION - DEFINE A
;   COMMAND TABLE, SET FCTPTR - BEFORE JUMPING
;   TO THE MONITOR INITIALIZATION.
;
; EACH ENTRY IN THE FUNCTION JUMP TABLE IS
;   ORGANIZED AS FOLLOWS:
;     FCB   XXX   XXX=TOTAL SIZE OF ENTRY
;     FCC   'STRING' WHERE STRING IS THE INPUT STRING
;     FDB   ADDR     WHERE ADDR IS THE ROUTINE ADDRESS
;
; THE LAST ENTRY IS:
;     -1  =  END OF EXTERNAL TABLE - GO SEARCH
;            INTERNAL TABLE.
;     -2  =  END OF TABLE(S)
;
; NOTE: AN EXTERNAL FUNCTION TABLE TERMINATED BY
;   -1, THE INTERNAL TABLE WILL ALSO BE SEARCHED.
;   IF TERMINATED BY -2, INTERNAL TABLE NOT CHECKED.

FCTABL              fcb       4
                    fcc       'B'
                    fdb       BRKPNT
                    fcb       4
                    fcc       'C'
                    fdb       CALL
                    fcb       4
                    fcc       'D'
                    fdb       DISPLAY
                    fcb       4
                    fcc       'G'
                    fdb       GOXQT
                    fcb       4
                    fcc       'L'
                    fdb       LOAD
                    fcb       4
                    fcc       'M'
                    fdb       MEMORY
                    fcb       4
                    fcc       'O'
                    fdb       OFFSET
                    fcb       4
                    fcc       'P'
                    fdb       PUNCH
                    fcb       4
                    fcc       'R'
                    fdb       REGSTR
                    fcb       5
                    fcc       'HI'
                    fdb       S120
                    fcb       5
                    fcc       'HY'
                    fdb       HY
                    fcb       4
                    fcc       'T'
                    fdb       TRACE
                    fcb       4
                    fcc       'V'
                    fdb       VERF
                    fcb       -2                  ; END OF TABLE

;**************** IO TABLE ***************
; ROUTINE IO IS CALLED WITH
; INDEX INTO IO TABLE CI OR INTO USER IO TABLE
; IOPTR POINTS TO THE IO TABLE TO BE USED
; THE INDEX TABLE DEFINES ORDER OF IO ROUTINES IN IO TABL

CI                  dw        CION,CIDTA,CIOFF
                    dw        COON,CODTA,COOFF
                    dw        HSON,HSDTA,HSOFF
                    dw        BSON,BSDTA,BSOFF

; THE FOLLOWING ARE INDICES INTO IO TABLE

                    #temp
CI.ON               next      :temp,2             ; INIT INPUT DEVICE
CI.DTA              next      :temp,2             ; INPUT A CHAR W/NO WAIT
CI.OFF              next      :temp,2             ; DISABLE INPUT DEVICE
CO.ON               next      :temp,2             ; INIT OUTPUT DEVICE
CO.DTA              next      :temp,2             ; OUTPUT A CHAR W/PADDING
CO.OFF              next      :temp,2             ; DISABLE OUTPUT DEVICE
HS.ON               next      :temp,2             ; INIT HIGH SPEED OUTPUT DEVICE
HS.DTA              next      :temp,2             ; OUTPUT BLOCK OF DATA
HS.OFF              next      :temp,2             ; DISABLE HIGH SPEED DEVICE
BS.ON               next      :temp,2             ; INIT TAPE DEVICE
BS.DTA              next      :temp,2             ; WRITE BLOCK OF DATA TO TAPE
BS.OFF              next      :temp,2             ; DISABLE TAPE DEVICE

;************** INCH **************
; CALL IO ROUTINE W/ INDEX TO INPUT DATA
; CLEARS PARITY
; IGNORES RUBOUT CHAR
; ECHOES OUTPUT IF FLAG CLEAR
; SAVE, RESTORE REG B

INCH1               proc
                    pshb
Loop@@              ldb       #CI.DTA             ; OFFSET TO CIDTA
                    bsr       IO                  ; SCAN IO DEVICE
                    bcc       Loop@@              ; LOOP ON NO WAIT INPUT
                    anda      #$7F                ; CLEAR PARITY
                    beq       Loop@@              ; IGNORE NULLS
                    cmpa      #$7F                ; RUBOUT?
                    beq       Loop@@
                    ldb       OUTSW               ; CHK IF ECHO
                    bne       Done@@
                    bsr       OUTCH1              ; ECHO INPUT
Done@@              pulb
                    rts

;*************** OUTCH ***************
; CALL IO ROUTINE W/ INDEX TO OUTPUT DATA
; SAVES, RESTORES REG B

OUTCH1              proc
                    pshb
                    ldb       #CO.DTA             ; PNTR TO OUTPUT A CHAR W/PADDING
                    bsr       IO
                    pulb
                    rts

;*************** CIDTA ***************
; READ 1 CHAR FROM INPUT W/ NO WAIT
; RETURN W/ C CLEAR IF NO READ
;     ELSE REG A = INPUT & C IS SET

CIDTA               proc
          #ifnz     CHIPIO
                    lda       ACIAS
                    asra
                    bcc       Done@@
                    lda       ACIAD               ; READ DATA
          #else
                    lda       TRCS                ; GET CONTROL WORD
                    asla                          ; CHK THAT RDRF IS SET
                    bcs       Read@@              ; READ DATA IF SET
                    asla                          ; LOOK AT ERR BIT
                    bcc       Done@@              ; RTN W/C CLR IF NO READ
          ; IF FRAMING ERR OR OVER RUN-READ
Read@@              lda       RECEV               ; READ
          #endif
          ; RETURN W/CARRY SET & lda  BITS SET
                    sec                           ; FLAG READ-NO WAIT ACOMPLISHD
Done@@              rts

;********** CODTA **********
; OUTPUT CHAR FROM REG A
; OUTC - SUBR CALLED BY CODTA
; EXPECT 30 OR 120 CPS
; DEFAULT SPEED = 30 CPS
; PADS CR AND CHAR FOR 120
; PAD 4 NULLS IF PUNCH CR

OUTC                proc
                    pshb
          #ifnz CHIPIO
Loop@@              ldb       ACIAS
                    asrb
                    asrb
                    bcc       Loop@@
                    sta       ACIAD               ; OUTPUT
          #else
Loop@@              ldb       TRCS                ; GET CONTRL WRD
                    bitb      #$20                ; TDRE SET?
                    beq       Loop@@              ; WAIT UNTIL IT IS
                    sta       TRANS
          #endif
                    pulb
                    rts

;*******************************************************************************

CODTA               bsr       OUTC                ; OUTPUT CHAR
                    ldb       OUTSW               ; GET TAPE FLAG
                    bne       N1
                    ldb       CHRNL               ; NOT TAPE
N1                  cmpa      #$D                 ; CR
                    beq       N3
                    cmpa      #$10                ; NO PADDING IF DLE
                    beq       Done@@
                    andb      #$3                 ; MASK OUT HIGH 6-BIT CNTR
                    bra       N4

N3                  lsrb:2                        ; REMOVE LOW 2-BIT CNTR
N4                  decb                          ; DECR NULL CNTR
                    bmi       Done@@              ; EXIT IF ENOUGH NULLS
                    psha
                    clra
                    bsr       OUTC                ; OUTPUT NULL
                    pula
                    bra       N4                  ; PR NXT NULL

Done@@              equ       :AnRTS

;*******************************************************************************
; INITIALIZE ON-CHIP SERIAL IO

CION                proc
                    ldd       #$1007              ; SET PADDING FOR 300
                    bsr       S1205               ; SET RMCR
                    lda       #$0A                ; SET TRCS FOR ON-CHP IO
                    sta       TRCS

          ; NO ACTION NEEDED BY THESE DEVICES

CIOFF               proc                          ; TURN CONSOLE IN OFF
HSON                proc                          ; TURN ON HIGH SPEED
HSOFF               proc                          ; TURN OFF HIGH SPEED
COOFF               proc
                    rts

;*******************************************************************************
; INITIALIZE OUTPUT DEVICE-SILENT 700 PRT
; TURN ON TI PRINTER

COON                proc
                    ldx       #PRTON              ; ACTIVATE ACD
                    jsr       PDATA1
;                   bra       DELAY

;*******************************************************************************
; ENTRY FROM BSOFF FOR DELAY AFTER TURN OFF PUNCH

DELAY               proc
                    ldx       #$411B              ; 100 MS DELAY
Loop@@              dex
                    bne       Loop@@
                    rts

;*******************************************************************************
;*************** IO ROUTINE ***************
; THIS ROUTINE USES INDEX TO RETRIEVE IO
; ROUTINE ADR FROM IO TABLE, THEN CALL AS SUBR
; REG B IS INDEX INTO IO TABLE
; TO DO IO, REG B IS SET, IO ROUTINE IS CALLED
; SAVES REG X

IO                  proc
                    pshx
                    ldx       IOPTR               ; ADR OF IO TABLE
                    abx                           ; ADD OFFSET
                    ldx       ,x                  ; GET IO ROUTINE ADR
                    jsr       ,x                  ; DO IO
                    pulx
                    rts

;*******************************************************************************
;************** HY / HI *************
; HY & HI SET CHRNL FLAG FOR PADDING
; LOW 2 BITS = NUM NULLS AFTER CHAR
; HIGH 6 BITS = NUM NULLS AFTER CR

;************** HI **************
; SET SPEED FOR 120 CPS
; SET # NULLS TO PAD CHAR
; SET BITS FOR 1200 BAUD IN RMCR

S120                proc
                    ldd       #$4F06
S1205               sta       CHRNL
                    stb       RMCR                ; SET BAUD RATE
                    rts

;*************** HY ***************
; HIGHER YET - 9600 BAUD ON CRT
; SET PADDING TO ZERO

HY                  ldd       #$0005              ; ALSO SET RMCR
                    bra       S1205

;********** RESET **********
; COME HERE FOR MONITOR RESTART
; INIT IO & FCN TABLE POINTERS
; TURN ON CONSOLE
; PRINT MONITOR NAME
; INIT RAM USED BY MONITOR
; MASK I BIT IN USER CC
; SET INITIAL SPEED
; INIT HARDWARE TRACE DEVICE

Start               proc
                    lds       #STACK              ; INIT STK PNTR
                    ldx       #CI                 ; INIT I/O PNTR TABLE
                    stx       IOPTR
                    ldx       #SERIAL             ; INIT VECTOR TABLE POINTER
                    stx       VECPTR
                    ldx       #FCTABL             ; INIT FUNCTION TABLE PTR
                    stx       FCTPTR
                    ldx       #PTMADR             ; SET ADR FOR PTM
                    stx       PTM
                    lds       #STACK-20           ; RESET INCASE USER DIDN'T
                    sts       SPSAVE              ; INIT USER STACK
                    lds       #STACK              ; RESET MONITOR STK
                    ldx       #BKADR              ; ZERO BKADR TO OVFL
ClrRam@@            clr       ,x
                    inx
                    cpx       #CALLF+1
                    bne       ClrRam@@
                    clrb                          ; OFFSET FOR CION
                    bsr       IO                  ; TURN ON CONSOLE IN
                    ldb       #CO.ON              ; OFFSET FOR COON
                    bsr       IO                  ; TURN ON CONSOLE OUTPUT
                    ldx       #LIL                ; PR LILBUG
                    jsr       PDATA               ; WITH CR/LF
                    lda       #$D0                ; MASK I IN CC
                    sta       SAVSTK+6

          ; INIT FOR HARDWARE TRACE -
          ;    CLOCK OR PTM

                    jsr       IFPTM
                    beq       INPTM               ; GO INIT PTM

          ; INIT ON-CHIP CLOCK

                    cpx       #IN.NMI             ; MAY NOT WANT ANY TRACE
                    bne       MAIN                ; IF NMI NOT SET-NO TRACE
                    inc       TCSR                ; SET OLVL BIT HI
                    bra       MAIN

;*******************************************************************************
; INIT PTM - SINGLE SHOT, 8 BIT
; USER MUST SET NMI VCTR FOR PTM TRACE
; MONITOR CHK IF VCTR SET

INPTM               proc
                    ldx       PTM                 ; GET PTM ADDRESS
                    clr       2,x                 ; SET LATCH FOR BRING OUT
                    clr       3,x                 ; OF RESET, MAKE G HI
                    ldd       #$0122
                    sta       1,x                 ; SET TO WRITE TO CR1
                    stb       ,x                  ; BRING OUT OF RESET
                    ldd       #$A600              ; SET SINGLE SHOT MODE
                    std       ,x                  ; ALSO SET NO WRITE TO CR1
;                   bra       MAIN

;*******************************************************************************
; PROMPT USER
; READ NEXT COMMAND

MAIN                proc
                    lds       #STACK
                    clr       OUTSW               ; MAKE SURE INPUT IS ECHOED
                    jsr       PCRLF               ; PRINT CR/LF
                    lda       #'!'
                    jsr       OUTCH
                    jsr       INPUTA              ; A-F ALPHA
                    bmi       MAIN                ; ABORT
                    beq       MAIN01

          ; HEX VALIDITY CHK

                    jsr       VALIN
                    bmi       MAIN                ; <ADR>/ VALID?
                    ldx       #MEM01              ; ENTER MEMORY ROUTINE
                    bra       MAIN08              ; SET UP FOR RTN

; A CONTAINS FIRST INPUT CHARACTER

MAIN01              ldx       #NEXT               ; CHK FOR TRACE 1
                    cmpa      #'.'                ; QUICK TRACE
                    beq       MAIN08
                    ldx       #MEMSL              ; CHK FOR /
                    cmpa      #'/'                ; QUICK MEM EXAMINE
                    beq       MAIN08

          ; READ IN STRING. PUSH STRING UNTO THE
          ;   STACK. MARK TOP OF STRING IN 'STRTX'

                    sts       STRTX               ; SAVE PTR TO INPUT STRING
                    clr       CT                  ; INPUT CHAR CT
MAIN03              bsr       TERM                ; CHECK FOR TERMINATORS
                    beq       SRCH                ; GOT ONE,GO DO COMPARES
                    inc       CT                  ; CT + 1 -> CT
                    psha                          ; SAVE INPUT CHAR ON STACK
                    tsx                           ; CHECK STACK POINTER
                    cpx       #LOWRAM
                    beq       MERROR              ; CHK IF END OF STK
                    bsr       INPUTA              ; GO GET NEXT CHAR
                    bmi       MAIN07              ; ESCAPE
                    bne       MERROR              ; NBRS ARE NOT ALLOWED
                    bra       MAIN03              ; LOOP

;*******************************************************************************
; HERE AFTER STRING WAS INPUT. CHECK IT AGAINST
;   STRINGS IN THE EXTERNAL AND/OR INTERNAL
;   FUNCTION TABLES. STRTX POINTS TO THE
;   INPUT STRING. FCTPTR POINTS TO THE START
;   OF THE FIRST TABLE TO SEARCH (EXTERNAL OR
;   INTERNAL).

SRCH                proc
                    sta       BBLK                ; LOCAL VAR - SAVE DELIMITER
                    ldx       FCTPTR              ; GET PTR TO TABLE
                    stx       NEXTX               ; SAVE IN TEMP
Loop@@              ldx       NEXTX               ; GET NEXT PTR INTO TABLE
                    pshx                          ; SAVE A COPY ON STACK
                    ldb       ,x                  ; GET ENTRY SIZE
                    abx                           ; CALCULATE ADDR OF NEXT ENTRY
                    stx       NEXTX               ; SAVE FOR NEXT SEARCH
                    subb      #3                  ; SUB OFF ADDR SIZE
                    cmpb      CT                  ; IS INPUT LENGTH=ENTRY LENGTH?
                    beq       SRCH03              ; YES,A POSSIBLE MATCH

          ; NO MATCH ON THIS ENTRY
          ; CHECK FOR TABLE TERMINATORS
          ; -1 = END OF EXTERNAL TABLE
          ; -2 = END OF TABLE(S)
          ; IF NOT -1 OR -2, NOT RECOGNIZE END OF TABLE
          ; B IS ALLREADY TERM-3

                    pulx                          ; CLEAN STACK
                    cmpb      #-4                 ; END OF EXTERNAL TABLE?
                    bne       SRCH02              ; NO

          ; SWITCH FROM EXT TO INT TABLE

                    ldx       #FCTABL             ; GET INNER TABLE
                    stx       NEXTX
SRCH02              cmpb      #-5                 ; END OF TABLE SEARCH?
                    bne       Loop@@              ; NO,KEEP TRUCKIN

          ; INPUT STRING NOT FOUND ! GO GRIPE
          ; HERE ON ERROR. PRINT ? AND
          ;   GO BACK TO MAIN START

MERROR              ldx       #QMARK
                    jsr       PDATA
MAIN07              bra       MAIN

; INPUT LENGTH=TABLE ENTRY LENGTH. TRY
;   FOR A MATCH. B=SIZE; (SP) = TABLE PTR

SRCH03              ldx       STRTX               ; INIT PTR TO INPUT STRING
                    stx       TEMPA
SRCH04              pulx                          ; RESTORE CURRENT TABLE PTR
                    inx
                    lda       ,x                  ; GET TABLE CHAR
                    pshx                          ; SAVE FOR NEXT LOOP
                    ldx       TEMPA               ; GET INPUT PTR
                    cmpa      ,x                  ; INPUT CHAR=TABLE CHAR?
                    beq       SRCH05              ; YES
                    pulx                          ; NO,CLEAN sta CK
                    bra       Loop@@              ; GET NEXT TABLE VALUE

; HERE WHEN A CHARACTER MATCHED

SRCH05              dex                           ; DEC INPUT PTR FOR NEXT TIME
                    stx       TEMPA
                    decb                          ; COMPARED ALL CHARS?
                    bne       SRCH04
;
; WE HAVE A MATCH! GO TO THE ROUTINE
;
                    pulx                          ; GET TABLE PTR
                    inx                           ; POINT TO ADDRESS IN TABLE
                    lds       STRTX               ; CLEAN STACK
                    ldx       ,x                  ; GET ROUTINE ADDRESS
                    lda       BBLK                ; LOAD TERMINATOR
MAIN08              jsr       ,x                  ; GO TO ROUTINE
                    bmi       MERROR              ; ERROR RETURN
                    bra       MAIN07              ; GO BACK TO MAIN

;*******************************************************************************
; CHECK INPUT CHAR FOR A TERMINATOR
;   TERMINATORS ARE: , BLANK <CR>
;   CHAR IN A ON CALL
;   Z BIT SET ON EXIT IFF CHAR WAS
;   TERMINATOR

TERM                proc
                    cmpa      #','                ; COMMA?
                    beq       Done@@
                    cmpa      #' '                ; BLANK?
                    beq       Done@@
                    cmpa      #$D                 ; CR?
                    beq       Done@@
                    cmpa      #'-'                ; ALLOW MINUS
Done@@              rts                           ; RETURN WITH Z BIT

;*******************************************************************************
; VALIDATE INPUT - ENTRY VALINP READS INPUT
; ALLOW 4 DIGIT INPUT W/LEADING 0'S NOT COUNT
; SET CC NEG IF ERROR

VALINP              proc
                    bsr       INPUT               ; READ HEX
;                   bra       VALIN

;*******************************************************************************

VALIN               proc
                    ble       Done@@
                    cmpb      #4
                    ble       INPUTC
                    tst       OVFL                ; LEADING ZEROES?
                    beq       INPUTC
                    comb                          ; SET C NEG FOR ERR RTN
Done@@              rts

;*******************************************************************************
;*****INPUT - READ ROUTINE
; INPUT ENTRY SET B=0, READ A-F AS HEX
; INPUTA ENTRY SET B#0, READ A-F AS ALPHA
; X= HEX NUMBER (ALSO IN TEMPA)
; A=LAST CHAR READ (NON-HEX)
; B= # HEX CHAR READ (TEMP)
; OVFL # 0 IF OVERFLOW FROM LEFT SHIFT
; CC SET FROM ldb  BEFORE RETRN
; CC SET NEG IF ABORT

INPUTA              proc
                    ldb       #$F0                ; READ A-F AS ALPHA
                    bra       INPUT2

INPUT               clrb                          ; READ A-F AS HEX
INPUT2              ldx       #0                  ; INIT VAR TO 0
                    stx       TEMPA
                    stx       TEMP                ; 0 TTEMP, OVFL
                    ldx       #TEMPA              ; X PNT TO WH INPUT CHR STORED
INPUT3              bsr       INHEX               ; READ A CHAR
                    bmi       INPUT7              ; JMP IF NOT HEX
                    ldb       #4
INPUT5              asl       1,x
                    rol       ,x
                    bcc       INPUT6              ; SET FLAG IF OVERFLOW
                    inc       OVFL
INPUT6              decb                          ; LEFT SHIFT 4 BITS
                    bne       INPUT5
                    oraa      1,x                 ; ADD IN LSB
                    sta       1,x
                    inc       TEMP
                    bra       INPUT3

INPUT7              cmpa      #CNTLX              ; CHK IF ABORT
                    bne       INPUT9              ; SKIP IF NOT ABORT
NOTHEX              equ       *                   ; ERROR ENTRY FROM INHEX
                    ldb       #$FF                ; SET CC NEG
                    rts

INPUT9              ldx       TEMPA               ; SET REG X=# READ
INPUTC              ldb       TEMP                ; SET REG B=# HEX CHAR READ
                    rts

;*******************************************************************************
; INPUT 1 HEX CHAR, CONVERT TO HEX
; RETURN HEX IN REG A
; REG B = 0 CONVERT A-F TO HEX
; REG B < 0 LEAVE A-F ALPHA

INHEX               proc
                    jsr       INCHNP              ; (INHEX) MUST BE NEG
                    cmpa      #'0'
                    bmi       NOTHEX              ; NOT HEX
                    cmpa      #'9'
                    ble       Done@@              ; GOOD
                    tstb      A-F                 ; NUMBERS?
                    bmi       NOTHEX              ; NO
                    cmpa      #'A'
                    bmi       NOTHEX              ; NOT HEX
                    cmpa      #'F'
                    bgt       NOTHEX              ; NOT HEX
                    suba      #7
Done@@              anda      #$F
                    clrb                          ; AFTER FIND 0-9 CLEARR
                    rts                           ; GOOD HEX - RTN

;************* MEMORY EXAMINE/CHANGE ***************
; PRINT VALUE AT <ADR>, MAINTAIN PNTR
; M <ADR>(SPACE)
; <ADR>/
; <ADR> IS 1-4 HEX, NOT COUNTING LEADING ZEROES
; SUBCOMMANDS
;      <DATA> MODIFY VALUE AT CURRENT LOC
;      SP     INCR POINTER, PR VALUE AT NEXT ADR
;      ,      INCR PNTR, NO PRINT
;      LF     INCR PNTR, PR ADR & VALUE ON NEXT LINE
;      UA     DECR PNTR, PR ADR & VALUE ON NEXT LINE
;      /      PR CURRENT ADR AND VALUE
;      CR     TERMINATE MEM/EXAM COMMAND

MEMORY              proc
                    bsr       VALINP
                    ble       MERRTN              ; NOT HEX - ERROR
MEM01               ldx       TEMPA               ; RRESET FOR ADR/
                    cmpa      #'/'                ; / DELIMITER?
                    beq       MEM02
                    cmpa      #$20                ; SPACE?
                    bne       MERRTN
MEM02               bsr       OUT2H               ; PRINT VALUE
MEM25               stx       PNTR
                    pshx
                    clrb                          ; A-F NUMBER FLAG
                    bsr       INPUT               ; X=ADR
                    pulx
                    bmi       RETRN               ; IF NEG - ABORT
                    beq       MEM03               ; JUMP IF NOT HEX
                    ldb       TEMPA+1             ; GET LAST BYTE
                    bsr       STRCHK              ; STORE B AND CHK FOR CHG MEM
                    bmi       RETRN               ; ERR IN CHG MEMORY
MEM03               cmpa      #$D                 ; CR?
                    beq       RETRN               ; END MEM/EX?
                                                  ; X = ADR OF CURRENT BYTE
                    cmpa      #','                ; COMMA?
                    bne       MEM33
                    inx                           ; OPEN NEXT LOC, DO NOT PR
                    bra       MEM25

MEM33               cmpa      #$20                ; SPACE?
                    bne       MEM04
                    inx                           ; INCR PNTR
                    bra       MEM02               ; GO PR VALUE

MEM04               cmpa      #$A                 ; LF?
                    bne       MEM06
                    inx
                    bsr       PCR                 ; OUT CR, NO LF
                    bra       MEM12               ; PR ADDR,SPACE

MEM06               cmpa      #$5E                ; UA?
                    bne       MEM08
                    dex
                    bra       MEM10

MEM08               cmpa      #'/'                ; SLASH?
                    bne       MERRTN
MEM10               bsr       PCRLF               ; PR CR/LF
MEM12               stx       PNTR                ; SAVE NEW PNTR ADR
MEMSL               equ       *                   ; FOUND / AS INSTR
                    ldx       #PNTR               ; X PNT TO PR OBJECT
                    bsr       OUT4HS              ; ADR,SP
                    ldx       PNTR                ; RESET X TO PNTR
                    bra       MEM02

MERRTN              lda       #$FF                ; SET CC NEG FOR RTN
RETRN               rts

;*******************************************************************************
; *O <ADR> CALCULATES OFFSET FROM LAST MEMORY REF
; *WHICH SHOULD BE LOC OF REL ADR OF BR INSTR, TO
; *THE <ADR> SPECIFIED
; IF A=0, B<80 DISTANCE CHK
; IF A=FF, B>7F

OFFSET              proc
                    jsr       RD2ADR              ; READ 2 ADDR
                    ldd       TEMPA
                    subd      #1
                    subd      PNTR                ; OFFSET=TO-(FROM+1)
                    cmpb      #$7F                ; CHK IF VALID DISTANCE
                    bhi       OFF4
                    tsta                          ; POSITIVE DISTANCE?
                    beq       OFF6
                    bra       MERRTN

OFF4                cmpa      #$FF                ; NEG DISTANCE
                    bne       MERRTN
OFF6                stb       TEMP                ; PR OFFSET
                    bsr       PCRLF               ; PR LF AFTER USER CR
                    ldx       #TEMP
                    bsr       OUT2HS
                    bsr       PCRLF
                    bra       MEMSL               ; GO TO / ROUTINE

;*******************************************************************************
; PRINT 2 BYTES AND SPACE
; REG X - ADR OF 1ST BYTE
; X WILL BE INCREMENTED BY 1

OUT4HS              proc
                    bsr       OUT2H
                    inx                           ; GET NEXT BYTE
;                   bra       OUT2HS

;*******************************************************************************
; PRINT 1 BYTE AND SPACE
; REG X - ADR OF BYTE

OUT2HS              proc
                    bsr       OUT2H               ; 1 BYTE
;                   bra       SPACE

;*******************************************************************************

SPACE               proc
                    lda       #' '                ; PR SPACE
                    bra       XOUTCH              ; PR 1 CHAR & RTN

;*******************************************************************************
; PRINT 1 BYTE
; REG X - ADR OF BYTE

OUT2H               proc
                    lda       ,x
                    psha                          ; READ BYTE ONLY ONCE
                    bsr       OUTHL
                    pula
                    bra       OUTHR               ; RIGHT

;*******************************************************************************
; CONVERT LEFT 4 BITS OF BYTE TO DISPLAY

OUTHL               proc
                    lsra:4                        ; OUTPUT LEFT 4 BINARY BITTS
;                   bra       OUTHR

;*******************************************************************************
; CONVERT RIGHT 4 BITS OF BYTE AND PRINT

OUTHR               proc
                    anda      #$F                 ; OUTPUT RIGHT 4 BITS
                    adda      #$90                ; CONVERT TO DISPLAY
                    daa
                    adca      #$40
                    daa
                    bra       XOUTCH              ; PR 1 CHAR & RTN

;*******************************************************************************
; STORE B AT ,x & VERIFY STORE *****
; DETECTS NON-EXISTENT MEMORY, ROM, PROTECTED RAM

STRCHK              proc
                    stb       ,x                  ; STORE B
                    cmpb      ,x                  ; VERIFY MEMORY CHG
                    beq       RETRN               ; OK
                    ldx       #NOCHG              ; MSG
                    bsr       PDATA
                    bra       MERRTN              ; SET CC NEG

;*******************************************************************************
; PRINT DATA STRING
; REG X POINTS TO PRINT ARRAY
; X WILL BE INCREMENTED

Loop@@              proc
                    bsr       XOUTCH              ; CALL OUTPUT ROUTINE
                    inx                           ; X=ADR OF OUTPUT ARRAY
PDATA1              lda       ,x                  ; GET CHAR
                    cmpa      #4                  ; EOT?
                    bne       Loop@@
                    rts

;*******************************************************************************
; CR/LF THEN PRINT DATA STRING

PDATA               proc
                    bsr       PCRLF               ; CR/LF, DATA STRING
                    bra       PDATA1

;*******************************************************************************
; OUTPUT CR/LF
; SAVE, RESTORE REG X

PCRLF               proc
                    lda       #$A                 ; OUTPUT LF
                    bsr       XOUTCH              ; PR & RTN

PCR                 lda       #$D                 ; DO CR
                    bsr       XOUTCH              ; PR & RTN
                    clra
XOUTCH              jmp       OUTCH               ; OUTPUT & RTN

;*******************************************************************************
; PR REGISTERS ACROSS PAGE
; PR 2ND LINE REG, READING INPUT
;     SPACE - PR CONTENTS REG, GO TO NEXT REG
;     HEX,SP - MODIFY REG, GO TO NEXT REG
;     HEX,CR - MODIFY REG, RTN
;     CR OR OTHER COMBINATION - NO CHG, RTN

REGSTR              proc
                    bsr       PREGS1
                    bsr       PCRLF               ; CR/LF AFTER REG PRINT
REGS1               ldx       #SAVSTK             ; PSEUDO REGS
                    clrb                          ; INIT OFFSET
REGS2               pshx                          ; SAVE REG PNTR
                    ldx       #ARRAY              ; CONTAINS REG NAMES
                    abx                           ; ADD OFFSET
                    lda       ,x                  ; GET CURRENT REG
                    bsr       OUTDA               ; PR REG NAME, DASH
                    lda       1,x                 ; #BYTES FLAG
                    pulx                          ; REG PNTR
                    tst       CT                  ; PRINT OR MOD?
                    beq       REGS3               ; MODIFY
                    tsta                          ; CHK # BYTES
                    beq       REGS4
                    bsr       OUT2H               ; PR 2 HEX DIGITS
                    inx
REGS4               bsr       OUT2HS              ; PR 2 HEX + SP_
                    inx
                    bra       REGS6

REGS3               pshb                          ; SAVE OFFSET
                    bsr       INDAT               ; GO READ INPUT
                    pulb                          ; RETRIEVE OFFSET
REGS6               addb      #2                  ; UPDATE
                    cmpb      #12                 ; ALL REG CHKED
                    bne       REGS2               ; NO - LOOP
                    rts

;*******************************************************************************
; INPUT FOR REG MODIFICATION

INDAT               proc
                    psha                          ; SAVE LEN FLG
                    pshx                          ; REG PNTR ADR
                    jsr       INPUT
                    pulx                          ; RESTORE
                    pulb
                    bmi       PRERR               ; ABORT
                    beq       INDAT2              ; NOT HEX
                    jsr       TERM                ; ACCEPT SP , CR
                    bne       PRERR               ; RTN TO MAIN
                    tstb      CHK                 ; LENGTH FLAG
                    beq       INDAT0
                    psha                          ; SAVE LAST CHAR READ
                    ldd       TEMPA               ; GET 2 BYTE READ IN
                    std       ,x
                    pula                          ; RESTORE LAST CHAR
                    inx                           ; INCR REG PNTR
                    bra       INDAT5

INDAT0              ldb       TEMPA+1             ; 1 BYTE CHANGE
                    stb       ,x
INDAT5              cmpa      #$D                 ; CR - RTN
                    bne       INDAT1
PRERR               pulx                          ; POP RTN ADR
                    pulb                          ; REMOVE FLAG FROM STK
                    clra                          ; NO BELL ON RETURN
                    rts                           ; RTN TO MAIN

INDAT2              cmpa      #$20                ; NO HEX, SPACE
                    bne       PRERR               ; RTN TO MAIN
                    tstb      2                   ; OR 4 CHAR
                    bne       INDAT4
                    jsr       OUT2HS              ; PR 2 CHAR,SPACE
                    bra       INDAT1

INDAT4              jsr       OUT4HS              ; PR 4 CHAR, SPACE
INDAT1              inx                           ; ADJUST REG PNTR
                    rts

;*******************************************************************************
; PRINT REGS - P,x,A,B,C,S

PREGS1              proc
                    bsr       PCRLF
PREGS               inc       CT                  ; SET FLAG-PRT REG
                    bsr       REGS1               ; GO PRINT
                    clr       CT                  ; RESET FLAG
                    rts

;*******************************************************************************
; PRINT REG A, -

OUTDA               proc
                    bsr       ZOUTCH              ; OUTPUT REG A
                    lda       #'-'                ; DASH
ZOUTCH              jmp       OUTCH

;*******************************************************************************
; COME HERE AFTER RECOGNIZE B<DELIM>
; B    DISPLAY ALL
; B -  REMOVE ALL
; B <ADR> INSERT BRKPNT
; B -<ADR> REMOVE BRKPNT

BRKPNT              proc
                    cmpa      #$D                 ; CR?
                    beq       PRBRK               ; PRINT
                    cmpa      #'-'                ; DELETE?
                    beq       DELBRK
                    jsr       VALINP
                    bmi       GOX2                ; ABORT?
                    bne       BP02                ; HEX?
                    cmpa      #'-'                ; DELETE
                    beq       DELBRK
                    bra       GOX2                ; ERR IF NOT DEL

BP02                cmpa      #$D                 ; CR
                    bne       BERRTN              ; ERROR RTN
                    bsr       BRKTAB              ; IN TABL
                    beq       PRBRK               ; YES - OK RTN
                    ldx       #BKADR
BP04                ldd       ,x
                    beq       BP06                ; AVAIL SP?
                    inx                           ; CHK NEXT POSN
                    inx
                    cpx       #OPCODE             ; END TABL?
                    bne       BP04
                    bra       BERRTN              ; NO AVAIL SP

BP06                ldd       TEMPA               ; GET ADR
                    std       ,x                  ; STORE IN TABLE
;                   bra       PRBRK

;*******************************************************************************
; PRINT BREAKPOINTS

PRBRK               proc
                    jsr       PCRLF
                    ldx       #BKADR
                    ldb       #4
Loop@@              jsr       OUT4HS
                    inx                           ; INCR PNTR TO BRKPNTS
                    decb
                    bne       Loop@@
                    rts

;*******************************************************************************
; SEARCH BREAKPOINT TABLE
; RETURN -1 IF BRKPNT NOT IN TABL
; OTHERWISE REG X POINT TO BRKPNT IN TABL

BRKTAB              proc
                    ldx       #BKADR
Loop@@              ldd       TEMPA               ; GET PC
                    subd      ,x
                    beq       Done@@
                    inx:2
                    cpx       #OPCODE             ; CMPAR TO END TABLE
                    bne       Loop@@
GOX2                equ       *                   ; ERROR RETURN ENTRY FROM G
BERRTN              lda       #$FF
Done@@              rts

;*******************************************************************************
; DELETE BRKPNT

DELBRK              proc
                    jsr       VALINP
                    bmi       BERRTN              ; ABORT OR ERR?
                    cmpa      #$D                 ; CR?
                    bne       BERRTN
                    tstb      HEX?
                    bne       Del@@               ; JMP IF SO
                    ldx       #BKADR-1
                    ldb       #12                 ; 0 BRKPNT TABLE
Loop@@              inx
                    clr       ,x
                    decb
                    bne       Loop@@
                    bra       PRBRK

          ; DELETE 1 BRKPNT

Del@@               bsr       BRKTAB
                    bne       BERRTN
                    std       ,x                  ; D=0 FROM BRKTAB
                    clr       8,x                 ; CLR OP CODE
                    bra       PRBRK

;*******************************************************************************
; CALL USER ROUTINE AS SUBR
; USER RTS RETURNS TO MONITOR
; STK PNTR NOT GOOD ON RETURN
; C <ADR> (CR) OR C (CR)

CALL                proc
                    sta       CALLF               ; SET FLAG # 0
;                   bra       GOXQT

;*******************************************************************************
; GO EXECUTE USER CODE
; G(CR) OR G <ADR>

GOXQT               proc
                    cmpa      #$D                 ; CR
                    beq       GOX6                ; XQT FROM CURRENT PC
                    jsr       VALINP
                    ble       GOX2
                    cmpa      #$D                 ; CR?
                    bne       GOX2                ; ERR
                    clr       EXONE               ; SEE BRKPNT, IF ANY
                    stx       SAVSTK              ; SET USER PC
GOX6                jsr       PCRLF
                    lda       CALLF               ; CALL CMD?
                    beq       GOX7                ; NO
                    clr       CALLF
                    ldx       SPSAVE              ; GET USER STK
                    ldd       #CRTS               ; RTN TO MONITOR ADR
                    dex
                    std       ,x                  ; STOR ON USER STK
                    dex                           ; ADJUST USER STK
                    stx       SPSAVE              ; RESAVE STK

          ; NOW GO XQT USER SUBR

GOX7                lda       EXONE               ; STOPPED ON BRKPNT
                    bne       GOX8
                    jsr       SETB
GOX8                bra       BARMS

;*******************************************************************************
; . (PERIOD)
; TRACE 1 INSTRUCTION

NEXT                proc
                    ldx       #1
                    bra       TRACE2

;*******************************************************************************
; T <HEX> - TRACE <HEX> INSTTR

TRACE               proc
                    cmpa      #$D                 ; T(CR) ? - TRACE 1
                    beq       NEXT
                    jsr       INPUT               ; GET <HEX>
                    ble       GOX2                ; RTN IF ABORT OR NOT HEX
TRACE2              stx       NTRACE              ; STORE <HEX>
                    beq       GOX2                ; RTN IF TRACE = 0
                    inc       EXONE               ; XQT 1 INSTR
BARMS               bra       ARMSTK

;*******************************************************************************
; ENTRY AFTER C COMMAND, AFTER XQT USER RTS
; SAVE USER REGISTERS
; PRINT REGISTERS
; RETURN TO ROUTINE CALLING C COMMAND ROUTINE

CRTS                proc
                    psha                          ; SAVE TO GET CC
                    tpa
                    sta       SAVSTK+6            ; CC
                    pula
                    sts       SPSAVE              ; STK PNTR
                    lds       #STACK
                    std       SAVSTK+4            ; A,B
                    stx       SAVSTK+2            ; X
                    ldx       #CRTS               ; PC PNT TO MONITOR
                    stx       SAVSTK
                    jsr       RBRK                ; REMOVE BRKPNTS
                    jmp       ENDCAL              ; GO PR REGS, 0 EXONE

;*******************************************************************************
; SETCLK - USED BY ON-CHIP CLOCK
; FOR HARDWARE TRACE
; SET TIMER TO COMPARE AFTER 1 CYCLE OF USER INSTR

SETCLK              proc
                    ldb       #$18                ; SET #CYCLES
                    ldx       CLOCK               ; GET CLOCK TIME
                    abx                           ; ADD # CYCLES
                    stx       OCREG               ; STORE IN COMPARE REG
                    rts

;*******************************************************************************
; ENTER FROM XQT 1 INSTR - TRACE OR XQT OVER BRKPNT
; MOVE REGS FROM USER STK TO MONITOR STORAGE
; REPLACE BRKPNTS WITH USER CODE
; IF NOT TRACING, REPLACE CODE WITH BRKPNTS (3F)
; IF TRACING, PRINT REGISTERS
;             EXECUTE NEXT USER INSTR
; ENTRY FOR ONCHIP CLOCK TRACE

C.NMI               proc
                    inc       TCSR                ; BRING LEVEL HIGH
                    bsr       SETCLK              ; NO NMI, BUT LEVEL CHG
;                   bra       M.NMI

;*******************************************************************************
; ENTRY FOR PTM HARDWARE TRACE

M.NMI               proc
                    tsx                           ; TRANSFER STK PNTR
                    lds       #STACK
                    bsr       MOVSTK              ; SAVE USER REGS
                    jsr       RBRK                ; REMOVE BRKPNT
                    ldx       NTRACE              ; TRACE?
                    bne       NMI01
                    clr       EXONE
                    jsr       SETB
                    bmi       NMI03
                    bra       ARMSTK

NMI01               dex
NMI015              stx       NTRACE
                    bne       NMI02
                    clr       EXONE
; PRINT TRACE LINE:
; OP-XX P-XXXX X-XXXX A-XX B-XX C-XX S-XXXX
; CHECK IF USER HIT CONTROL X TO TERMINATE TRACE
NMI02               ldx       #0                  ; CLR TRACE & EXONE IF TERMINATE
                    jsr       CHKABT
                    beq       NMI015              ; TERMINT IF = CNTL X
                    ldx       #PRTOP              ; GET ADR OF OP-
                    jsr       PDATA
                    ldx       TEMPA               ; GET OLD PC
                    jsr       OUT2HS              ; PR OPCODE
                    jsr       PREGS               ; PR TRACE LINE
                    lda       EXONE
                    bne       ARMSTK
NMI03               jmp       MAIN

;*******************************************************************************
; STACK USER REGISTERS
; MOVE FROM MONITOR STORAGE TO USER STACK
; IF TRACE - SET HARDWARE

ARMSTK              proc
                    lds       SPSAVE              ; SET STK FOR RTI
                    ldx       SAVSTK              ; PC
                    pshx
                    ldx       SAVSTK+2            ; X
                    pshx
                    ldd       SAVSTK+4            ; GET A, B
                    psha                          ; MOVE TO STK
                    pshb
                    lda       SAVSTK+6            ; GET CC
                    psha
                    lda       EXONE
                    beq       Done@@
                    ldx       SAVSTK              ; SAVE PC PNTR FOR NXT TRACE PRT
                    stx       TEMPA
          ; CHECK IF USE PTM OR ON-CHIP CLOCK
                    bsr       IFPTM
                    beq       SETPTM              ; GO USE PTM
          ; IF USER ISSUE TRACE COMMAND AND
          ; NOT USING PTM - ASSUME ON-CHIP
                    lda       #2                  ; SET DDR FOR OUTPUT
                    sta       P2DDR               ; PORT 2
                    ldb       TCSR                ; SET UP FOR ON-CHIP CLOCK
                    andb      #$FE                ; CLEAR OLVL BIT
                    stb       TCSR
                    bsr       SETCLK              ; SET CMPR REG, WAIT FOR CMPR
DUMMY               equ       *                   ; INTERRUPT VECTORS USE THIS
Done@@              rti

;*******************************************************************************
; SET HARDWARE FOR PTM
; INITIATE COUNTER

SETPTM              proc
                    ldd       #$0501              ; M=5,L=1 TURN ON TRACE
                    ldx       PTM                 ; GET ADR OF PTM
                    std       2,x                 ; STORE AT PTM ADR +2
                    rti

;*******************************************************************************
; CHECK NMI VECTOR
; DETERMINE IF USE ON-CHIP CLOCK OR PTM
;    FOR HARDWARE TRACE

IFPTM               proc
                    ldx       #VECTR              ; GET ADR OF VECTORS
                    lda       MODE                ; EXTERNAL VECTRS?
                    anda      #$E0                ; CHK 3 MSB
                    cmpa      #$20                ; MODE 1?
                    beq       Go@@
                    ldx       VECPTR              ; GET VECTOR TABLE
Go@@                ldx       $C,x                ; GET NMI ADDRESS
                    cpx       #EX.NMI             ; PTM ENTRY?
                    rts                           ; RETURN WITH CC SET

;*******************************************************************************
; MOVE USER REGS FROM USER STACK TO MONITOR STORAGE
; RESET USER STACK POINTER

MOVSTK              proc
                    lda       ,x                  ; MOVE C,B,A,x,PC
                    sta       SAVSTK+6            ; TO PC,x,A,B,C
                    ldd       1,x
                    sta       SAVSTK+5
                    stb       SAVSTK+4
                    ldd       3,x
                    std       SAVSTK+2
                    ldd       5,x
                    std       SAVSTK
                    ldb       #6
                    abx
                    stx       SPSAVE
                    rts

;*******************************************************************************
; REPLACE BRKPNTS (SWI) WITH USER CODE
; BKADR - TABLE OF 4 BRKPNT ADR
; OPCODE - TABLE OF OPCODES, CORRESPOND TO ADR

RBRK                proc
                    lda       BRKFLG              ; IGNORE IF BRKPNTS NOT IN
                    beq       Done@@
                    ldx       #BKADR              ; GET TABLE OF ADR
                    ldb       #NUMBP*2            ; INDEX INTO OPCODE TABLE
Loop@@              pshx:2                        ; SAVE TABLE ADR
                    abx
                    lda       ,x                  ; GET OPCODE
                    pulx
                    ldx       ,x                  ; GET USER BRKPNT ADR
                    beq       Cont@@              ; NO ADR
                    sta       ,x                  ; RESTORE OPCODE
Cont@@              pulx                          ; GET NXTT ADR FROM TABL
                    inx:2
                    decb                          ; ADJUST OPCODE INDEX
                    cmpb      #NUMBP              ; END TABLE?
                    bne       Loop@@
                    clr       BRKFLG              ; CLR BRKPNT FLAG
Done@@              rts

;*******************************************************************************
; REPLACE USER CODE WITH 3F AT BRKPNT ADDRESSES
; IGNORE IF BREAKPOINTS ALREADY IN

SETB                proc
                    lda       BRKFLG              ; ALREADY IN?
                    bne       SHERR               ; SET NEG RETURN
                    ldx       #BKADR
                    ldb       #NUMBP*2            ; SET INDEX INTO OPCODES
Loop@@              pshx                          ; SAVE ADR PNTR
                    ldx       ,x                  ; GET USER BRKPNT ADR
                    beq       Cont@@              ; SKIP IF NO ADR
                    lda       ,x                  ; GET OPCODE
                    pshb                          ; SAVE OPCODE INDEX
                    ldb       #$3F                ; SET SWI
                    jsr       STRCHK              ; STORE & CHK CHG
                    pulb                          ; INDEX
                    pulx                          ; ADR TABLE PNTR
                    bmi       Done@@              ; 3F STORED GOOD?
                    pshx                          ; RESAVE TABLE PNTR
                    abx                           ; CALCLATE OP POS IN TABLE
                    sta       ,x                  ; SAVE OPCODE
Cont@@              pulx                          ; GET TABLE ADR
                    inx:2                         ; GET NXT ADT
                    decb                          ; ADJUST OPCODE INDEX
                    cmpb      #NUMBP              ; END TABLE?
                    bne       Loop@@              ; LOOP IF NOT
                    stb       BRKFLG              ; SET BRKPNT FLAG
Done@@              rts

;*******************************************************************************
; SWI ENTRY
; ENTER WITH BRKPOINT SETTING
; SAVE USER REGISTERS
; DECR PC TO POINT AT SWI
; REPLACE SWI'S WITH USER CODE
; PRINT REGISTERS
; GO TO MAIN CONTROL LOOP

M.SWI               proc
                    tsx                           ; GET USER STK
                    lds       #STACK              ; SET TO INTERNAL STK
                    bsr       MOVSTK              ; SAVE USER REGS
                    ldx       SAVSTK              ; DECR USER PC
                    dex
                    stx       SAVSTK
                    stx       TEMPA               ; SAVE FOR BRKTAB CHK
                    lda       BRKFLG              ; ERR IF NOT BRKPOINT
                    beq       BKPERR
                    bsr       RBRK                ; REMOVE BRKPNT FROM CODE
                    jsr       BRKTAB              ; BRKPNT IN TABLE?
                    bne       BKPERR
          ; REG A = 0 IF BRKTAB FIND BRKPNT
                    inca
                    bra       SWI3

;*******************************************************************************
; ENTRY FROM CRTS - PR REGS, RTN TO MAIN

ENDCAL              proc
BKPERR              clrd
                    std       NTRACE              ; RESET NUM INSTR TO TRACE
SWI3                sta       EXONE               ; CLEAR XQT 1 INSTR
                    jsr       PREGS1
                    jmp       MAIN                ; GO TO MAIN LOOP

;*******************************************************************************
; D   OR D <ADR>  OR D <ADR> <ADR>
; DISPLAY MEMORY - BLK OF MEMORY AROUND LAST
;   REFERENCED BYTE FROM MEM/EX
; DISPLAY 16 BYTES AROUND <ADR> SPECIFIED
; OR DISPLAY FROM <ADR> TO <ADR> MOD 16
; ASCII CHAR WILL BE PRINTED ON THE RIGHT
; MEM/EX PNTR WILL PNT TO LAST ADR REFERENCED
; AT END OF DISPLAY COMMAND

DISPLAY             proc
                    ldx       PNTR                ; SAVE MEMORY/EX PNTR
                    pshx
                    cmpa      #$D                 ; CR?
                    beq       SHOW35              ; NO ARG
                    bsr       PVALIN
                    ble       SHERR2              ; ERR IF NOT HEX, OR ABORT
                    stx       PNTR
                    cmpa      #$D                 ; CR?
                    bne       SHOW4
SHOW35              ldd       PNTR                ; DEFINE BLK TO DMP
                    andb      #$F0                ; MASK OUT LOW DIGIT
                    subd      #$10
                    std       PNTR
                    addd      #$20
                    std       TEMPA               ; TO ADR
                    bra       SHOW8

SHERR2              pulx                          ; RESET MEM/EX PNTR
                    stx       PNTR
SHERR               lda       #$FF
                    rts

SHOW4               bsr       PVALIN              ; READ HEX #
                    ble       SHERR2              ; JMP IF ERR
                    ldd       PNTR                ; FROM ADR < TO ADR?
                    andb      #$F0                ; MASK OUT LOW ORDER DIGIT
                    stb       PNTR+1
                    subd      TEMPA
                    bhi       SHERR2
                    lda       TEMPA+1             ; MASK TO FULL LINE
                    anda      #$F0
                    sta       TEMPA+1             ; CHANGES LAST REF ADR

          ; TURN ON HIGH SPEED DEVICE
          ; CALL HIGH SPEED DATA ROUTINE TO OUTPUT
          ;    DATA FROM ADR IN PNTR TO ADR IN TEMPA

SHOW8               ldb       #HS.ON
                    jsr       IO2
                    ldx       #BBLK+1             ; GET TRANSFER PACKET
                    ldb       #HS.DTA
                    jsr       IO
                    pulx                          ; RETRIEVE MEM/EX PNTR
                    stx       PNTR
                    ldb       #HS.OFF
                    bsr       IO2
                    clra                          ; CLEAR CC FOR RETURN
                    rts

;*******************************************************************************
; READ WITH NO WAIT
; CHK FOR CONTROL X - ESCAPE FROM PRINT
; CHK FOR CONTROL W - WAIT DURING T OR D PRINT
;    ANY CHARACTER CONTINUES PRINT
; ANY OTHER CHARACTER - READ & IGNORE

CHKABT              proc
                    psha
                    ldb       #CI.DTA             ; READ A CHAR
                    bsr       IO2
                    anda      #$7F                ; CLEAR PARITY
                    cmpa      #CNTLW              ; CONTROL W?
                    bne       CtrlX?@@            ; IF SO WAIT FOR INPUT
                    jsr       INCHNP              ; TO CONTINUE PRINT
CtrlX?@@            cmpa      #CNTLX              ; CONTROL X?
          ; RETURN WITH CC SET
                    pula
SHOW19              rts

PVALIN              jmp       VALINP              ; SAVE BYTES

;*******************************************************************************
; FROM ADR, TO ADR IN TRANSFER BLOCK
; ADR ARE DIVISIBLE BY 16
; ADR OF BLOCK WAS IN REG X
; X SAVED ON STK BY IO

HSDTA               proc
                    tsx                           ; GET TRANSFER PACKET
                    ldx       2,x
                    ldd       ,x                  ; GET FROM ADR
                    std       PNTR                ; SAVE FOR DUMP
                    ldd       2,x                 ; GET TO ADR
                    std       TEMPA
SHOW9               jsr       PCRLF               ; LINE FEED
          ; PRINT BLOCK HEADING
                    ldx       #SPACE6             ; PR LEADING BLANKS
                    jsr       PDATA
                    clra
PRTTL               psha
                    jsr       OUTHR               ; CONVERT TO DISPLAY
                    jsr       SPACE
                    jsr       SPACE               ; PR 2 SPACES
                    pula                          ; GET CNTR
                    inca
                    cmpa      #$10                ; PR 0-F
                    bne       PRTTL               ; FINISHED?
          ; CHECK IF USER WANT TO TERMINT DISPLAY CMD
SHOW10              bsr       CHKABT
                    beq       SHOW19              ; RETURN IF CONTROL X
                    jsr       PCRLF
                    ldx       #PNTR               ; GET ADR OF LINE
                    jsr       OUT4HS              ; PRINT ADR
                    ldx       PNTR                ; GET CONTENTS OF MEMORY
                    ldb       #16                 ; CNTR FOR LINE
SHOW12              jsr       OUT2HS              ; PR DATA
                    inx                           ; INCR ADR PNTR
                    decb
                    bne       SHOW12              ; LOOP
                    jsr       SPACE               ; PRINT ASCII DUMP
                    ldb       #16                 ; NUM CHAR/LINE
                    ldx       PNTR
SHOW14              lda       ,x
                    anda      #$7F                ; CHK PRINTABLE
                    cmpa      #$20
                    blt       SHOW16              ; NON-CHAR
                    cmpa      #$61
                    blt       SHOW18
SHOW16              lda       #'.'                ; PR . FOR NON-CHAR
SHOW18              jsr       OUTCH
                    inx
                    decb
                    bne       SHOW14              ; LOOP
                    ldd       TEMPA
                    subd      PNTR
                    beq       SHOW19              ; RETURN
                    stx       PNTR                ; SAVE FROM ADR
                    tst       PNTR+1
                    bne       SHOW10              ; END OF LINE
                    bra       SHOW9               ; END OF BLOCK

; IO CALL - TO SAVE A FEW BYTES
IO2                 jmp       IO

;*******************************************************************************
; READ <DELIM> <ADR1> <ADR2>

RD2ADR              proc
                    cmpa      #$0D                ; CR?
                    beq       Fail@@
                    bsr       PVALIN              ; CALL INPUT ROUTINE
                    ble       Fail@@              ; CHK IF NUMBER
                    stx       BBLK+1              ; SAVE 1ST ADR (PNTR)
          ; INPUT CHECKS FOR DELIMITER
                    cmpa      #$D                 ; CR?
                    beq       Fail@@              ; DO NOT ALLOW CR
PNCH3               bsr       PVALIN              ; READ NEXT ADR
                    ble       Fail@@              ; VALID ADR?
                    cmpa      #$D                 ; REQUIRE CR AFTER ADR
                    beq       Done@@
Fail@@              lda       #$FF                ; ERR RTN
                    pulx                          ; REMOVE SUBR CALL ADR
Done@@              rts

;*******************************************************************************
; P <ADR1> <ADR2>
; PUNCH FROM <ADR1> TO <ADR2>
; ERROR IF <ADR2> LT <ADR1>
; SET UP TRANSFER PACKET
; 1ST WRD - FCN FOR PUNCH = 0
; 2ND, 3RD WRDS = <ADR1>
; 4TH, 5TH WRDS = <ADR2>
; LDX W/ ADR OF TRANSFER PACKET
; JMP THRU IO VECTOR TO BSDTA

PUNCH               proc
                    clr       BBLK                ; SET BULK STR FCN
                    bsr       RD2ADR              ; READ 2 ADDRESSES

          ; HEX STILL IN TEMPA (BBLK+3) - END ADR

PNCH4               jsr       PCRLF

          ; SET NO ECHO FLAG/ TAPE FLAG

                    lda       #$10                ; # NULLS W/TAPE CR
                    sta       OUTSW
                    ldb       #BS.ON              ; TURN PUNCH ON
                    bsr       IO2
                    ldx       #BBLK               ; ADR OF BULK STORE BLK
                    ldb       #BS.DTA             ; OFFSET TO BULK ROUTINE
                    bsr       IO2
                    psha                          ; SAVE FOR RETURN CC
                    ldb       #BS.OFF             ; TURN OFF TAPE
                    bsr       IO2
                    jsr       CHKABT              ; CLEAR IO BUF
                    jsr       CHKABT              ; DOUBLE BUF
                    clr       OUTSW               ; TURN PR ON
                    pula
                    tsta                          ; SET RETURN PR
                    rts

;*******************************************************************************
; L  LOAD A TAPE FILE
; L <OFFSET>  LOAD WITH AN OFFSET
; SET FUNCTION IN BULK STORE PACKET
; IF OFFSET - 3RD, 4TH WRDS OF PACKET = OFFSET
; LDX W/ ADR OF TRANSFER PACKET
; JMP THRU IO VECTOR TO BSDTA

LOAD                proc
                    ldb       #1                  ; SET LOAD FCN = 1
LOAD2               stb       BBLK
                    ldx       #0                  ; INIT OFFSET=0
                    stx       BBLK+3
                    cmpa      #$D                 ; CR?
                    beq       PNCH4               ; YES
                    bsr       PNCH3
                    bra       PNCH4

;*******************************************************************************
; V  VERIFY THAT TAPE LOADED CORRECTLY
; V <OFFSET> CHECK PROG LOADED WITH OFFSET CORRECTLY
; SET FCN IN BULK STORE PACKET
; IF OFFSET - 3RD, 4TH WRDS = OFFSET
; LDX W/ ADR OF PACKET
; JMP THRU IO VECTOR TO BSDTA

VERF                proc
                    ldb       #$FF
                    bra       LOAD2

;*******************************************************************************
; TURN PUNCH ON FOR READ OR WRITE
; BBLK MUST BE SET - BBLK=0 WRITE
;                BBLK#0 ON FOR READ

BSON                proc
                    lda       #$11                ; SET FOR READ
                    tst       BBLK
                    bne       Go@@                ; JUMP IF VERF/LOAD
                    inca                          ; SET REG A=$12 FOR WRT TAPE
Go@@                jmp       OUTCH

;*******************************************************************************

BSOFF               proc
                    ldx       #PUNOFF             ; TURN PUNCH OFF
                    jsr       PDATA1              ; OUTPUT STRG & RTN
                    jmp       DELAY               ; WAIT FOR PRT SYNC

;*******************************************************************************

BSDTA               proc
                    tsx                           ; BULK STORE DATA
                    ldx       2,x                 ; GET IO PACK VECTOR
                    lda       ,x                  ; GET FCN
                    sta       BBLK                ; USED BY VERF/LOAD
                    beq       BSPUN               ; JUMP TO PUNCH, FCN=0

          ; FALL THRU TO VERF-BBLK=-1, LOAD-BBLK=1

; VERIFY, LOAD
; GET OFFSET FROM IO PACKET
; FIND S1 REC - DATA
; READ BYTE CNT (TEMP)
; READ ADDRESS - SET REG X
; READ & STORE DATA, COMPUTE CHK SUM
; COMPARE TAPE TO COMPUTED CKSUM

                    ldd       3,x                 ; GET OFFSET
                    std       PNTR
LOAD3               jsr       INCHNP              ; READ
LOAD4               cmpa      #'S'                ; GET 1ST GOOD REC
                    bne       LOAD3
                    jsr       INCHNP
                    cmpa      #'9'
                    beq       LOAD20              ; FINI AFTER S9
                    cmpa      #'1'                ; DATA REC
                    bne       LOAD4               ; NO
                    clr       CKSUM               ; INIT CHECK SUM
                    bsr       BYTE                ; GET BYTE CNT
                    subb      #2                  ; DECR BYTE CNT FROM IT
                    stb       TEMP                ; STORAGE FOR BYTE CNT
          ; READ 4 HEX DIGITS FROM INPUT
          ; FORM ADDRESS AND STORE IN REG X
                    bsr       BYTE                ; 1 BYTE
                    pshb                          ; SAVE 1ST BYTE
                    bsr       BYTE                ; 2ND BYTE
                    pula                          ; GET 1ST BYTE
                    addd      PNTR                ; ADD OFFSET
                    pshd                          ; MOVE A:B TO X
                    pulx                          ; SET REG X = ADR
          ; STORE DATA
LOAD11              bsr       BYTE                ; GET BYTE IN REG B
                    dec       TEMP                ; DEC BYTE CNT
                    beq       LOAD15              ; END REC?
                    tst       BBLK                ; SKIP STORE IF VERF
                    bmi       LOAD12              ; JUST COMPARE
                    stb       ,x
LOAD12              cmpb      ,x
                    bne       LOAD19              ; ERROR
                    inx
                    bra       LOAD11

; CHECKSUMS GOOD?
; CKSUM IS ONE'S COMPLE

LOAD15              inca                          ; CHKSUM ADDED INTO B
                    beq       LOAD3               ; GET NEXT REC
          ; CHECKSUM ERROR, VERF FAILURE, LOAD FAIL ERR
LOAD19              lda       #$FF                ; SET NEG FOR ER RTN
LOAD20              rts

;*******************************************************************************
; FORM A HEX BYTE FROM 2 DISPLAY BYTES
; CALL INHEX TO READ 1 HEX DIGIT FROM INPUT

BYTE                proc
                    clrb                          ; READ A-F AS HEX
                    jsr       INHEX
                    ldb       #16
                    mul                           ; LSB IN REG B
                    pshb                          ; SAVE
                    clrb                          ; FLAG FOR INHEX
                    jsr       INHEX
                    pulb
                    aba                           ; GET 1 BYTE
                    tab                           ; SAVE IN B
                    adda      CKSUM
                    sta       CKSUM
                    rts

;*******************************************************************************
; BSDTA - PUNCH
; MOVE FROM & TO ADDRESSES TO STORAGE
; PNTR - FROM ADR   TEMPA - TO ADR
; BBLK - REUSED FOR FRAME CNT
; TEMP - REUSED FOR BYTE CNT
; PUNCH NULLS AS LEADER ON TAPE
; PUNCH CR/LF, NULL, S1(RECORD TYPE),
;       FRAME COUNT, ADDRESS, DATA, CHECKSUM
; EOF RECORD - S9030000FC

BSPUN               proc
                    ldd       1,x                 ; GET FROM ADR
                    std       PNTR
                    ldd       3,x                 ; GET TO ADR
                    std       TEMPA
          ; PUNCH LEADER
                    ldb       #25
PNULL               clra                          ; OUTPUT NULL CHAR
                    jsr       OUTCH
                    decb
                    bne       PNULL               ; LOOP IF NOT FINI
PUN11               ldd       TEMPA
                    subb      PNTR+1
                    sbca      PNTR                ; FROM ADR < TO ADR?
                    bne       PUN22
                    cmpb      #24
                    bcs       PUN23
PUN22               ldb       #23                 ; SET FRAME CNT
PUN23               addb      #4
                    stb       BBLK
                    subb      #3
                    stb       TEMP                ; BYTE CNT THIS REC
          ; PUNCH CR/LF, NULLS,S,1
                    ldx       #MTAPE
                    jsr       PDATA
                    clrb                          ; ZERO CHKSUM
          ; PUNCH FRAME CNT
                    ldx       #BBLK
                    bsr       PUNT2               ; PUNCH 2 HEX CHAR
          ; PUNCH ADDRESS
                    ldx       #PNTR
                    bsr       PUNT2
                    inx
                    bsr       PUNT2
          ; PUNCH DATA
                    ldx       PNTR
PUN32               bsr       PUNT2               ; PUNCH 1BYTE (2 FRAMES)
                    inx                           ; INCR X PNTR
                    dec       TEMP                ; DECR BYTE CNT
                    bne       PUN32
                    stx       PNTR
                    comb
                    pshb
                    tsx
                    bsr       PUNT2               ; PUNCH CHKSUM
                    pulb                          ; RESTORE
                    ldx       PNTR
                    dex
                    cpx       TEMPA
                    bne       PUN11
                    ldx       #MEOF               ; PUNCH EOF
                    jsr       PDATA
                    clra                          ; CLEAR CC FOR RETURN
                    rts

; PUNCH 2 HEX CHAR, UPDATE CHKSUM

PUNT2               addb      ,x
                    jmp       OUT2H               ; OUTPUT 2 HEX & RTN

;*******************************************************************************
; ROM DATA
;*******************************************************************************

PRTON               fcb       $10,$3A,$10,$39,4   ; TURN ON PRT
PUNOFF              fcb       $14,$13             ; TAPE CONTROL
                    fcb       4                   ; EOF
QMARK               fcb       $3F,4               ; PR ?
LIL                 fcc       'LILBUG 1.0'
                    fcb       4
NOCHG               fcc       'NO CHG'
                    fcb       4                   ; EOF
MTAPE               fcb       'S1',4
MEOF                fcc       'S9030000FC'
                    fcb       $D,4
PRTOP               fcc       'OP-'               ; PRT FOR TRACE LINE
                    fcb       4
ARRAY               fcb       'P',1               ; ARRAY OF REG AND WRD LEN
                    fcb       'X',1
                    fcb       'A',0
                    fcb       'B',0
                    fcb       'C',0
                    fcb       'S',1
SPACE6              fcc       '      '            ; 6 SPACES FOR SHOW HEADER
                    fcb       4

;*************** VECTORS ***************
; VECTOR INDEPENDENCE
; ALSO SAVE ON RAM USAGE
; VECPTR - RAM PNTR TO VECTOR TABLE
; VECTOR TABLE - ADR OF INTERRUPT VECTORS
; MAY BE REDEFINED BY USER TABLE IN SAME FORM

SERIAL              dw        DUMMY               ; NOT USED BY MONITOR
TIMOVF              dw        DUMMY               ; (DUMMY IS AN RTI)
TIMOUT              dw        DUMMY
TIMIN               dw        DUMMY
IRQ1                dw        DUMMY
SWI                 dw        IN.SWI
NMI                 dw        IN.NMI

                    org       $FFD6

;*******************************************************************************
; USE ADR ON STK TO OBTAIN INDEX
; USE INDEX TO GET CORRECT VECTOR
;    ROUTINE ADR FROM VECTOR TABLE.

VECTOR              proc
                    puld                          ; THROW AWAY MSB OF ADR & GET LSB
                    subd      #I.SER+2
                    ldx       VECPTR              ; ADR OF VECTOR TABLE
                    abx                           ; ADD OFFSET
                    ldx       ,x                  ; GET VECTOR ADR
                    jmp       ,x                  ; GO THRU VECTOR

; INTERRUPTS GO THRU VECTORS, THEN HERE BSR STORES ADR ON STACK
; ADR USED TO OBTAIN INDEX INTO VECTOR TABL

I.SER               bsr       VECTOR
I.TOVF              bsr       VECTOR
I.TOVT              bsr       VECTOR
I.TIN               bsr       VECTOR
I.IRQ1              bsr       VECTOR
I.SWI               bsr       VECTOR
I.NMI               bsr       VECTOR

;*******************************************************************************
; INTERRUPT VECTORS
;*******************************************************************************

VECTR               dw        I.SER
                    dw        I.TOVF
                    dw        I.TOVT
                    dw        I.TIN
                    dw        I.IRQ1
                    dw        I.SWI
                    dw        I.NMI
                    dw        STRT

                    end       Start

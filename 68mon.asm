;*******************************************************************************
;* Program   : 68MON.ASM
;* Programmer: Keith Vasilakes    K?V   Original
;*           : Tony Papadimitriou TGP   Modified
;* Purpose   : 6811 ML Monitor
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;* Status    : FREEWARE
;* History   : 92.03.?? v1.00 K?V Original
;*           : 98.08.?? v1.01 TGP Adapted to ASM11 by Tony Papadimitriou
;*           : 00.10.20       TGP Added NOICE support
;*           : 00.10.26 v1.02 TGP On zero S9 address, do not execute user code
;*           : 11.02.20       Minor changes in some messages
;*           : 12.03.09       Minor changes
;*           : 17.02.08       Adapted to current coding conventions
;*           : 17.02.09       BugFix: Infinite ClearRAM loop during startup
;*******************************************************************************
;
;                 '6811 ML Monitor'
;            (c) MARCH 1992 KEITH VASILAKES
;            (c) AUGUST 1998 TONY PAPADIMITRIOU
;
;  Modified for ASM11 by Tony Papadimitriou, August 1998
;  Optimized and changed a few bits without altering functionality.
;  (Also, modified to work with the 811E2's memory map.)
;
;  Assemble using ASM11 v9.70 or later. 68Mon is setup to reside
;  at $F800 but isn't too picky about where it's at. Programs written to
;  run under 68Mon must end in an SWI or the results are undefined (crash)
;  note that as soon as an illegal opcode is encountered control is
;  returned to 68Mon. Be careful of page zero especially the stack
;  pointers at $00F8 and $00FA
;
; V1.1       ADD S19 UPLOAD  **************DONE*******************
; V1.2       ADD HELP ( LIST COMMANDS ) ***DONE*****************
; V1.3       Tony Papadimitriou's modified version (also, adapted to ASM11)
;
;  [THE FOLLOWING COMMENTS ARE FROM THE ORIGINAL AUTHOR]
;  This is a small ML monitor that I originally wrote on my Commodore 64
;  using a symbolic cross-assembler I wrote in 6502 assembly. The assembler
;  was nice if non-standard and lacking features such as conditional assembly
;  includes, etc. Shortly after finishing 68mon I broke down and
;  bought an Amiga 2000HD, this allowed me to use AS11 and the Buffalo
;  monitor. As it turns out Buffalo is huuuuge, designed to run on EVB
;  boards and doesn't like other systems. So I resurrected 68mon, quickly
;  ported it to as11, and here is the result. It's not much but then again
;  it's not supposed to be.
;
;  68mon neither requires nor expects expansion RAM and uses only five
;  bytes of zero page RAM for variables, unless INTERRUPTS is defined which
;  uses another 48 bytes. 68mon keeps track of two stacks,
;  one monitor stack and one user stack. If the INTERRUPTS variable is
;  defined 68mon allows the use of all of the 68HC11 interrupts via a
;  pseudovector system a la Barfalo mon, 68mon however uses different memory
;  locations so be careful.  (I implemented my vectors before noticing
;  Buffalo's pattern.)
;
;  68mon supports some standard monitor functions that are
;  listed below including an intell upload (if defined, that is) for those
;  cases when intell hex makes more sense such as when an s19 file has been
;  converted to intel hex (such as for my EPROM blaster) and the s19 code
;  doesn't exist.
;
;  Note that 68mon has some useful functions that can be called from your
;  assembly language program.  These functions include string I/O character
;  conversion, and serial port support. See 68mon.h for a complete listing.
;
;  Not supported is writing to the eeprom, changing the baud rate.  There may
;  be other functions missing, oh well, feel free to add them, and your name
;  to the list at the top. just remember 68mon is supposed to be small and
;  light, make it possible to undefine unnessary code like INTERRUPTS for
;  those who need lots of room.
;
;
;LEGAL STUFF:
;  This program is hereby released into the public domain. It may not be sold
;  in any form for any price. If included with hardware offered for sale,
;  the words "Public Domain Monitor 68Mon V1.2" must be clearly visible on all
;  sales literature.

VERSION             equ       120                 ;version as x.xx

#ifdef ?
  #Message +===================================================
  #Message | Available conditionals (for use with -Dx option)
  #Message +===================================================
  #Message | DEBUG: for SIM11x runs (faster bps, etc.)
  #Message | TONYP: for Tony's modifications
  #Message | GREEK: for Greek Language menus
  #Message | ECHO.: for enabling SCI echo
  #Message | WSI..: for the WSI Development Kit
  #Message | NOICE: for the NOICE Debugger
  #Message | F1...: for F1 with 32KB ROM and 32KB RAM
  #Message | RAMF1: for F1 with virtual ROM
  #Message | ROM..:<value> to change ROM location
  #Message +===================================================
  #Fatal Run ASM11 -Dx (where x is any of the above)
#endif
                    #ListOff
          #ifdef F1
ROM                 def       $F800
                    #Uses     exp-f1.inc
          #else ifdef WSI
                    #Uses     wsi.inc             ;equates for the WSI Development Kit
          #else ifdef NOICE
                    #Uses     noice.inc
          #else ifdef RAMF1
MHZ                 def       16
                    #Uses     ram-f1.inc
          #else
                    #Uses     811e2.inc           ;equates for the M68HC811E2
          #endif
                    #ListOn
          #ifdef TONYP
                    #Message  Tony's mods enabled
          #endif

          #ifdef DEBUG
                    #Message  DEBUG Mode (do NOT burn device)
SPEED               def       $00                 ;Max bps rate for SIM11E
          #else ifdef __WSI__
SPEED               def       $04                 ;9600 bps rate
          #else if MHZ = 8
SPEED               def       $30                 ;9600 bps rate
          #else if MHZ = 16
SPEED               def       $30                 ;19200 bps rate
          #endif

          #ifndef SPEED
                    #Error    SPEED not defined
          #endif

;*******************************************************************************
                    #RAM
;*******************************************************************************

;pseudo interrupts similar to Barfalomon but notice the different addresses and order !

PSCI                rmb       3                   ;serial communication interface (RS232)
PSPI                rmb       3                   ;sync serial port
PPAC                rmb       3                   ;pulse accumulator input edge
PPACOV              rmb       3                   ;  "       "       overflow
PTOF                rmb       3                   ;timer overflow
PTIOC45             rmb       3                   ;in capt 4 / out comp 5
PTOC4               rmb       3                   ;output compare 4
PTOC3               rmb       3                   ;  "      "     3
PTOC2               rmb       3                   ;  "      "     2
PTOC1               rmb       3                   ;  "      "     1
PTIC3               rmb       3                   ;input capture  3
PTIC2               rmb       3                   ;  "      "     2
PTIC1               rmb       3                   ;  "      "     1
PRTI                rmb       3                   ;real time interrupt
PIRQ                rmb       3                   ;irq
PXIRQ               rmb       3                   ;xirq

NUMBER_OF_VECTORS   equ       *-PSCI/3

;*************************ZERO PAGE USAGE**********

TEMPX               rmb       2                   ;$00FE
FLAG                rmb       1                   ;$00FD
CHECK               rmb       1                   ;$00FC  ;USED FOR SUMCHECK
S19FLG              rmb       1                   ;$00FB  ;CURRENT LINE IS S9 RECORD
S0FLAG              rmb       1                   ;$00FA  ;CURRENT LINE IS S0 RECORD

;MONSTACK           rmb       2                   ;$00F9
USRSTACK            rmb       2                   ;$00F7

;**************************************************

                    rmb       20                  ;User Stack
USTACK              equ       *                   ;$00D0

;*******************************************************************************

                    #ROM
          #ifdef GREEK
                    #Message  Greek Language

TRAPP               fcs       LF,'* ΠΑΓΙΔΑ ΑΚΥΡΩΝ ΕΝΤΟΛΩΝ *',LF
PROMPT              fcc       LF,'68Mon v{VERSION(2)} (c) 1992-{:year} Κηθ Βασιλάκης',LF
                    fcs       '68Mon v{VERSION+1(2)} (c) {:year} (ASM11) από Αντώνη Παπαδημητρίου',LF
          #else
                    #Message  English Language

TRAPP               fcs       LF,'* ILLEGAL OPCODE TRAP *',LF
PROMPT              fcc       LF,'68Mon v{VERSION(2)} (c) 1992-{:year} Keith Vasilakes',LF
                    fcs       '68Mon v{VERSION+1(2)} (c) {:year} (ASM11) by Tony Papadimitriou',LF
          #endif
Prompt2             fcs       CR,'68MON-->'
FormFeed            fcs       ASCII_FF

;***************** THIS IS WHERE IT ALL STARTS *****************

COLD                proc
                    sei                           ;Disable interrupts (for no-reset start)
                    lds       #STACKTOP

          #ifdef __ASPISYS__
                    @SetChipSelects
          #endif

          #ifdef __WSI__
                    jsr       SetupWSI
          #endif
                    ldx       #REGS
                    lda       #$20
                    ora       [SPCR,x
                    sta       [SPCR,x

                    clr       [SCCR1,x
                    lda       #SPEED
                    sta       [BAUD,x
                    lda       #%1100              ;TX/RX enabled in polled mode
                    sta       [SCCR2,x
                    clr       FLAG                ;CLR ECHO (BIT 7 ) = ECHO CHARS

                    ldx       #STACKTOP           ;Clear all RAM
ClearRAM@@          clr       ,x
                    dex
                    cmpx      #RAM_END
                    bhs       DoneClearRAM@@
                    cmpx      #RAM
                    bhs       ClearRAM@@
DoneClearRAM@@

; Initialize all pseudovectors to an RTI instruction

          #ifdef TONYP
                    ldb       #NUMBER_OF_VECTORS  ;number of entries to point
                    ldy       #PSEUDOVECT         ;.. to this address
                    lda       #JMP_OPCODE         ;Get the JMP opcode
                    ldx       #PSCI               ;first vector
SetVectors@@        sta       ,x
                    sty       1,x
                    inx:3
                    decb
                    bne       SetVectors@@
          #else
                    lda       #JMP_OPCODE         ;Get the JMP opcode
                    ldx       #PSEUDOVECT

?                   macro     Vector
                    mreq      1:Vector
                    stx       ~1~+1
                    sta       ~1~
                    endm

                    @?        PSCI
                    @?        PSPI
                    @?        PPAC
                    @?        PPACOV
                    @?        PTOF
                    @?        PTIOC45
                    @?        PTOC4
                    @?        PTOC3
                    @?        PTOC2
                    @?        PTOC1
                    @?        PTIC3
                    @?        PTIC2
                    @?        PTIC1
                    @?        PRTI
                    @?        PIRQ
                    @?        PXIRQ
          #endif

          #ifdef DEBUG
                    clrd
                    clrx
                    clry
          #endif
                    cli

                    ldx       #FormFeed
                    bsr       PRINT
                    bra       MON68

;*******************************************************************************

MONSWI              proc
                    sts       USRSTACK
                    lds       #STACKTOP
                    bra       MON68

;*******************************************************************************

MONTRAP             proc
                    sts       USRSTACK
                    lds       #STACKTOP
                    ldx       #TRAPP
                    bsr       PRINT
;                   bra       MON68

;*******************************************************************************

MON68               proc
                    ldx       #PROMPT             ;SWI CALLS MONITOR
                    bsr       PRINT
                    jsr       REG1
;                   bra       MainLoop

;*******************************************************************************

MainLoop            proc
                    ldx       #Prompt2
                    bsr       PRINT
                    ldb       #3
                    ldx       #CMDTAB
                    jsr       CHRIN
                    cmpa      #CR
                    bne       Loop@@
                    lda       #LF
                    jsr       CHROUT
                    bra       MainLoop
Loop@@              cmpa      ,x
                    beq       CALL
                    tst       ,x
                    beq       NotFound@@          ;END OF TABLE
                    abx
                    bra       Loop@@

NotFound@@          bsr       ERROR
                    bra       MainLoop

;*******************************************************************************

cmd                 macro     CmdLetter,CmdAddress
                    mreq      1,2:CmdLetter,CmdAddress
                    fcb       \@~1~\@
                    fdb       ~2~
                    endm

CMDTAB              @cmd      M,MEMEX
                    @cmd      G,GO
                    @cmd      U,UPLOAD
                    @cmd      F,FILL
                    @cmd      R,REGISTER
                    @cmd      C,CONTINUE
                    @cmd      S,S19UPLOAD
                    @cmd      ?,HELP
                    fcb       NUL                 ;Marks end of command table

;*******************************************************************************

ERROR               proc
                    pshx
                    ldx       #Msg@@
                    bsr       PRINT
                    pulx
                    sec
                    rts

          #ifdef GREEK
Msg@@               fcs       '*ΛΑΘΟΣ*',LF
          #else
Msg@@               fcs       '*ERROR*',LF
          #endif

;*******************************************************************************

CALL                proc
                    ldx       1,x
                    jsr       ,x
                    bra       MainLoop

;*******************************************************************************

PRINT               proc
Loop@@              lda       ,x
                    beq       Done@@
                    jsr       CHROUT
                    inx
                    bra       Loop@@
Done@@              equ       :AnRTS

;*******************************************************************************

INWORD              proc
                    pshd
                    bsr       INBYTE
                    bcs       Fail@@
                    tab
                    bsr       INHEX
                    bcs       Fail@@
                    psha                          ;IS THIS IN THE CORRECT ORDER?
                    pshb
                    pulx
                    puld
                    rts

Fail@@              puld
                    bra       ERROR

;*******************************************************************************

OUTWORD             proc
                    psha
                    pshx
                    pula
                    bsr       OUTHEX              ;PRINT 2 HEX CHRS
                    pula
                    bsr       OUTBYTE             ;PRINT 2 HEX CHRS + SPACE
                    pula
                    rts

;*******************************************************************************

INBYTE              proc
                    bsr       INHEX               ;ALLOW LEADING SPACES
                    bcc       Done@@
                    cmpa      #' '
                    beq       INBYTE
Done@@              rts                           ;RETURNS W CARRY SET IF NOT SP OR HEX

;*******************************************************************************

INHEX               proc
                    pshb
                    bsr       CHRIN
                    bsr       FASCII
                    bcs       Done@@
                    tab
                    bsr       CHRIN
                    bsr       FASCII
                    bcs       Done@@
                    aslb:4
                    aba
                    clc
Done@@              pulb
                    rts                           ;RETURNS WITH ERROR CHAR IN ACCA

;*******************************************************************************

OUTHEX              proc
                    psha:2
                    anda      #$F0
                    lsra:4
                    bsr       TOASCII
                    bsr       CHROUT
                    pula
                    anda      #$0F
                    bsr       TOASCII
                    bra       OUTBYTE2

;*******************************************************************************

OUTBYTE             proc
                    bsr       OUTHEX
                    psha
                    lda       #' '
OUTBYTE2            bsr       CHROUT
                    pula
                    rts

;*******************************************************************************

FASCII              proc
                    bsr       ToUpper

                    cmpa      #'0'
                    jlo       ERR

                    cmpa      #'9'
                    bls       Go@@

                    cmpa      #'A'
                    blo       ERR

                    cmpa      #'F'
                    bhi       ERR

                    adda      #'0'-'A'+10

Go@@                suba      #'0'
                    clc
                    rts

;*******************************************************************************

TOASCII             proc
                    adda      #$90
                    daa
                    adca      #$40
                    daa
                    rts

;*******************************************************************************

ToUpper             proc
                    cmpa      #'a'
                    blo       Done@@

                    cmpa      #'z'
                    bhi       Done@@

                    adda      #'A'-'a'
Done@@              rts

;*******************************************************************************

ToLower             proc
                    cmpa      #'A'
                    blo       Done@@

                    cmpa      #'Z'
                    bhi       Done@@

                    adda      #'a'-'A'
Done@@              rts

;*******************************************************************************

CHRIN               proc
Loop@@              bsr       GetChar
                    bcs       Loop@@

                    cmpa      #BS                 ;backspace not recognized
                    beq       Loop@@

                    bsr       ToUpper

                    tst       FLAG
                    bmi       Done@@
                    bra       CHROUT

Done@@              equ       :AnRTS

;*******************************************************************************

CHROUT.TWICE        proc
                    bsr       CHROUT              ;display the character twice
;                   bra       CHROUT

;*******************************************************************************

CHROUT              proc
Loop@@              bsr       PutChar
                    bcs       Loop@@
                    rts

;*******************************************************************************

GetChar             lda       SCSR
                    anda      #$20
                    beq       ERR
                    lda       SCDR
                    clc
                    rts

;*******************************************************************************

PutChar             proc
                    cmpa      #LF
                    bne       Send@@

                    lda       #CR
                    bsr       Send@@
                    lda       #LF

Send@@              tst       SCSR
                    bpl       ERR
                    sta       SCDR
                    clc
                    rts

;*******************************************************************************

MEMEX               proc
                    jsr       INWORD
                    bcs       ERR
                    stx       TEMPX
Loop@@              jsr       INBYTE              ;CHANGE MEMORY
                    bcs       READ0
                    sta       ,x
                    inx
                    bra       Loop@@

;*******************************************************************************

LINEFEED            proc
                    lda       #CR
                    bsr       CHROUT
                    lda       #LF
                    bra       CHROUT

FILLERR             pulx
ERR                 sec
                    rts

;*******************************************************************************

READ0               cmpa      #ESC
                    beq       ERR

                    ldx       TEMPX
                    bsr       LINEFEED
READ                jsr       OUTWORD             ;PRINT ADDRESS
                    ldb       #16                 ;NUMBER OF BYTES PER LINE
READ1               lda       ,x
                    jsr       OUTBYTE
                    inx
                    decb
                    bne       READ1

                    ldx       TEMPX
                    ldb       #16
READ2               lda       ,x
                    cmpa      #' '
                    blo       READ4
          #ifdef GREEK
                    bra       READ3               ;do not mask high ASCII chars
          #else
                    tsta                          ;mask 8-bit ASCII chars
                    bpl       READ3
          #endif
READ4               lda       #'.'
READ3               bsr       CHROUT
                    inx
                    decb
                    bne       READ2
                    stx       TEMPX
                    bsr       GetChar             ;PRINT DATA UNTIL KEYPRESS
                    bcs       READ0
READ5               bra       LINEFEED

;*******************************************************************************

FILL                proc
                    jsr       INWORD              ;GET FROM ADDRESS
                    bcs       ERR
                    pshx
                    jsr       INWORD              ;GET TO ADDRESS
                    bcs       FILLERR
                    stx       TEMPX
                    jsr       INBYTE              ;GET FILL VALUE
                    bcc       Go@@
                    lda       #-1                 ;IF NO FILL VALUE,FILL WITH NOP'S
Go@@                pulx
Loop@@              sta       ,x
                    inx
                    cpx       TEMPX
                    bls       Loop@@
                    bra       LINEFEED

;*******************************************************************************

UPLOAD              proc
                    lda       FLAG
                    psha
                    ora       #Bit7.              ;BIT 7 SET = NOECHO
                    sta       FLAG
                    bsr       LINEFEED
                    lda       #'.'                ;print a dot
                    jsr       CHROUT
UPSTART             jsr       CHRIN               ;get a character
                    bcs       ERROR1
                    cmpa      #':'
                    bne       UPEND
                    clr       CHECK
                    jsr       INBYTE              ;GET NUM OF BYTES
                    bcs       ERROR1
                    psha                          ;SAVE NUMBER OF BYTES (WILL BE USED IN ACCB LATER)
                    adda      CHECK
                    sta       CHECK
                    jsr       INWORD              ;GET ADDRESS
                    bcc       UPOK
                    pula
                    bra       ERROR1
UPOK                pshx
                    xgdx
                    adda      CHECK
                    aba
                    sta       CHECK
                    pulx
                    pulb                          ;GET BACK NUMBER OF FCBs
                    jsr       INBYTE              ;GET NULL (?) FCB
                    adda      CHECK
                    sta       CHECK
                    incb
UPLOAD1             jsr       INBYTE
                    bcs       ERROR1
                    decb
                    beq       END2
                    sta       ,x
                    adda      CHECK
                    sta       CHECK
                    inx
                    bra       UPLOAD1
END2                neg       CHECK
                    cmpa      CHECK
                    bne       ERROR1
                    jsr       CHRIN               ;GET CR AT END OF LINE
                    bra       UPSTART

UPEND               pula
                    ora       #1
                    sta       FLAG
                    rts

ERROR1              pula
                    sta       FLAG
                    jmp       ERROR

;*******************************************************************************

REGISTER            jsr       CHRIN
                    cmpa      #CR
                    bne       :AnRTS

REG1                lda       #LF
                    jsr       CHROUT
                    ldx       #REGNAME
                    jsr       PRINT
                    lda       #' '
                    jsr       CHROUT.TWICE
                    ldy       USRSTACK            ;Get the CCR
                    iny
                    ldb       ,y                  ;Get the saved CCR in B
                    lda       #8                  ;number of bits to display
                    sta       TEMPX

REG                 clra                          ;Display the 8 CCR bits
                    aslb                          ;get next bit into Carry
                    adca      #'0'                ;produces '0' or '1'
                    jsr       CHROUT              ;prints it
                    dec       TEMPX               ;are we done with all 8?
                    bne       REG                 ;if not, repeat

                    lda       #' '                ;print a couple of spaces
                    jsr       CHROUT.TWICE
                    lda       2,y                 ;Display Register A
                    jsr       OUTBYTE             ;ACCA
                    lda       1,y                 ;Display Register B
                    jsr       OUTBYTE             ;ACCB
                    ldx       3,y                 ;Display Register X
                    jsr       OUTWORD             ;INX
                    ldx       5,y                 ;Display Register Y
                    jsr       OUTWORD             ;INY
                    ldx       7,y                 ;Display PC
                    jsr       OUTWORD             ;PC
                    ldx       USRSTACK            ;Display Stack Pointer
                    jsr       OUTWORD             ;SP
                    jsr       LINEFEED            ;..and finally, advance line
                    jmp       CHROUT

GO                  jsr       INWORD
                    bcs       :AnRTS
                    jsr       CHRIN
                    cmpa      #CR
                    bne       :AnRTS
                    lds       #USTACK             ;reset user stack
                    jmp       ,x                  ;CALL USER PROG,x

CONTINUE            jsr       CHRIN
                    cmpa      #CR
                    bne       :AnRTS
                    lds       USRSTACK            ;CONTINUE FROM LAST SWI
                    rti

;*******************************************************************************

HELP                proc
                    ldx       #Msg@@
                    jmp       PRINT

          #ifdef GREEK
Msg@@               fcc       LF,LF,' *** Διαθέσιμες εντολές ***',LF,LF
                    fcc       '? Ένδειξη παρούσας οθόνης',LF
                    fcc       'F x y z Γέμισμα μνήμης από x έως y με z',LF
                    fcc       'M x Ένδειξη μνήμης από x μέχρι πίεση πλήκτρου',LF
                    fcc       'M x y z Αλλαγή Μνήμης, γράφει y στο x και z στο x+1',LF
                    fcc       'G x Έναρξη προγράμματος από x. Το πρόγραμμα έχει δικό',LF
                    fcc       '    του stack και τερματίζεται με SWI',LF
                    fcc       'R   Ένδειξη καταχωρητών',LF
                    fcc       'C   Επανεκκίνηση μετά από SWI',LF
                    fcc       'S   Αποστολή αρχείου S19',LF
                    fcs       'U   Αποστολή αρχείου intel hex',LF,LF
          #else
Msg@@               fcc       LF,LF,' *** Available Commands ***',LF,LF
                    fcc       '? Show this screen',LF
                    fcc       'F x y z Fill memory from x to y with z',LF
                    fcc       'M x Memory display at x until a key press',LF
                    fcc       'M x y z Memory change, writes y to x and z to x+1',LF
                    fcc       'G x Run program at x. Prog has own stack and ends with a SWI',LF
                    fcc       'R   Show Registers',LF
                    fcc       'C   Continue after SWI',LF
                    fcc       'S   Upload S19 file',LF
                    fcs       'U   Upload intel hex file',LF,LF
          #endif

;*******************************************************************************

S19UPLOAD           proc
          #ifdef TONYP
                    clrx
                    bsr       S19
                    stx       TEMPX
                    bcs       Err@@
                    cmpx      #0
                    jeq       ERROR               ;if zero, no S9 address
                    jmp       ,x
                    bra       *
          #else
                    lda       FLAG
                    psha
          #ifdef ECHO
                    ora       #Bit7.              ;BIT 7 SET = NOECHO
                    sta       FLAG
          #endif

          #if *-LINEFEED < 126
                    bsr       LINEFEED
          #else
                    jsr       LINEFEED
          #endif

SUPSTART            clra
                    sta       S19FLG
                    sta       S0FLAG
                    jsr       CHRIN
                    bcs       Fail@@
                    cmpa      #'S'
                    bne       Fail@@
                    jsr       CHRIN
                    cmpa      #'1'
                    beq       SUPSTRT
                    cmpa      #'0'
                    bne       S9?@@
                    lda       #-1
                    sta       S0FLAG              ;SET FLAG TO NOT STORE THIS LINE
                    bra       SUPSTRT

S9?@@               cmpa      #'9'
                    bne       Fail@@

                    lda       #-1                 ;S9 RECEIVED SET FLAG TO JUMP TO ADDRESS AT END
                    sta       S19FLG

SUPSTRT             clr       CHECK
                    jsr       INBYTE              ;GET NUM OF BYTES
                    bcs       Fail@@
                    psha                          ;SAVE NUMBER OF BYTES (WILL BE USED IN ACCB LATER)
                    adda      CHECK
                    sta       CHECK
                    jsr       INWORD              ;GET ADDRESS
                    stx       TEMPX
                    bcc       SUPOK
                    pula
                    bra       Fail@@

SUPOK               pshx
                    xgdx
                    adda      CHECK
                    aba
                    sta       CHECK
                    pulx
                    pulb                          ;GET BACK NUMBER OF BYTES
                    subb      #2
SUPLOAD1            jsr       INBYTE
                    bcs       Fail@@
                    decb                          ;dec byte count
                    beq       SEND2
                    tst       S0FLAG              ;IF S0 DON'T STORE DATA
                    bne       NOSTORE
                    sta       ,x
NOSTORE             adda      CHECK
                    sta       CHECK
                    inx
                    bra       SUPLOAD1

SEND2               com       CHECK
                    cmpa      CHECK               ;Compare Checksum at end of line with calculated checksum
                    bne       Fail@@
                    jsr       CHRIN               ;GETCR AT END OF LINE
                    tst       S19FLG
                    beq       SUPSTART

          ;S19 Upload End

                    pula
                    ora       #1                  ;DOWNLOAD OK
                    sta       FLAG                ;LOCAL ECHO ON + DOWNLOAD OK
                    ldx       TEMPX               ;EXECUTE PROGRAM AT S9 ADDRESS
                    jeq       ERROR               ;if zero, no S9 address
                    jmp       ,x

Fail@@              pula
                    sta       FLAG
          #endif
Err@@               jmp       ERROR

;*******************************************************************************

PSEUDOVECT          rti

;*******************************************************************************

REGNAME             fcs       '  SXHINZVC  A[D]B  IX   IY   PC   SP',LF
                              ;  12345678  12 12 1234 1234 1234 1234
#ifdef TONYP
          #ifexists lib/s19load.sub
                    #Uses     lib/s19load.sub
          #else
                    #Error    No S19LOAD.SUB (assemble without -dTONYP)
          #endif
#endif

;*******************************************************************************

                    #VECTORS
                    org       VECTORS

                    dw        PSCI
                    dw        PSPI
                    dw        PPAC
                    dw        PPACOV
                    dw        PTOF
                    dw        PTIOC45
                    dw        PTOC4
                    dw        PTOC3
                    dw        PTOC2
                    dw        PTOC1
                    dw        PTIC3
                    dw        PTIC2
                    dw        PTIC1
                    dw        PRTI
                    dw        PIRQ
                    dw        PXIRQ
                    dw        MONSWI
                    dw        MONTRAP
                    dw:3      COLD                ;COP, COP1, and RESET

                    end       :s19crc

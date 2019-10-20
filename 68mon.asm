; ' 6811 ML monitor'
; (c) MARCH 1992 KEITH VASILAKES

; This is a small ml monitor that I originally wrote on my Commodore 64
; using a symbolic crossassembler I wrote in 6502 assembly. The assembler
; was nice if nonstandard and lacking features such as conditional assembly
; includes, etc. Shortly after finishing 68mon I broke down and
; bought an Amiga 2000HD, this allowed me to use AS11 and the Buffalo
; monitor. As it turns out Buffalo is huuuuge, designed to run on EVB
; boards and doesn't like other systems. So I resurrected 68mon quickly
; ported it to as11 and here is the result. Its not much but then again
; its not supposed to be.

; 68mon neither requires nor expects expansion ram and uses only five
; bytes of zero page ram for variables,unless INTERRUPTS is defined which
; uses another 48 bytes. 68mon keeps track of two stacks,
; one monitor stack and one user stack. If the INTERRUPTS variable is
; defined 68mon allows the use of all of the 68HC11 interrupts via a
; pseudovector system ala Barfalo mon, 68mon however uses different memory
; locations so be careful. (I implemented my vectors before noticing
; Buffalo's pattern )
; 68mon supports some standard monitor functions that are
; listed below including an intel upload ( if defined that is ) for those
; cases when intel hex makes more sense such as when an s19 file has been
; converted to intel hex (such as for my EPROM blaster) and the s19 code
; doesn't exist.

; Note that 68mon has some usefull functions that can   be called from your
; assembly language program. these functions include string IO character
; conversion, and serial port support. See 68mon.h for a complete listing

; Not supported is writing to the eeprom, changing the baud rate  There may
; be other functions missing, oh well , feel free to add them, and your name
; to the list at the top. just remember 68mon is supposed to be small and
; light, make it possible to undefine unnessary code like INTERRUPTS for
; those who need lots of room.

; LEGAL STUFF:
; This program is hereby released into the public domain. It my not be sold
; in any form for any price. If included with hardware offered for sale,
; the words "Pubic Domain Monitor 68Mon V1.2" must be clearly visable on all
; sales literature.

; Usage:
; Assemble using AS11 or compatable assembler. 68Mon is setup to reside
; at $E000 but isn"t too picky about where it's at. Programs written to
; run under 68Mon must end in an SWI or the results are undefined ( crash )
; note that as soon as an illegal opcode is encountered control is
; returned to 68Mon. Be careful of page zero especially the stack
; pointers at $00F8 and $00FA

; V1.1       ADD S19 UPLOAD  **************DONE*******************
; V1.2       ADD HELP ( LIST COMMANDS ) ***DONE*****************
; V1.?       ADD XON / XOFF ($13 = XOFF, $11 = XON )  YUCK !!!

ROM                 equ       $E000               ; start of eprom
PORTD               equ       $1008
DDRD                equ       $1009
SPCR                equ       $1028
BAUD                equ       $102B
SCCR1               equ       $102C
SCCR2               equ       $102D
SCSR                equ       $102E
SCDAT               equ       $102F

                    org       $0000               ; pseudo interrupts similar to Barfalomon but notice the different adresses and order !

PSCI                equ       0                   ; serial comunication interface (RS232)
PSPI                equ       0003                ; sync serial port
PPAC                equ       0006                ; pulse accumulator input edge
PPACOV              equ       0009                ; "       " overflow
PTOF                equ       $000C               ; timer overflow
PTIOC45             equ       $000F               ; in capt 4 / out comp 5
PTOC4               equ       $0012               ; output compare 4
PTOC3               equ       $0015               ; "      " 3
PTOC2               equ       $0018               ; "      " 2
PTOC1               equ       $001B               ; "      " 1
PTIC3               equ       $001E               ; input capture 3
PTIC2               equ       $0021               ; "      " 2
PTIC1               equ       $0024               ; "      " 1
PRTI                equ       $0027               ; real time interrupt
PIRQ                equ       $002A               ; irq
PXIRQ               equ       $002D               ; xirq

                    org       ROM

SP                  equ       $20
CR                  equ       $0D
LF                  equ       $0A
ESC                 equ       $1B
; XON               equ       $11                 ; cant quite figure this out yet ????
; XOFF              equ       $13
;*************************ZERO PAGE USAGE**********
TEMPX               equ       $00FE
FLAG                equ       $00FD
CHECK               equ       $00FC               ; USED FOR SUMCHECK
S19FLG              equ       $00FB               ; CURRENT LINE IS S9 RECORD
S0FLAG              equ       $00FA               ; CURRENT LINE IS S0 RECORD

MONSTACK            equ       $00F9
USRSTACK            equ       $00F7
;**************************************************
STACK               equ       $00F6
USTACK              equ       $00D0

TRAPP               fcb       CR,LF
                    fcb       CR,LF
                    fcb       '    ******** ILLEGAL OPCODE TRAP !!! ********'
                    fcb       CR,LF
                    fcb       0
PROMPT              fcb       CR,LF
                    fcb       '               68Mon V1.2 (C) 1992 Keith Vasilakes'
                    fcb       CR,LF
                    fcb       0
COLD                lds       #STACK
                    lda       #$20
                    oraa      SPCR
                    sta       SPCR
                    lda       #$30
                    sta       BAUD
                    lda       #$0C
                    sta       SCCR2
                    clr       FLAG                ; CLR ECCO (BIT 7 ) = ECCO CHARS

                    lda       #$7E
                    ldx       #PSEUDOVECT
                    stx       PSCI+1
                    sta       PSCI
                    stx       PSPI+1
                    sta       PSPI
                    stx       PPAC+1
                    sta       PPAC
                    stx       PPACOV+1
                    sta       PPACOV
                    stx       PTOF+1
                    sta       PTOF
                    stx       PTIOC45+1
                    sta       PTIOC45
                    stx       PTOC4+1
                    sta       PTOC4
                    stx       PTOC3+1
                    sta       PTOC3
                    stx       PTOC2+1
                    sta       PTOC2
                    stx       PTOC1+1
                    sta       PTOC1
                    stx       PTIC3+1
                    sta       PTIC3
                    stx       PTIC2+1
                    sta       PTIC2
                    stx       PTIC1+1
                    sta       PTIC1
                    stx       PRTI+1
                    sta       PRTI
                    stx       PIRQ+1
                    sta       PIRQ
                    stx       PXIRQ+1
                    sta       PXIRQ

                    cli

                    bra       MON68

MONSWI              sts       USRSTACK
                    lds       #STACK
                    bra       MON68

MONTRAP             sts       USRSTACK
                    lds       #STACK
                    ldx       #TRAPP
                    bsr       PRINT
MON68               ldx       #PROMPT             ; SWI CALLS MONITOR
                    bsr       PRINT
                    jsr       REG1
MAIN                lda       #'>'
                    jsr       CHROUT
                    ldb       #3
                    ldx       #CMDTAB
                    jsr       CHRIN
                    jsr       TOUPPER
                    cmpa      #CR
                    bne       LOOP
                    lda       #LF
                    jsr       CHROUT
                    bra       MAIN

LOOP                cmpa      0,x
                    beq       CALL
                    tst       0,x
                    beq       NOTFOUND            ; END OF TABLE
                    abx
                    bra       LOOP

NOTFOUND            bsr       ERROR
                    bra       MAIN

ERROR               lda       #'?'
                    jsr       CHROUT
                    lda       #CR
                    jsr       CHROUT
                    lda       #LF
                    jsr       CHROUT
                    sec
                    rts

CALL                ldx       1,x
                    jsr       0,x
                    bra       MAIN

PRINT               lda       0,x
                    beq       PREND
                    jsr       CHROUT
                    inx
                    bra       PRINT

PREND               rts

INWORD              pshb
                    psha
                    bsr       INBYTE
                    bcs       WRDERR
                    tab
                    bsr       INHEX
                    bcs       WRDERR
                    psha
                    pshb
                    pulx
                    pula
                    pulb
                    rts

WRDERR              pula
                    pulb
                    bra       ERROR

OUTWORD             psha
                    pshx
                    pula
                    bsr       OUTHEX              ; PRINT 2 HEX CHRS
                    pula
                    bsr       OUTBYTE             ; PRINT 2 HEX CHRS + SPACE
                    pula
                    rts

INBYTE              bsr       INHEX               ; ALLOW LEADING SPACES
                    bcc       INB1
                    cmpa      #SP
                    beq       INBYTE
INB1                rts                           ; RETURNS W CARRY SET IF NOT SP OR HEX

INHEX               pshb
INH1                bsr       CHRIN
                    bsr       FASCII
                    bcs       INHERR
                    tab
                    bsr       CHRIN
                    bsr       FASCII
                    bcs       INHERR
                    aslb:4
                    aba
                    clc
INHERR              pulb
                    rts                           ; RETURNS WITH ERROR CHAR IN ACCA

OUTHEX              psha
                    psha
                    anda      #$F0
                    lsra:4
                    bsr       TOASCII
                    bsr       CHROUT
                    pula
                    anda      #$0F
                    bsr       TOASCII
                    bsr       CHROUT
                    pula
                    rts

OUTBYTE             bsr       OUTHEX
                    psha
                    lda       #SP
                    bsr       CHROUT
                    pula
                    rts

FASCII              bsr       TOUPPER
                    cmpa      #$30
                    blo       GETEND
                    cmpa      #$39
                    bls       FASC1
                    cmpa      #$41
                    blo       GETEND
                    cmpa      #$46
                    bhi       GETEND
                    suba      #$07
FASC1               suba      #$30
                    clc
                    rts

TOASCII             clc
                    adda      #$90
                    daa
                    adca      #$40
                    daa
                    rts

TOUPPER             cmpa      #$61
                    blo       END
                    cmpa      #$7A
                    bhi       END
                    suba      #$20
END                 rts

TOLOWER             cmpa      #$41
                    blo       END1
                    cmpa      #$5A
                    bhi       END1
                    adda      #$20
END1                rts

CHRIN               bsr       GETIN
                    bcs       CHRIN
                    tst       FLAG
                    bmi       END1
                    bra       CHROUT

GETIN               lda       SCSR
                    anda      #$20
                    beq       GETEND
                    lda       SCDAT
                    clc
                    rts

GETEND              sec
                    rts

CHROUT              bsr       PUTOUT
                    bcs       CHROUT
                    rts

PUTOUT              pshb
PUTOUT1             ldb       SCSR
                    andb      #$80
                    beq       PUTOUT2
                    sta       SCDAT
                    pulb
                    clc
                    rts

PUTOUT2             pulb
                    sec
                    rts

CMDTAB              fcb       'M'
                    fdb       MEMEX
                    fcb       'G'
                    fdb       GO
                    fcb       'U'
                    fdb       UPLOAD
                    fcb       'F'
                    fdb       FILL
                    fcb       'R'
                    fdb       REGISTER
                    fcb       'C'
                    fdb       CONTINUE
                    fcb       'S'
                    fdb       S19UPLOAD
                    fcb       '?'
                    fdb       HELP
                    fcb       0

MEMEX               jsr       INWORD
                    bcs       ERR
                    stx       TEMPX
MEMX                jsr       INBYTE              ; CHANGE MEMORY
                    bcs       READ0
                    sta       0,x
                    inx
                    bra       MEMX

READ0               cmpa      #ESC
                    beq       ERR
                    ldx       TEMPX
                    lda       #CR
                    bsr       CHROUT
                    lda       #LF
                    bsr       CHROUT
READ                jsr       OUTWORD             ; PRINT ADDRESS
                    ldb       #$10                ; NUMBER OF BYTES PER LINE
READ1               lda       0,x
                    jsr       OUTBYTE
                    inx
                    decb
                    bne       READ1
                    ldx       TEMPX
                    ldb       #$10
READ2               lda       0,x
                    cmpa      #SP
                    blo       READ4
                    cmpa      #$80
                    bls       READ3
READ4               lda       #'.'
READ3               bsr       CHROUT
                    inx
                    decb
                    bne       READ2
                    stx       TEMPX
                    jsr       GETIN               ; PRINT DATA UNTIL KEYPRESS
                    bcs       READ0
READ5               lda       #CR
                    bsr       CHROUT
                    lda       #LF
                    jsr       CHROUT
                    rts

FILLERR             pulx
ERR                 sec
                    rts

FILL                jsr       INWORD              ; GET FROM ADDRESS
                    bcs       ERR
                    pshx
                    jsr       INWORD              ; GET TO ADDRESS
                    bcs       FILLERR
                    stx       TEMPX
                    jsr       INBYTE              ; GET FILL VALUE
                    bcc       FILLX
                    lda       #$FF                ; IF NO FILL VALUE,FILL WITH NOP'S
FILLX               pulx
FILL1               sta       0,x
                    inx
                    cpx       TEMPX
                    bls       FILL1
                    lda       #CR
                    jsr       CHROUT
                    lda       #LF
                    jmp       CHROUT

UPLOAD              lda       FLAG
                    psha
                    oraa      #%10000000          ; BIT 7 SET = NO ECCO
                    sta       FLAG
                    lda       #CR
                    jsr       CHROUT
                    lda       #LF
                    jsr       CHROUT
                    lda       #'.'
                    jsr       CHROUT
UPSTART             jsr       CHRIN
                    bcs       ERROR1
                    cmpa      #':'
                    bne       UPEND
                    clr       CHECK
                    jsr       INBYTE              ; GET NUM OF BYTES
                    bcs       ERROR1
                    psha                          ; SAVE NUMBER OF BYTES (WILL BE USED IN ACCB LATER)
                    adda      CHECK
                    sta       CHECK
                    jsr       INWORD              ; GET ADDRESS
                    bcc       UPOK
                    pula
                    bra       ERROR1

UPOK                pshx
                    xgdx
                    adda      CHECK
                    sta       CHECK
                    addb      CHECK
                    stb       CHECK
                    pulx
                    pulb                          ; GET BACK NUMBER OF FCBS
                    jsr       INBYTE              ; GET NULL (?) FCB
                    adda      CHECK
                    sta       CHECK
                    incb
UPLOAD1             jsr       INBYTE
                    bcs       ERROR1
                    decb
                    beq       END2
                    sta       0,x
                    adda      CHECK
                    sta       CHECK
                    inx
                    bra       UPLOAD1

END2                neg       CHECK
                    cmpa      CHECK
                    bne       ERROR1
                    jsr       CHRIN               ; GETCR AT END OF LINE
                    bra       UPSTART

UPEND               pula
                    sta       FLAG
                    lda       #1
                    oraa      FLAG
                    sta       FLAG
                    rts

ERROR1              pula
                    sta       FLAG
                    jmp       ERROR

REGNAME             fcb       '  SXHINZVC  AB AA  IX   IY   PC   SP'
                              ; 12345678  12 12 1234 1234 1234 1234
                    fcb       CR,LF
                    fcb       0
REGISTER            jsr       CHRIN
                    cmpa      #CR
                    beq       REG1
                    bra       GOERR

REG1                lda       #LF
                    jsr       CHROUT
                    ldx       #REGNAME
                    jsr       PRINT
                    lda       #SP
                    jsr       CHROUT
                    jsr       CHROUT
                    ldx       USRSTACK
                    inx
                    ldb       0,x
                    lda       #8
                    sta       TEMPX
REG                 clra
                    aslb
                    adca      #'0'
                    jsr       CHROUT
                    dec       TEMPX
                    bne       REG
                    lda       #SP
                    jsr       CHROUT
                    jsr       CHROUT
                    lda       1,x
                    jsr       OUTBYTE             ; ACCB
                    lda       2,x
                    jsr       OUTBYTE             ; ACCA
                    ldd       3,x
                    xgdx
                    jsr       OUTWORD             ; INX
                    xgdx
                    ldd       5,x
                    xgdx
                    jsr       OUTWORD             ; INY
                    xgdx
                    ldd       7,x
                    xgdx
                    jsr       OUTWORD             ; PC
                    ldx       USRSTACK
                    jsr       OUTWORD             ; SP
                    lda       #CR
                    jsr       CHROUT
                    lda       #LF
                    jsr       CHROUT
                    rts

GO                  jsr       INWORD
                    bcs       GOERR
                    jsr       CHRIN
                    cmpa      #CR
                    bne       GOERR
                    lds       #USTACK
                    jmp       0,x                 ; CALL USER PROG,x

GOERR               rts

CONTINUE            jsr       CHRIN
                    cmpa      #CR
                    bne       GOERR
                    lds       USRSTACK            ; CONTINUE FROM LAST SWI
                    rti

HELP                ldx       #HELPLIST
                    jsr       PRINT
                    rts

HELPLIST            fcb       CR,LF,CR,LF,CR,LF,CR,LF,CR,LF
                    fcb       'COMMANDS:'
                    fcb       CR,LF,CR,LF
                    fcb       CR,LF
                    fcb       CR,LF
                    fcb       '?        DISPLAYS THIS SCREEN'
                    fcb       CR,LF,CR,LF
                    fcb       'F [xxxx] [yyyy] [zz] FILL MEMORY FROM xxxx to yyyy with zz '
                    fcb       CR,LF,CR,LF
                    fcb       'M [xxxx] MEMORY EXAMINE, DISPLAYS DATA AT xxxx UNTIL ANY KEY IS PRESSED'
                    fcb       CR,LF,CR,LF
                    fcb       'M [xxxx] [yy zz ...] MEMORY CHANGE, WRITES yy TO xxxx AND zz TO xxxx +1'
                    fcb       CR,LF,CR,LF
                    fcb       'G xxxx   TRANSFERS CONTROL TO PROGAM AT xxxx. PROG IS GIVEN ITS OWN'
                    fcb       CR,LF,CR,LF
                    fcb       '          STACK AND MUST END WITH A SWI TO RETURN TO THE MONITOR'
                    fcb       CR,LF,CR,LF
                    fcb       'R        DISPLAYS USER REGISTERS'
                    fcb       CR,LF,CR,LF
                    fcb       'C        CONTINUES A USER PROGRAM AFTER A SWI, LIKE A BREAKPOINT'
                    fcb       CR,LF,CR,LF
                    fcb       'S        UPLOADS A MOTO S19 HEX FILE'
                    fcb       CR,LF,CR,LF
                    fcb       'U        UPLOADS AN INTEL HEX FILE'
                    fcb       CR,LF,CR,LF
                    fcb       CR,LF
                    fcb       0

S19UPLOAD           lda       FLAG
                    psha
;                   oraa      #%10000000          ; BIT 7 SET = NO ECCO
;                   sta       FLAG
                    lda       #CR
                    jsr       CHROUT
                    lda       #LF
                    jsr       CHROUT
SUPSTART            clr       S19FLG
                    clr       S0FLAG
                    jsr       CHRIN
                    bcs       SERROR1
                    cmpa      #'S'
                    bne       SERROR1
                    jsr       CHRIN
                    cmpa      #'1'
                    beq       SUPSTRT
                    cmpa      #'0'
                    bne       CHKS9
                    lda       #$FF
                    sta       S0FLAG              ; SET FLAG TO NOT STORE THIS LINE
                    bra       SUPSTRT

CHKS9               cmpa      #'9'
                    bne       SERROR1
                    lda       #$FF                ; S9 RECIEVED SET FLAG TO JUMP TO ADDRESS AT END
                    sta       S19FLG
SUPSTRT             clr       CHECK
                    jsr       INBYTE              ; GET NUM OF BYTES
                    bcs       SERROR1
                    psha                          ; SAVE NUMBER OF BYTES (WILL BE USED IN ACCB LATER)
                    adda      CHECK
                    sta       CHECK
                    jsr       INWORD              ; GET ADDRESS
                    stx       TEMPX
                    bcc       SUPOK
                    pula
                    bra       SERROR1

SUPOK               pshx
                    xgdx
                    adda      CHECK
                    sta       CHECK
                    addb      CHECK
                    stb       CHECK
                    pulx
                    pulb                          ; GET BACK NUMBER OF BYTES
                    subb      #2
SUPLOAD1            jsr       INBYTE
                    bcs       SERROR1
                    decb                          ; dec byte count
                    beq       SEND2
                    tst       S0FLAG              ; IF S0 DONT STORE DATA
                    bne       NOSTORE
                    sta       0,x
NOSTORE             adda      CHECK
                    sta       CHECK
                    inx
                    bra       SUPLOAD1

SEND2               com       CHECK
                    cmpa      CHECK               ; Compare Checksum at end of line with calculated checksum
                    bne       SERROR1
                    jsr       CHRIN               ; GETCR AT END OF LINE
                    tst       S19FLG
                    beq       SUPSTART
SUPEND              pula
                    sta       FLAG                ; LOCAL ECCO ON
                    lda       #1                  ; DOWNLOAD OK
                    oraa      FLAG
                    sta       FLAG
                    ldx       TEMPX               ; EXECUTE PROGRAM AT S9 ADDRESS
                    jmp       0,x

SERROR1             pula
                    sta       FLAG
                    jmp       ERROR

PSEUDOVECT          rti

;**********VECTORS*******************

                    org       $FFD6

SCI                 fdb       PSCI
SPI                 fdb       PSPI
PACC                fdb       PPAC
PACCOV              fdb       PPACOV
TOVF                fdb       PTOF
TIOC45              fdb       PTIOC45
TOC4                fdb       PTOC4
TOC3                fdb       PTOC3
TOC2                fdb       PTOC2
TOC1                fdb       PTOC1
TIC3                fdb       PTIC3
TIC2                fdb       PTIC2
TIC1                fdb       PTIC1
RTI                 fdb       PRTI
IRQ                 fdb       PIRQ
XIRQ                fdb       PXIRQ

SWI                 fdb       MONSWI
TRAP                fdb       MONTRAP
COP                 fdb       COLD
COP1                fdb       COLD
RESET               fdb       COLD

                    end

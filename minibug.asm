;*******************************************************************************
; M I N I - D E B U G G E R    FUER  HC11
;
; E. WITTICH  21.11.1989
; Modified for ASM11 by Tony Papadimitriou 98.09.22
;*******************************************************************************
;
; ACHTUNG! 1. BOOT MODE AM HC11 EINSTELLEN (MODE-A, MODE-B PIN AUF LOW SCHALTEN)
; 2. AM HC11 RESET BETAETIGEN
; 3. UNTER MS-DOS "MINIBUG" EINGEBEN ->  MINI-DEBUGGER WIRD GELADEN
; (ES WIRD KERMIT AUFRUFEN (BAUDRATE 1200 BEI 8-MHZ))
; 4. NACH EINGABE ENTER  -> MONITOR MELDET SICH MIT  *?
;
; BAUDRATE IST BEI 4 MHZ CRYSTAL = 600  -> BITTE FILE MINIBUG.BAT AENDERN
; 8 MHZ CRYSTAL = 1200
;
; DIESES PROGRAMM  ERLAUBT 3 BEFEHLE:
;
; G XXXX GO TO ADDRESS
; D XXXX MEMORY DISPLAY
; M XXXX MEMORY CHANGE
; ENTER := NAECHSTE SPEICHERSTELLE DARSTELLEN
; R := VORHERIGE SPEICHERSTELLE DARSTELLEN
; PUNKT := ENDE MEMORY CHANGE
;
;*******************************************************************************
;
; Speicherbelegung:  256 Byte Programmlaenge,  2 Byte RAM, 8 BYTE STACK
; $00 PROGRAMM START-ADRESSE
; $0   RAM ARBEITSBEREICH 2 BYTE
;
; MEMORY DISPLAY: D AAAA (16-BYTE PRO ZEILE ANZEIGEN BIS MAX 256 BYTE)
;
; MEMORY CHANGE: M AAAA DD NN
; Mit diesem Befehl kann der Inhalt des Speichers gelesen bzw.
; modifiziert werden.
; Nach Erscheinen des Zeichens '* ' kann nach
; Eingabe des M-Befehles sofort die anzusprechende Hex-
; adresse eingegeben werden. Die Adresse muss immer 4 stellig
; eingegeben werden. Nach Adresseingabe wird sofort der
; aktuelle Inhalt dieser Speicherzelle angezeigt. Durch eine
; zweistellige Eingabe kann der aktuelle Inhalt dieser
; Adresse ueberschrieben werden. Abschluss der Eingabe
; erfolgt immer mit der ENTER-Tasten (CR Taste).
;
; Bei Eingabe des Buchstaben 'R 'wird der vorhergehenden
; Inhalt der Speicherzellen angezeigt.
;
; Bei Eingabe des Buchstaben 'W 'wird der aktuelle Imhalt
; der Speicherzellen angezeigt.
;
; Bei Eingabe des Buchstaben '. 'wird die Eingabe beendet.
;
;*******************************************************************************

          #ifdef DEBUG
                    #Message  DEBUG Mode (do NOT burn device)
          #endif

; INIT-WERTE FUER MC68HC11

; SCI - REG.DEFINITIONEN

STACK               equ       $FF                 ; 8 BYTE

REGS                equ       $1000
ACIAS               equ       REGS+$2E            ; ACIA STATUS (CONTROL)
ACIAD               equ       REGS+$2F            ; ACIA DATA
BAUD                equ       REGS+$2B
SCCR1               equ       REGS+$2C
SCCR2               equ       REGS+$2D
SCSR                equ       $2E                 ; SCI STATUSREG.
SCDR                equ       $2F                 ; SCI DATA
HPRIO               equ       $3C
MDA                 equ       $20
SMOD                equ       $40
RDRF                equ       $20                 ; SCI READY
TDRE                equ       $80

CR                  equ       13
LF                  equ       10

; WORKING AREA

XHI                 equ       0
XLOW                equ       1

;*******************************************************************************
                    #ROM
;*******************************************************************************
                    org       0

;*******************************************************************************
;**** INITIALISIERUNG NACH RESET
;*******************************************************************************

Start               proc
                    ldy       #REGS               ; IO-ADDRESSE
                    lds       #STACK
          #ifdef
                    bset      HPRIO,Y,#MDA        ; FORCE SPEC.TEST MODE
                    bclr      HPRIO,Y,#SMOD       ; SET EXPANDED MODE
          #endif
          ;-------------------------------------- ; OPTION FUER 9600 BAUD
          #ifdef DEBUG
                    clra
          #else
                    lda       #$30
          #endif
                    sta       BAUD                ; 9600 BAUD EINSTELLEN
                    lda       #$0C                ; Enable Transmitter & Receiver
                    sta       SCCR2
;                   bra       MainLoop

;*******************************************************************************

MainLoop            proc
                    jsr       <PDATA1             ; ? UND * AUSGEBEN
                    bsr       INCH                ; PROGRAM RETURN

                    cmpa      #'M'
                    beq       MEMCHG              ; EXEC MEMORY CHANGE

                    cmpa      #'G'
                    beq       GOADR               ; GO TO ADRESSE

                    cmpa      #'D'
                    bne       MainLoop

                    jmp       MDISP               ; MEMORY-DISPLAY

;*******************************************************************************
; G XXXX   -> JUMP TO PROGRAM

GOADR               proc
                    bsr       BADDR
                    jmp       ,x                  ; GO TO ADRESSE

;*******************************************************************************
; WAIT FOR INPUT FROM RS232

INCH                proc
                    brclr     SCSR,Y,#RDRF,*
                    lda       ACIAD
                    bra       OUTCH

;*******************************************************************************
; MEMORY CHANGE M AAAA DD NN

MEMCHG              proc
                    bsr       BADDR
MainLoop@@          bsr       OUT2HM              ; SPEICHER INHALT ANZEIGEN
                    jsr       <OUTS               ; BLANK AUSGEBEN

Loop@@              bsr       INCH

                    cmpa      #CR                 ; CR? = ADR + 1
                    beq       _3@@

                    cmpa      #'R'                ; EINGABE R = ADR.-1
                    beq       _1@@

                    cmpa      #'.'                ; PUNKT
                    beq       ENDC

                    cmpa      #'W'
                    beq       _2@@                ; LETZTE ADRESSE WIEDERHOLEN

                    bsr       INHEX2              ; EINGABE 2. HEX BYTE
                    bsr       BYTE2               ; HEX ZEICHEN IN ACC
          ;-------------------------------------- ; BYTE -> SPEICHER
                    sta       ,x                  ; A -> ((XHI,XLOW))
                    cmpa      ,x                  ; COMPARE -> ((XHI,XLOW))
                    beq       Loop@@              ; OK
ENDC                bra       MainLoop
          ;-------------------------------------- ; ADR. -1
_1@@                dex:2
          ;-------------------------------------- ; LF1 AUSGEBEN
_3@@                inx                           ; ADR +1
_2@@                bsr       PDATA3              ; AUSGABE: LF, CR, *
                    bsr       OUT2HA              ; AKT.ADR.AUSGEBEN
                    bra       MainLoop@@

;*******************************************************************************
; 2 BYTE HEX-ADRESSE EINGEBEN

BADDR               proc
                    bsr       OUTS                ; BLANK AUSGEBEN
                    bsr       BYTE                ; 1. BYTE EINGEBEN
                    sta       XHI
                    bsr       BYTE                ; 2. BYTE EINGEBEN
                    sta       XLOW
                    ldx       XHI                 ; EINGABE-ADR. -> X
                    rts

;*******************************************************************************
; 1 BYTE HEX EINGEBEN

BYTE                proc
                    bsr       INHEX
;                   bra       BYTE2

;*******************************************************************************

BYTE2               proc
                    asla:4
                    tab
                    bsr       INHEX
                    aba
                    rts

;*******************************************************************************
; AKT.ADRESSE ANZEIGEN

OUT2HA              proc
                    stx       XHI                 ; AKT.ADR.ZWISCHENSPEICHER
                    lda       XHI
                    bsr       OUT2H               ; AKT.ADRESSE ANZEIGEN
                    lda       XLOW
                    bra       OUT2H

;*******************************************************************************
; INHALT DER AKT.ADRESSE AUSGEBEN: BLANK, ((XHI,XLOW))

OUT2HM              proc
                    bsr       OUTS                ; LEERSCHRITT AUSGEBEN
                    lda       ,x                  ; ((XHI,XLOW)) -> A
;                   bra       OUT2H

;*******************************************************************************
; 1 BYTE IN HEX ANZEIGEN

OUT2H               proc
                    tab
                    bsr       OUTHL
                    tba
                    bra       OUTHR

;*******************************************************************************

OUTHL               proc
                    lsra:4
;                   bra       OUTHR

;*******************************************************************************

OUTHR               proc
                    anda      #$F
                    adda      #'0'
                    cmpa      #'9'
                    bls       OUTCH
                    adda      #7
;                   bra       OUTCH

;*******************************************************************************
; SENDE ZEICHEN -> RS232:  (A)=SENDE-ZEICHEN

OUTCH               proc
                    brclr     SCSR,Y,#TDRE,*      ; OUTPUT READY?
                    sta       ACIAD
                    rts

;*******************************************************************************
; INPUT 1 HEX BYTE

INHEX               proc
                    jsr       INCH
INHEX2              suba      #'0'
                    cmpa      #9
                    bls       Done@@
                    suba      #7
Done@@              rts

;*******************************************************************************
; AUSGABE  ? LF CR * BLANK

PDATA1              proc
                    lda       #'?'
                    bsr       OUTCH
;                   bra       PDATA3

;*******************************************************************************

PDATA3              proc
                    lda       #LF
                    bsr       OUTCH

                    lda       #CR
                    bsr       OUTCH

                    lda       #'*'
                    bsr       OUTCH
;                   bra       OUTS

;*******************************************************************************

OUTS                proc
                    lda       #' '                ; BLANK AUSGEBEN
                    bra       OUTCH

;*******************************************************************************
; MEMORY DISPLAY:  * D AAAA DD DD DD DD DD DD DD DD DD DD DD DD DD DD DD DD

MDISP               proc
                    bsr       BADDR               ; EINGABE ADRESSE
                    bra       Cont@@

Loop@@              bsr       OUT2HM              ; INHALT DER AKT.ADRESSE AUSGEBEN
                    inx
          ;-------------------------------------- ; ADR +1 AUSGEBEN
                    inc       XLOW                ; ADR +1
                    beq       ENDC                ; ENDE DES 256 BLOCK ERREICHT
                    lda       XLOW
                    anda      #$0F
                    bne       Loop@@              ; 256 BYTE BLOCK ANZEIGEN
Cont@@              bsr       PDATA3              ; CR UND * AUSGEBEN
                    bsr       OUT2HA              ; AKT.ADRESSE ANZEIGEN
                    bra       Loop@@

                    end       :s19crc

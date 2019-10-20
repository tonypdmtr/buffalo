;***************************************
; Moniteur 68HC11 V1.3
;
; Par: Sylvain Bissonnette
;      8382 De Fougeray
;      Anjou, Que.
;      H1K 1K6
;
;***************************************
; Adapted to assemble with P&E's IASM
; and ASM11 by Tony Papadimitriou
;***************************************

;***************************************
; Etiquette pour ACIA de communication
;***************************************

REGS                equ       $1000

BAUD                equ       REGS+$2B
SCCR1               equ       REGS+$2C
SCCR2               equ       REGS+$2D
SCSR                equ       REGS+$2E
SCDR                equ       REGS+$2F

TMSK2               equ       REGS+$24
OPTION              equ       REGS+$39

;***************************************
; Etiquette de la Ram du system
;***************************************

; Registre des pseudo vecteurs

PSSCI               equ       $C1
PSSPI               equ       $C4
PSPAI               equ       $C7
PSPAO               equ       $CA
PSTO                equ       $CD
PSTOC5              equ       $D0
PSTOC4              equ       $D3
PSTOC3              equ       $D6
PSTOC2              equ       $D9
PSTOC1              equ       $DC
PSTIC3              equ       $DF
PSTIC2              equ       $E2
PSTIC1              equ       $E5
PSRTI               equ       $E8
PSIRQ               equ       $EB
PSXIRQ              equ       $EE
PSSWI               equ       $F1
PSIOT               equ       $F4
PSCOPF              equ       $F7
PSCOPC              equ       $FA
PSRESET             equ       $FD

; Registre de l'usager

REGCCR              equ       $B6
REGB                equ       $B7
REGA                equ       $B8
REGX                equ       $B9
REGY                equ       $BB
REGPC               equ       $BD
REGST               equ       $BF

; Restart

RESTART             equ       $A0

; Buffer getkey

QIN                 equ       $A1                 ; 2 BYTE
BUFFER              equ       $A3                 ; 4 BYTE

; Valeur du prescaler

PRESCALE            equ       $A7                 ; 1 BYTE

; Variable d'initialisation

STARTUP             equ       $A8                 ; 1 BYTE

; Stack

STACK               equ       $0080

; MEMOIRE DU SYSTEM

MEM                 equ       $0000

;***************************************
; Programme principale
;***************************************

                    org       $E000

RESET               nop
                    nop
                    ldx       #REGS
                    bset      OPTION-REGS,x,%00001000  ; RESET ON STOP CLOCK
                    ldaa      STARTUP
                    cmpa      #%10101010
                    bne       RESET1

                    ldaa      PRESCALE            ; SET LE PRESCALER AVEND 64xE
                    anda      #%00000011
                    ldx       #REGS
                    staa      TMSK2-REGS,x

RESET1              sei                           ; Masque IRQ & FIRQ
                    tpa
                    anda      #%01111111          ; Permet les STOP
                    tap
                    lds       #STACK              ; Initialisation du stack
                    ldy       #MEM
                    ldaa      #%10101010
                    staa      STARTUP
                    ldaa      #$00
                    staa      RESTART,y           ; Met RESTART a $00 quand power on
                    jsr       INIVEC              ; Initialisation des pseudo vecteur
                    jsr       INISCI              ; Initialisation de l'acia de communication
                    jsr       SNDCRLF
                    bsr       MENU                ; Affiche le menu
BOUCLE              bsr       WAITCOM             ; Va attendre une commande
                    bra       BOUCLE

;***************************************
; Attend une commande du terminal
; et l'execute
;***************************************

WAITCOM             jsr       INCAR
                    cmpa      #'S'                ; Download S1S9
                    beq       GO1
                    cmpa      #'G'                ; Execution de programme en ram
                    beq       GO2
                    cmpa      #'R'                ; Reset du system
                    beq       GO3
                    cmpa      #'M'                ; Menu des commandes
                    beq       GO4
                    cmpa      #'C'                ; Continuer apres un SWI
                    beq       GO5
                    cmpa      #'E'                ; Examein de memoire
                    beq       GO6
                    cmpa      #'P'                ; Ajustement du prescaler
                    beq       GO7
                    rts

GO1                 jmp       S1S9

GO2                 jmp       EXEC

GO3                 bra       RESET

GO4                 ldy       #MEM
                    ldaa      #$00
                    staa      RESTART,y
                    bra       MENU

GO5                 jsr       SNDCRLF
                    ldx       #GOMSG1
                    jsr       SNDMSG
                    jmp       SNDCRLF

GO6                 jmp       MEMEX

GO7                 jmp       PSCALE

GOMSG1              fcb       '!! IMPOSIBLE DE CONTINUER IL N`Y A PAS EU DE SWI !!'
                    fcb       $04

;***************************************
; Menu
;***************************************

MENU                jsr       SNDCRLF
                    ldx       #MENUMSG            ; Affiche l'entete
                    jsr       SNDMSG
                    ldy       #MEM
                    ldaa      #%10101010
                    cmpa      RESTART,y
                    bne       MENU2
                    rts

MENU2               ldx       #MENUMSG2
                    jsr       SNDMSG              ; Affiche le menu des commandes
                    jsr       SNDCRLF
                    ldy       #MEM
                    ldaa      #%10101010
                    staa      RESTART,y
                    rts

MENUMSG             fcb       '****************************************',$0D,$0A
                    fcb       '*   MONITEUR POUR 68HC11 EVBU++ V1.3   *',$0D,$0A
                    fcb       '*                                      *',$0D,$0A
                    fcb       '*      Par: Sylvain Bissonnette        *',$0D,$0A
                    fcb       '****************************************',$0D,$0A,$0A,$0A,04

MENUMSG2            fcb       'G-> Execution de programme en RAM',$0D,$0A
                    fcb       'R-> Reset du system',$0D,$0A
                    fcb       'M-> Menu des commandes',$0D,$0A
                    fcb       'C-> Pour continuer apres un SWI',$0D,$0A
                    fcb       'E-> Examein de memoire',$0D,$0A
                    fcb       'P-> Ajustement du prescaler',$0D,$0A,$04

;***************************************
; Initialisation de l'acia de
; communication
; 9600 bps, 8 bit, pas de parity, 1 sb
;
; Reg aff: A,CCR
;***************************************

INISCI              ldx       #REGS
                    ldaa      #$30                ; 9600 BPS
                    staa      BAUD-REGS,x

                    ldaa      #$00                ; 8 BIT, NO WAKE UP, 1 STOP BIT
                    staa      SCCR1-REGS,x

                    ldaa      #$0C                ; PAS INTERRUPT, TX ENABLE, RX ENABLE
                    staa      SCCR2-REGS,x
                    rts

;***************************************
; Initialisation des pseudo vecteur
;***************************************

INIVEC              ldx       #VECTAB
                    ldy       #PSSCI
INIVEC2             ldaa      0,x
                    inx
                    cmpa      #$04
                    beq       INIVEC3
                    staa      0,y
                    iny
                    bra       INIVEC2

INIVEC3             rts

VECTAB              fcb       $7E                 ; SCI
                    fdb       SCI
                    fcb       $7E                 ; SPI
                    fdb       SPI
                    fcb       $7E                 ; PAI
                    fdb       PAI
                    fcb       $7E                 ; PAO
                    fdb       PAO
                    fcb       $7E                 ; TOV
                    fdb       TOV
                    fcb       $7E                 ; TOC5
                    fdb       TOC5
                    fcb       $7E                 ; TOC4
                    fdb       TOC4
                    fcb       $7E                 ; TOC3
                    fdb       TOC3
                    fcb       $7E                 ; TOC2
                    fdb       TOC2
                    fcb       $7E                 ; TOC1
                    fdb       TOC1
                    fcb       $7E                 ; TIC3
                    fdb       TIC3
                    fcb       $7E                 ; TIC2
                    fdb       TIC2
                    fcb       $7E                 ; TIC1
                    fdb       TIC1
                    fcb       $7E                 ; RTI
                    fdb       RTI
                    fcb       $7E                 ; IRQ
                    fdb       IRQ
                    fcb       $7E                 ; XIRQ
                    fdb       XIRQ
                    fcb       $7E                 ; SWI
                    fdb       SWII
                    fcb       $7E                 ; IOT
                    fdb       IOTT
                    fcb       $7E                 ; COPF
                    fdb       COPF
                    fcb       $7E                 ; COPC
                    fdb       COPC
                    fcb       $7E                 ; RESET
                    fdb       RESET

                    fcb       $04

;***************************************
; Interuption de type SWI
;***************************************

SWI                 tsx
                    ldy       #REGCCR
SWI2                ldaa      0,x
                    inx
                    staa      0,y
                    iny
                    cpy       #REGST
                    bne       SWI2
                    stx       REGST
                    bsr       SWIRES

SWI3                jsr       SNDCRLF
                    ldx       #SWIMSG
                    jsr       SNDMSG
                    jsr       INCAR
                    cmpa      #'C'
                    bne       SWI3

                    rti

SWIMSG              fcb       'APPUIE SUR C POUR CONTINUER',$0D,$0A,$04

;***************************************
; Interuption de type IOT
;***************************************

IOT                 tsx
                    ldy       #REGCCR
IOT2                ldaa      0,x
                    inx
                    staa      0,y
                    iny
                    cpy       #REGST
                    bne       IOT2
                    stx       REGST
                    bsr       SWIRES

                    tsx                           ; Ajustement de l'adresse de retour
                    ldab      #$08
                    abx
                    ldaa      0,x
                    adda      #$01
                    staa      0,x

                    rti

;**************

SWIRES              jsr       SNDCRLF             ; TITRE
                    ldx       #SWMSG1
                    jsr       SNDMSG
                    jsr       SNDCRLF

                    ldx       #SWMSG2             ; REG A
                    jsr       SNDMSG
                    ldaa      REGA
                    jsr       OUT2HA

                    ldx       #SWMSG3             ; REG B
                    jsr       SNDMSG
                    ldaa      REGB
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

                    jsr       SNDCRLF
                    jsr       SNDCRLF

                    ldx       #SWMSG8             ; REG CC
                    jsr       SNDMSG
                    jsr       SNDCRLF
                    ldx       #SWMSG9
                    jsr       SNDMSG
                    ldab      REGCCR
                    ldx       #$0000
SWIRES1             lslb
                    bcs       SWIRES2
                    ldaa      #'0'
                    bra       SWIRES3

SWIRES2             ldaa      #'1'
SWIRES3             jsr       OUTCAR
                    ldaa      #' '
                    jsr       OUTCAR
                    inx
                    cpx       #$08
                    bne       SWIRES1
                    jsr       SNDCRLF
                    jsr       SNDCRLF
                    ldx       #SWMSG10
                    jsr       SNDMSG
                    tsx
                    ldab      #$08
                    abx
                    jsr       OUT4HX
                    jmp       SNDCRLF

SWMSG1              fcb       '               V A L E U R   D E S   R E G I S T R E S',$0D,$0A
                    fcb       '---------------------------------------------------------------------',$0D,$0A,$04
SWMSG2              fcb       '            A=',$04
SWMSG3              fcb       '  B=',$04
SWMSG4              fcb       '  X=',$04
SWMSG5              fcb       '  Y=',$04
SWMSG6              fcb       '  S=',$04
SWMSG7              fcb       '  PC=',$04
SWMSG8              fcb       '                           S X H I N Z V C',$04
SWMSG9              fcb       '                           ',$04
SWMSG10             fcb       '                            STACK = ',$04


;***************************************
; Message d'interuption
;***************************************

SCI                 ldx       #SCIMSG
                    bra       MESSINT

SPI                 ldx       #SPIMSG
                    bra       MESSINT

PAI                 ldx       #PAIMSG
                    bra       MESSINT

PAO                 ldx       #PAOMSG
                    bra       MESSINT

TOV                 ldx       #TOVMSG
                    bra       MESSINT

TOC5                ldx       #TOC5MSG
                    bra       MESSINT

TOC4                ldx       #TOC4MSG
                    bra       MESSINT

TOC3                ldx       #TOC3MSG
                    bra       MESSINT

TOC2                ldx       #TOC2MSG
                    bra       MESSINT

TOC1                ldx       #TOC1MSG
                    bra       MESSINT

TIC3                ldx       #TIC3MSG
                    bra       MESSINT

TIC2                ldx       #TIC2MSG
                    bra       MESSINT

TIC1                ldx       #TIC1MSG
                    bra       MESSINT

RTI                 ldx       #RTIMSG
                    bra       MESSINT

IRQ                 ldx       #IRQMSG
                    bra       MESSINT

XIRQ                ldx       #XIRQMSG
                    bra       MESSINT

SWII                ldx       #SWIIMSG
                    bra       MESSINT2

IOTT                ldx       #IOTTMSG
                    bra       MESSINT3

COPF                ldx       #COPFMSG
                    bra       MESSINT4

COPC                ldx       #COPCMSG
                    bra       MESSINT4

RES                 ldx       #RESMSG

MESSINT             jsr       SNDCRLF
                    jsr       SNDMSG
                    jsr       SNDCRLF
                    rti

MESSINT2            jsr       SNDCRLF
                    jsr       SNDMSG
                    jsr       SNDCRLF
                    jmp       SWI

MESSINT3            jsr       SNDCRLF
                    jsr       SNDMSG
                    jsr       SNDCRLF
                    jmp       IOT

MESSINT4            jsr       SNDCRLF
                    jsr       SNDMSG
                    jsr       SNDCRLF
                    jmp       RESET

SCIMSG              fcb       '!!! INTERUPTION SCI !!!',$04
SPIMSG              fcb       '!!! INTERUPTION SPI !!!',$04
PAIMSG              fcb       '!!! INTERUPTION PAI !!!',$04
PAOMSG              fcb       '!!! INTERUPTION PAO !!!',$04
TOVMSG              fcb       '!!! INTERUPTION TO !!!',$04
TOC5MSG             fcb       '!!! INTERUPTION TOC5 !!!',$04
TOC4MSG             fcb       '!!! INTERUPTION TOC4 !!!',$04
TOC3MSG             fcb       '!!! INTERUPTION TOC3 !!!',$04
TOC2MSG             fcb       '!!! INTERUPTION TOC2 !!!',$04
TOC1MSG             fcb       '!!! INTERUPTION TOC1 !!!',$04
TIC3MSG             fcb       '!!! INTERUPTION TIC3 !!!',$04
TIC2MSG             fcb       '!!! INTERUPTION TIC2 !!!',$04
TIC1MSG             fcb       '!!! INTERUPTION TIC1 !!!',$04
RTIMSG              fcb       '!!! INTERUPTION RTI !!!',$04
IRQMSG              fcb       '!!! INTERUPTION IRQ !!!',$04
XIRQMSG             fcb       '!!! INTERUPTION XIRQ !!!',$04
SWIIMSG             fcb       '!!! INTERUPTION SWI !!!',$04
IOTTMSG             fcb       '!!! INTERUPTION Illegal Opcode Trap !!!',$04
COPFMSG             fcb       '!!! INTERUPTION COP Failure !!!',$04
COPCMSG             fcb       '!!! INTERUPTION COP Clock Monitor Fail !!!',$04
RESMSG              fcb       '!!! INTERUPTION RESET !!!',$04

;***************************************
; Examein de memoire
;***************************************

MEMEX               jsr       SNDCRLF
                    ldx       #EXMSG1
                    jsr       SNDMSG
                    jsr       GET4HX
MEMEX2              jsr       SNDCRLF
                    jsr       OUT4HX
                    ldaa      #':'
                    jsr       OUTCAR
                    ldaa      0,x
                    jsr       OUT2HA
MEMEX3              jsr       INCAR
                    cmpa      #'+'
                    beq       MEMEX4
                    cmpa      #'-'
                    beq       MEMEX5
                    cmpa      #'/'
                    beq       MEMEX6
                    cmpa      #$0D
                    beq       MEMEX7
                    bra       MEMEX3

MEMEX4              inx
                    bra       MEMEX2

MEMEX5              dex
                    bra       MEMEX2

MEMEX6              ldaa      #' '
                    jsr       OUTCAR
                    ldaa      #'/'
                    jsr       OUTCAR
                    jsr       GET2HA
                    staa      0,x
                    bra       MEMEX2

MEMEX7              jsr       INCAR
                    jmp       MENU

EXMSG1              fcb       '             EXAMEIN DE MEMOIRE',$0D,$0A,$0D,$0A
                    fcb       '(+) ou (-) Pour avencer ou reculer en memoire',$0D,$0A
                    fcb       '(/) Pour modifier le contenu de la memoire',$0D,$0A
                    fcb       '(ENTER) Pour sortir de l`examein de memoire',$0D,$0A
                    fcb       'QUELLE ADRESSE:',$04

;***************************************
; Ajustement du prescaler
;***************************************

PSCALE              ldx       #PSCALEM
                    bsr       SNDCRLF
                    jsr       SNDMSG
                    jsr       GET1HA
                    staa      PRESCALE
                    nop
                    stop

PSCALEM             fcb       'Pour diviser par x entrer',$0D,$0A
                    fcb       ' 1 -> 0',$0D,$0A
                    fcb       ' 4 -> 1',$0D,$0A
                    fcb       ' 8 -> 2',$0D,$0A
                    fcb       '16 -> 3',$0D,$0A,$0D,$0A,$04

;***************************************
; Envoie le contenu de A sur le port
; seriel
;
; Reg aff:CCR
;***************************************

OUTCAR              tst       SCSR
                    beq       OUTCAR
                    staa      SCDR
                    rts

;***************************************
; Routine qui transmet la valeur de
; A au terminal sous la forme ASCII.
;
; Reg aff:CCR
;***************************************

OUT2HA              psha
                    psha
                    lsra
                    lsra
                    lsra
                    lsra
                    jsr       HEXASC              ; conversion
                    bsr       OUTCAR              ; transmet le MSB
                    pula
                    anda      #$0F
                    jsr       HEXASC              ; conversion
                    bsr       OUTCAR              ; transmet le LSB
                    pula
                    rts

;***************************************
; Routine qui transmet la valeur de B
; au terminal sous la forme ASCII.
;
; Reg aff:CCR
;***************************************

OUT2HB              psha
                    tba
                    bsr       OUT2HA
                    pula
                    rts

;***************************************
; Routine qui transmet le nombre contenu
; dans X au terminal sous la forme ASCII.
;
; Reg aff:CCR
;***************************************

OUT4HX              psha
                    pshb
                    pshx
                    xgdx
                    bsr       OUT2HA
                    tba
                    bsr       OUT2HA
                    pulx
                    pulb
                    pula
                    rts

;***************************************
; Transmet un retour de chariot
;
; Reg aff: CCR
;***************************************

SNDCRLF             psha
                    ldaa      #$0D
                    bsr       OUTCAR
                    ldaa      #$0A
                    bsr       OUTCAR
                    pula
                    rts

;***************************************
; Envoie une chaine de charactere
; sur le port seriel. Fin avec $04
;
; X= Adresse de la chaine
;
; Reg aff:A,x,CC
;***************************************

SNDMSG              ldaa      0,x
                    inx
                    cmpa      #$04
                    beq       SNDMSG2
                    bsr       OUTCAR
                    bra       SNDMSG

SNDMSG2             rts

;***************************************
; LOOKCAR
;
; Lit le port seriel, mais attend pas
; la reception du caractere
;
; Z=1, pas de car.
; Z=0, car. recu dans A
;
; Reg Aff: A,CCR
;***************************************

LOOKCAR             pshb
                    pshx
                    ldx       #REGS
                    ldab      SCSR-REGS,x
                    bitb      #%00100000
                    beq       LOOKCAR1
                    ldaa      SCDR-REGS,x
                    psha
                    tpa
                    anda      #%11111011
                    tap
                    pula
LOOKCAR1            pulx
                    pulb
                    rts

;***************************************
; INCAR
;
; Lit le port seriel, et attend un
; caractere
;
; Reg Aff: A,CCR
;***************************************

INCAR               bsr       LOOKCAR
                    beq       INCAR
                    rts

;***************************************
; Routine pour lire un chiffre
; hexa --> LSB de acc.A
;
; Reg aff:A,CCR
;***************************************

IN1HA               bsr       INCAR
                    jmp       ASCHEX

;***************************************
; Routine pour lire un nombre
; hexa --> acc.A
;
; Reg aff:A,CCR
;***************************************

IN2HA               pshy
                    bsr       IN1HA
                    asla
                    asla
                    asla
                    asla
                    psha
                    bsr       IN1HA
                    tsy
                    oraa      0,y
                    ins
                    puly
                    rts

;***************************************
; Routine qui attend du terminal
; une nombre hexa. de 16 bits (4 touches)
; Le nombre est plac‚ dans le reg.X
;
; Reg aff:X,CCR
;***************************************

IN4HX               psha
                    pshb
                    bsr       IN2HA
                    psha
                    bsr       IN2HA
                    tab
                    pula
                    xgdx
                    pulb
                    pula
                    rts

;***************************************
; Routine pour lire un chiffre
; hexadecimal provenant du terminal.
; par.d'entree: acc.A = qte de car.
;                       a recevoir.
;                       maximum = 4
; par.de sortie:BUFFER contient
;               les car. recus
;***************************************

GETHEX              pshx
                    pshy
                    pshb
                    ldy       #MEM
                    ldx       #BUFFER
                    tab
                    abx
                    stx       QIN,y
                    ldx       #BUFFER             ; adr. du dernier car a recevoir.
GETHEX0             bsr       INCAR
                    jsr       MAJUS
                    cmpa      #$08                ; BACKSPACE ?
                    bne       GETHEX3
                    cpx       #BUFFER
                    beq       GETHEX0
                    dex
                    jsr       OUTCAR              ; echo du backspace
                    bra       GETHEX0

GETHEX3             cmpa      #$0D                ; RETURN ?
                    bne       GETHEX4
                    cpx       QIN,y               ; test si recu tous les car.
                    beq       GETHEX5             ; oui, ok
                    bra       GETHEX0             ; non, attend les autres car.

GETHEX4             cmpa      #'0'                ; decode si le car. est hexa.
                    blo       GETHEX0
                    cmpa      #'F'
                    bhi       GETHEX0
                    cmpa      #'9'
                    bls       GETHEX1
                    cmpa      #'A'
                    bhs       GETHEX1
                    bra       GETHEX0             ; si pas hexa, recommence

GETHEX1             cpx       QIN,y               ; si tous les car. sont recus,
                    beq       GETHEX0             ; attend la touche RETURN
                    jsr       OUTCAR              ; echo du car.
                    suba      #$30                ; conversion de ASCII-->hexa
                    cmpa      #$09
                    bls       GETHEX2
                    suba      #$07                ; ajustement pour code de A … F
GETHEX2             staa      0,x                 ; sauve le chiffre dans le buffer
                    inx
                    bra       GETHEX0             ; attend prochain car.

GETHEX5             pulb
                    puly
                    pulx
                    rts

;***************************************
; Routine pour lire un chiffre
; hexa --> LSB de acc. A
;
; Reg Aff: A,CCR
;***************************************

GET1HA              ldaa      #$01
                    bsr       GETHEX
                    ldaa      BUFFER
                    rts

;***************************************
; Assemble 2 chiffre hexa dans le
; buffer et les places dans A
;***************************************

ASS2HEX             pshy
                    ldaa      0,x
                    inx
                    asla
                    asla
                    asla
                    asla
                    psha
                    ldaa      0,x
                    inx
                    tsy
                    oraa      0,y
                    ins
                    puly
                    rts

;***************************************
; Routine pour lire un nombre hexa
; --> acc. A
;
; Reg Aff: A,CCR
;***************************************

GET2HA              pshx
                    ldaa      #$02
                    bsr       GETHEX
                    ldx       #BUFFER
                    bsr       ASS2HEX
                    pulx
                    rts

;***************************************
; Routine qui attend du terminal
; un nombre hexa de 16 bits (4 touches)
; le nombre est placer dans le reg. X
;
; Reg Aff: X,CCR
;****************************************

GET4HX              psha
                    pshb
                    ldaa      #$04
                    jsr       GETHEX
                    ldx       #BUFFER
                    bsr       ASS2HEX
                    psha
                    bsr       ASS2HEX
                    psha
                    pulb
                    pula
                    xgdx
                    pulb
                    pula
                    rts

;***************************************
; Convertion de ASCII->HEXA
;***************************************

ASCHEX              suba      #$30
                    cmpa      #$09
                    bls       ASCHEX2
                    suba      #$07
ASCHEX2             rts

;***************************************
; Routine de conversion HEXA-->ASCII
;***************************************

HEXASC              adda      #$30
                    cmpa      #'9'
                    bls       HEXAS
                    adda      #7
HEXAS               rts

;***************************************
; Conversion en majuscule
;***************************************

MAJUS               cmpa      #'a'
                    blo       MAJUS2
                    cmpa      #'z'
                    bhi       MAJUS2
                    suba      #$20
MAJUS2              tsta
                    rts

;***************************************
; Transfert de fichier, format S1S9
;***************************************

S1S9                jsr       INCAR
                    cmpa      #'0'
                    beq       S1
                    cmpa      #'1'
                    beq       S1
                    cmpa      #'9'
                    beq       S9
                    rts

S1                  jsr       IN2HA
                    tab
                    subb      #$03
                    jsr       IN4HX

S1A                 jsr       IN2HA
                    staa      0,x
                    inx
                    decb
                    bne       S1A
                    jmp       IN2HA

S9                  jsr       IN4HX
                    jmp       IN4HX

;***************************************
; Execute un programme en ram
;***************************************

EXEC                jsr       SNDCRLF
                    ldx       #EXECMSG
                    jsr       SNDMSG
                    bsr       GET4HX
                    ldy       #RESET
                    pshy
                    pshx
                    rts

EXECMSG             fcb       'EXECUTE A QUELLE ADRESSE?:'
                    fcb       $04

;***************************************
; Adresse des sous-routine
;***************************************

                    org       $FF00

                    fdb       OUTCAR              ; FF00
                    fdb       OUT2HA              ; FF03
                    fdb       OUT2HB              ; FF06
                    fdb       OUT4HX              ; FF09
                    fdb       LOOKCAR             ; FF0C
                    fdb       INCAR               ; FF0F
                    fdb       IN1HA               ; FF10
                    fdb       IN2HA               ; FF13
                    fdb       IN4HX               ; FF16
                    fdb       GET1HA              ; FF19
                    fdb       GET2HA              ; FF1C
                    fdb       GET4HX              ; FF1F
                    fdb       SNDCRLF             ; FF20
                    fdb       SNDMSG              ; FF23
                    fdb       RESET               ; FF26
                    fdb       RESET               ; FF29
                    fdb       RESET               ; FF2C
                    fdb       RESET               ; FF2F

;***************************************
; VECTEUR RESET
;***************************************

                    org       $FFD6
; FFC0 a FFD5 Reserver par 68hc11
                    fdb       PSSCI               ; FFD6
                    fdb       PSSPI               ; FFD8
                    fdb       PSPAI               ; FFDA
                    fdb       PSPAO               ; FFDC
                    fdb       PSTO                ; FFDE
                    fdb       PSTOC5              ; FFE0
                    fdb       PSTOC4              ; FFE2
                    fdb       PSTOC3              ; FFE4
                    fdb       PSTOC2              ; FFE6
                    fdb       PSTOC1              ; FFE8
                    fdb       PSTIC3              ; FFEA
                    fdb       PSTIC2              ; FFEC
                    fdb       PSTIC1              ; FFEE
                    fdb       PSRTI               ; FFF0
                    fdb       PSIRQ               ; FFF2
                    fdb       PSXIRQ              ; FFF4
                    fdb       PSSWI               ; FFF6
                    fdb       PSIOT               ; FFF8
                    fdb       PSCOPF              ; FFFA
                    fdb       RESET               ; FFFC
                    fdb       RESET               ; FFFE

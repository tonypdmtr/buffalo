;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
;                  BUFFALO
; "Bit User's Fast Friendly Aid to Logical Operation"
;
; Rev 2.0 - 4/23/85 - added disassembler.
;                   - variables now PTRn and TMPn.
; Rev 2.1 - 4/29/85 - added byte erase to chgbyt routine.
; Rev 2.2 - 5/16/85 - added hooks for evb board - acia
;                     drivers, init and host routines.
;           7/8/85  - fixed dump wraparound problem.
;           7/10/85 - added evm board commands.
;                   - added fill instruction.
;           7/18/85 - added jump to EEPROM.
; Rev 2.3 - 8/22/85 - call targco to disconnect sci from host
;                     in reset routine for evb board.
;           10/3/85 - modified load for download through terminal.
; Rev 2.4 - 7/1/86  - Changed DFLOP address to fix conflicts with
;                     EEPROM.  (was at A000)
; Rev 2.5 - 9/8/86  - Modified to provide additional protection from
;                     program run-away on power down.  Also fixed bugs
;                     in MM and MOVE.  Changed to 1 stop bit from 2.
; Rev 2.6 - 9/25/86 - Modified boot routine for variable length download
;                     for use with 'HC11E8.
; Rev 3.0   1/15/87 - EEPROM programming routines consolidated into WRITE.
;                     Fill, Assem, and breakpoints will now do EEPROM.
;                   - Added compare a to $0D to WSKIP routine.
;           2/11/87 - Set up load to detect receiver error.
; Rev 3.2   7/7/87  - Add disassembly to trace.
;                   - Add entries to jump table.
;           9/20/87 - Rewrote trace to use XIRQ, added STOPAT Command
;           11/24/87- Write block protect reg for 'E9 version
;                   - Modified variable length download for use
;                       with 'E9 bootloader (XBOOT command)
; Rev 3.3   3/17/88 - Set I bit to block interrupts on Warm Start and
;                       return from CALL command.
;                   - Added EEMOD Command.
;                   - Rearranged source so that HELP command overlaps
;                       EEPROM in test mode.
;           3/24/88 - Added '+', '-', '=', '.' to MEM and ASM commands.
;                   - Added check for 16 byte boundary to MEM
;                       space sub-command.
;                   - LOAD command now puts dummy (~) command into
;                       inbuff so that any stray cr's won`t hang.
; Rev 3.4   8/15/88 - Changed WRITE subroutine so that config register
;                       gets byte erased before programmed.  The original
;                       value of config is used for EEBYTE so that config
;                       RAM value doesn't get changed in test mode.
;           8/17/88 - Fixed MOVE command so that it doesn't hang when move
;                       is done to a ROM location.
;                   - Added OFFSET command for download offset capability.
;
;****************************************************
;    Although the information contained herein,
;    as well as any information provided relative
;    thereto, has been carefully reviewed and is
;    believed accurate, Motorola assumes no
;    liability arising out of its application or
;    use, neither does it convey any license under
;    its patent rights nor the rights of others.
;****************************************************

;***************
;   EQUATES
;***************
; *Author EQU  Tony Fourcroy
RAMBS               equ       $0000               ; start of ram
REGBS               equ       $1000               ; start of registers
ROMBS               equ       $E000               ; start of rom
DSTREE              equ       $B600               ; start of eeprom
DENDEE              equ       $B7FF               ; end of eeprom
PORTE               equ       REGBS+$0A           ; port e
CFORC               equ       REGBS+$0B           ; force output compare
TCNT                equ       REGBS+$0E           ; timer count
TOC5                equ       REGBS+$1E           ; oc5 reg
TCTL1               equ       REGBS+$20           ; timer control 1
TMSK1               equ       REGBS+$22           ; timer mask 1
TFLG1               equ       REGBS+$23           ; timer flag 1
TMSK2               equ       REGBS+$24           ; timer mask 2
BAUD                equ       REGBS+$2B           ; sci baud reg
SCCR1               equ       REGBS+$2C           ; sci control1 reg
SCCR2               equ       REGBS+$2D           ; sci control2 reg
SCSR                equ       REGBS+$2E           ; sci status reg
SCDAT               equ       REGBS+$2F           ; sci data reg
BPROT               equ       REGBS+$35           ; block protect reg
OPTION              equ       REGBS+$39           ; option reg
COPRST              equ       REGBS+$3A           ; cop reset reg
PPROG               equ       REGBS+$3B           ; ee prog reg
HPRIO               equ       REGBS+$3C           ; hprio reg
CONFIG              equ       REGBS+$3F           ; config register
DFLOP               equ       $4000               ; evb d flip flop
DUART               equ       $D000               ; duart address
PORTA               equ       DUART
PORTB               equ       DUART+8
ACIA                equ       $9800               ; acia address
PROMPT              equ       '>'
BUFFLNG             equ       35
CTLA                equ       $01                 ; exit host or assembler
CTLB                equ       $02                 ; send break to host
CTLW                equ       $17                 ; wait
CTLX                equ       $18                 ; abort
DEL                 equ       $7F                 ; abort
EOT                 equ       $04                 ; end of text/table
SWI                 equ       $3F

;***************
;     RAM
;***************
                    org       $2D
;*** Buffalo ram space ***
                    rmb       20                  ; user stack area
USTACK              rmb       30                  ; monitor stack area
STACK               rmb       1
REGS                rmb       9                   ; user's pc,y,x,a,b,c
SP                  rmb       2                   ; user's sp
INBUFF              rmb       BUFFLNG             ; input buffer
ENDBUFF             equ       *
COMBUFF             rmb       8                   ; command buffer
SHFTREG             rmb       2                   ; input shift register
STREE               rmb       2                   ; eeprom start address
ENDEE               rmb       2                   ; eeprom end address
BRKTABL             rmb       8                   ; breakpoint table
AUTOLF              rmb       1                   ; auto lf flag for i/o
IODEV               rmb       1                   ; 0=sci, 1=acia, 2=duartA, 3=duartB
EXTDEV              rmb       1                   ; 0=none, 1=acia, 2=duart,
HOSTDEV             rmb       1                   ; 0=sci, 1=acia, 3=duartB
COUNT               rmb       1                   ; # characters read
CHRCNT              rmb       1                   ; # characters output on current line
PTRMEM              rmb       2                   ; current memory location
LDOFFST             rmb       2                   ; offset for download

;*** Buffalo variables - used by: ***
PTR0                rmb       2                   ; main,readbuff,incbuff,AS
PTR1                rmb       2                   ; main,BR,DU,MO,AS,EX
PTR2                rmb       2                   ; EX,DU,MO,AS
PTR3                rmb       2                   ; EX,HO,MO,AS
PTR4                rmb       2                   ; EX,AS
PTR5                rmb       2                   ; EX,AS,BOOT
PTR6                rmb       2                   ; EX,AS,BOOT
PTR7                rmb       2                   ; EX,AS
PTR8                rmb       2                   ; AS
TMP1                rmb       1                   ; main,hexbin,buffarg,termarg
TMP2                rmb       1                   ; GO,HO,AS,LOAD
TMP3                rmb       1                   ; AS,LOAD
TMP4                rmb       1                   ; TR,HO,ME,AS,LOAD
;*** Vector jump table ***
JSCI                rmb       3
JSPI                rmb       3
JPAIE               rmb       3
JPAO                rmb       3
JTOF                rmb       3
JTOC5               rmb       3
JTOC4               rmb       3
JTOC3               rmb       3
JTOC2               rmb       3
JTOC1               rmb       3
JTIC3               rmb       3
JTIC2               rmb       3
JTIC1               rmb       3
JRTI                rmb       3
JIRQ                rmb       3
JXIRQ               rmb       3
JSWI                rmb       3
JILLOP              rmb       3
JCOP                rmb       3
JCLM                rmb       3
;*****************************************************
;        USER PRGRAM AREA for 27256
;*****************************************************
;       ORG     $8000
;       FCC     'VERSION 96 V1.0'



;*****************
;
; ROM starts here
;
;*****************

                    org       ROMBS

;*****************
;**  BUFFALO - This is where Buffalo starts
;** out of reset.  All initialization is done
;** here.
;*****************
; ROUTINE TO PUT HELLO WORLD ON LCD
;
BUFFALO             lds       #STACK              ; SET STACK
                    ldaa      $1100               ; READ THE 74HC244
                    staa      $1100               ; WRITE BACK TO 74HC273
                    ldx       #$FFFF              ; GIVE LCD TIME TO RESET
JKD                 dex
                    bne       JKD
                    bsr       CLRLCD              ; SET UP LCD
                    ldx       #RESMES             ; PRINT POWER ON RESET
                    bsr       LCDTEXT
                    ldx       #$FFFF              ; DELAY
JKD1                dex
                    bne       JKD1
                    lda       #$01                ; CLEAR SCRN
                    bsr       WCTRL
                    ldx       #HELLO              ; PRINT HELLO WORD
                    bsr       LCDTEXT             ; DISPLAY A LINE OF TEXT
                    lda       #$C0                ; ADDRESS OF SECOND LINE
                    bsr       WCTRL               ; WRITE CONTROL BYTE TO LCD
                    ldx       #HELLO1             ; SEND SECOND LINE
                    bsr       LCDTEXT             ; DISPLAY A LINE OF TEXT
                    bra       BUFISIT

;***************************************************

; SET UP LCD

;***************************************************

CLRLCD              psha
                    ldaa      #$01
                    bsr       WCTRL               ; CLEAR LCD
                    ldaa      #$02
                    bsr       WCTRL               ; HOME CURSOR
                    ldaa      #$38
                    bsr       WCTRL               ; SET 8 BIT, 2 LINE, 5X7
                    ldaa      #$0C
                    bsr       WCTRL               ; DISPLAY ON, CURSOR OFF
                    ldaa      #$06
                    bsr       WCTRL               ; ENTRY MODE - INC ADDR, NO SHIFT
                    pula
                    rts

;***************************************************

; SEND A LINE OF TEXT TO LCD

; PASS  START ADDRESS OF TEXT IN X

; TEXT STRING TERMINATED BY $00 OR $04

;***************************************************

LCDTEXT             psha
LCD                 ldaa      0,X                 ; GET BYTE FROM MESSAGE TABLE
                    cmpa      #$00                ; Check for end of string
                    beq       DLCD                ; $00 OR $04
                    cmpa      #$04
                    beq       DLCD
                    bsr       WDAT                ; SEND CHARACTER TO LCD
                    inx
                    bra       LCD

DLCD                pula
                    rts

;****************************************************
; MESSAGES
;****************************************************

HELLO               fcs       'HELLO WORLD V96'
HELLO1              fcs       'HC11 MPP (C)1996'
RESMES              fcs       'POWER ON RESET'
;**************************************************
; OUPUT CONTROL BYTE TO LCD
;**************************************************
WCTRL               pshb
                    sta       $1400
WCTRLL              ldab      $1400
                    andb      #$80
                    bne       WCTRLL
                    pulb
                    rts

;***************************************************

; OUTPUT DATA BYTE TO LCD

;***************************************************

WDAT                pshb
                    sta       $1401
WDATL               ldab      $1400
                    andb      #$80
                    bne       WDATL
                    pulb
                    rts

;

;  MODIFIED TO USE ONLY SERIAL PORT ON MPP BOARD

;  CODE THAT JUMPS TO EEPROM AT START REMOVED

;

BUFISIT             ldaa      #$93
                    staa      OPTION              ; adpu, dly, irqe, cop
                    ldaa      #$00
                    staa      TMSK2               ; timer pre = %1 for trace
                    ldaa      #$00
                    staa      BPROT               ; clear 'E9 eeprom block protect
                    ldx       #DSTREE             ; set up default eeprom address range
                    stx       STREE
                    ldx       #DENDEE
                    stx       ENDEE
                    ldx       #$0000              ; set up default download offset
                    stx       LDOFFST
                    lds       #STACK              ; monitor stack pointer
                    jsr       VECINIT
                    ldx       #USTACK
                    stx       SP                  ; default user stack
                    ldaa      TCTL1
                    oraa      #$03
                    staa      TCTL1               ; force oc5 pin high for trace
                    ldaa      #$D0
                    staa      REGS+8              ; default user ccr
                    ldd       #$3F0D              ; initial command is ?
                    std       INBUFF
                    jsr       BPCLR               ; clear breakpoints
                    clr       AUTOLF
                    inc       AUTOLF              ; auto cr/lf = on

; Determine type of external comm device - none, or acia

                    clr       EXTDEV              ; default is none
;         LDAA HPRIO
;         ANDA #$20
;         BEQ  BUFF2       jump if single chip mode
;         LDAA #$03        see if external acia exists
;         STAA ACIA        master reset
;         LDAA ACIA
;         ANDA #$7F        mask irq bit from status register
;         BNE  BUFF1       jump if status reg not 0
;         LDAA #$12
;         STAA ACIA        turn on acia
;         LDAA ACIA
;         ANDA #$02
;         BEQ  BUFF1       jump if tdre not set
;         LDAA #$01
;         STAA EXTDEV      external device is acia
;         BRA  BUFF2
;
; *BUFF1    EQU  *           see if duart exists
;         LDAA  DUART+$0C  read IRQ vector register
;         CMPA  #$0F       should be out of reset
;         BNE   BUFF2
;         LDAA #$AA
;         STAA DUART+$0C   write irq vector register
;         LDAA DUART+$0C   read irq vector register
;         CMPA #$AA
;         BNE  BUFF2
;         LDAA #$02
;         STAA EXTDEV      external device is duart A

; Find terminal port - SCI or external.

BUFF2               clr       IODEV
;         JSR  TARGCO    disconnect sci for evb board
                    bsr       SIGNON              ; initialize sci
;         LDAA EXTDEV
;         BEQ  BUFF3     jump if no external device
;         STAA IODEV
;         JSR  SIGNON    initialize external device
BUFF3               clr       IODEV
                    jsr       INPUT               ; get input from sci port
                    cmpa      #$0D
                    bra       BUFF4               ; jump if cr - sci is terminal port

;         LDAA EXTDEV
;         BEQ  BUFF3     jump if no external device
;         STAA IODEV
;         JSR  INPUT     get input from external device
;         CMPA #$0D
;         BEQ  BUFF4     jump if cr - terminal found ext
;         BRA  BUFF3

SIGNON              jsr       INIT                ; initialize device
                    ldx       #MSG1               ; buffalo message
                    jsr       OUTSTRG
                    rts

; Determine where host port should be.

BUFF4               clr       HOSTDEV             ; default - host = sci port
;         LDAA IODEV
;         CMPA #$01
;         BEQ  BUFF5       default host if term = acia
;         LDAA #$03
;         STAA HOSTDEV     else host is duart port b
BUFF5               equ       *
; add switch to user routine in rom

;        LDAA    $1100
;        ANDA    #$01
;        BEQ     MAIN
;        JMP     $8500
;*****************
;**  MAIN - This module reads the user's input into
;** a buffer called INBUFF.  The first field (assumed
;** to be the command field) is then parsed into a
;** second buffer called COMBUFF.  The command table
;** is then searched for the contents of COMBUFF and
;** if found, the address of the corresponding task
;** routine is fetched from the command table.  The
;** task is then called as a subroutine so that
;** control returns back to here upon completion of
;** the task.  Buffalo expects the following format
;** for commands:
;**     <cmd>[<wsp><arg><wsp><arg>...]<cr>
;** [] implies contents optional.
;** <wsp> means whitespace character (space,comma,tab).
;** <cmd> = command string of 1-8 characters.
;** <arg> = Argument particular to the command.
;** <cr> = Carriage return signifying end of input string.
;*****************
; Prompt user
; *do
;   a=input();
;   if(a==(cntlx or del)) continue;
;   elseif(a==backspace)
;      b--;
;      if(b<0) b=0;
;   else
;      if(a==cr && buffer empty)
;         repeat last command;
;      else put a into buffer;
;         check if buffer full;
; *while(a != (cr or /)
MAIN                sei                           ; block interrupts
                    lds       #STACK              ; initialize sp every time
                    clr       AUTOLF
                    inc       AUTOLF              ; auto cr/lf = on
                    jsr       OUTCRLF
                    ldaa      #PROMPT             ; prompt user
                    jsr       OUTPUT
                    clrb
MAIN1               jsr       INCHAR              ; read terminal
                    ldx       #INBUFF
                    abx                           ; pointer into buffer
                    cmpa      #CTLX
                    beq       MAIN                ; jump if cntl X
                    cmpa      #DEL
                    beq       MAIN                ; jump if del
                    cmpa      #$08
                    bne       MAIN2               ; jump if not bckspc
                    decb
                    blt       MAIN                ; jump if buffer empty
                    bra       MAIN1

MAIN2               cmpa      #$D
                    bne       MAIN3               ; jump if not cr
                    tstb
                    beq       COMM0               ; jump if buffer empty
                    staa      ,X                  ; put a in buffer
                    bra       COMM0

MAIN3               staa      ,X                  ; put a in buffer
                    incb
                    cmpb      #BUFFLNG
                    ble       MAIN4               ; jump if not long
                    ldx       #MSG3               ; "long"
                    jsr       OUTSTRG
                    bra       MAIN

MAIN4               cmpa      #'/'
                    bne       MAIN1               ; jump if not "/"
;        *******************

;*****************
;  Parse out and evaluate the command field.
;*****************
; *Initialize

COMM0               equ       *
                    clr       TMP1                ; Enable "/" command
                    clr       SHFTREG
                    clr       SHFTREG+1
                    clrb
                    ldx       #INBUFF             ; ptrbuff[] = inbuff[]
                    stx       PTR0
                    jsr       WSKIP               ; find first char

; *while((a=readbuff) != (cr or wspace))
;     upcase(a);
;     buffptr[b] = a
;     b++
;     if (b > 8) error(too long);
;     if(a == "/")
;          if(enabled) mslash();
;          else error(command?);
;     else hexbin(a);

COMM1               equ       *
                    jsr       READBUFF            ; read from buffer
                    ldx       #COMBUFF
                    abx
                    bsr       UPCASE              ; convert to upper case
                    staa      ,X                  ; put in command buffer
                    cmpa      #$0D
                    beq       SRCH                ; jump if cr
                    jsr       WCHEK
                    beq       SRCH                ; jump if wspac
                    jsr       INCBUFF             ; move buffer pointer
                    incb
                    cmpb      #$8
                    ble       COMM2
                    ldx       #MSG3               ; "long"
                    jsr       OUTSTRG
                    bra       MAIN

COMM2               equ       *
                    cmpa      #'/'
                    bne       COMM4               ; jump if not "/"
                    tst       TMP1
                    bne       COMM3               ; jump if not enabled
                    decb
                    stab      COUNT
                    ldx       #MSLASH
                    bra       EXEC                ; execute "/"

COMM3               ldx       #MSG8               ; "command?"
                    jsr       OUTSTRG
                    jmp       MAIN

COMM4               equ       *
                    jsr       HEXBIN
                    bra       COMM1

;*****************
;   Search tables for command.  At this point,
; COMBUFF holds the command field to be executed,
; and B = # of characters in the command field.
; The command table holds the whole command name
; but only the first n characters of the command
; must match what is in COMBUFF where n is the
; number of characters entered by the user.
;*****************
; *count = b;
; *ptr1 = comtabl;
; *while(ptr1[0] != end of table)
;   ptr1 = next entry
;   for(b=1; b=count; b++)
;      if(ptr1[b] == combuff[b]) continue;
;      else error(not found);
;   execute task;
;  return();
; *return(command not found);

SRCH                stab      COUNT               ; size of command entered
                    ldx       #COMTABL            ; pointer to table
                    stx       PTR1                ; pointer to next entry
SRCH1               ldx       PTR1
                    ldy       #COMBUFF            ; pointer to command buffer
                    ldab      0,X
                    cmpb      #$FF
                    bne       SRCH2
                    ldx       #MSG2               ; "command not found"
                    jsr       OUTSTRG
                    jmp       MAIN

SRCH2               pshx                          ; compute next table entry
                    addb      #$3
                    abx
                    stx       PTR1
                    pulx
                    clrb
SRCHLP              incb                          ; match characters loop
                    ldaa      1,X                 ; read table
                    cmpa      0,Y                 ; compare to combuff
                    bne       SRCH1               ; try next entry
                    inx                           ; move pointers
                    iny
                    cmpb      COUNT
                    blt       SRCHLP              ; loop countu1 times
                    ldx       PTR1
                    dex
                    dex
                    ldx       0,X                 ; jump address from table
EXEC                jsr       0,X                 ; call task as subroutine
                    jmp       MAIN

;

;*****************

;   UTILITY SUBROUTINES - These routines

; are called by any of the task routines.

;*****************

;*****************

;  UPCASE(a) - If the contents of A is alpha,

; returns a converted to uppercase.

;*****************

UPCASE              cmpa      #'a'
                    blt       UPCASE1             ; jump if < a
                    cmpa      #'z'
                    bgt       UPCASE1             ; jump if > z
                    suba      #$20                ; convert
UPCASE1             rts

;*****************
;  BPCLR() - Clear all entries in the
; table of breakpoints.
;*****************
BPCLR               ldx       #BRKTABL
                    ldab      #8
BPCLR1              clr       0,X
                    inx
                    decb
                    bgt       BPCLR1              ; loop 8 times
                    rts

;*****************
;  RPRNT1(x) - Prints name and contents of a single
; user register. On entry X points to name of register
; in reglist.  On exit, a=register name.
;*****************
REGLIST             fcc       'PYXABCS'           ; names
                    fcb       0,2,4,6,7,8,9       ; offset
                    fcb       1,1,1,0,0,0,1       ; size
RPRNT1              ldaa      0,X
                    psha
                    pshx
                    jsr       OUTPUT              ; name
                    ldaa      #'-'
                    jsr       OUTPUT              ; dash
                    ldab      7,X                 ; contents offset
                    ldaa      14,X                ; bytesize
                    ldx       #REGS               ; address
                    abx
                    tsta
                    beq       RPRN2               ; jump if 1 byte
                    jsr       OUT1BYT             ; 2 bytes
RPRN2               jsr       OUT1BSP
                    pulx
                    pula
                    rts

;*****************
;  RPRINT() - Print the name and contents
; of all the user registers.
;*****************
RPRINT              pshx
                    ldx       #REGLIST
RPRI1               bsr       RPRNT1              ; print name
                    inx
                    cmpa      #'S'                ; s is last register
                    bne       RPRI1               ; jump if not done
                    pulx
                    rts

;*****************
;   HEXBIN(a) - Convert the ASCII character in a
; to binary and shift into shftreg.  Returns value
; in tmp1 incremented if a is not hex.
;*****************
HEXBIN              psha
                    pshb
                    pshx
                    bsr       UPCASE              ; convert to upper case
                    cmpa      #'0'
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
                    ldab      #4
HEXSHFT             asl       1,X                 ; 2 byte shift through
                    rol       0,X                 ; carry bit
                    decb
                    bgt       HEXSHFT             ; shift 4 times
                    oraa      1,X
                    staa      1,X
                    bra       HEXRTS

HEXNOT              inc       TMP1                ; indicate not hex
HEXRTS              pulx
                    pulb
                    pula
                    rts

;*****************
;  BUFFARG() - Build a hex argument from the
; contents of the input buffer. Characters are
; converted to binary and shifted into shftreg
; until a non-hex character is found.  On exit
; shftreg holds the last four digits read, count
; holds the number of digits read, ptrbuff points
; to the first non-hex character read, and A holds
; that first non-hex character.
;*****************
; *Initialize
; *while((a=readbuff()) not hex)
;     hexbin(a);
; *return();

BUFFARG             clr       TMP1                ; not hex indicator
                    clr       COUNT               ; # or digits
                    clr       SHFTREG
                    clr       SHFTREG+1
                    jsr       WSKIP
BUFFLP              jsr       READBUFF            ; read char
                    bsr       HEXBIN
                    tst       TMP1
                    bne       BUFFRTS             ; jump if not hex
                    inc       COUNT
                    jsr       INCBUFF             ; move buffer pointer
                    bra       BUFFLP

BUFFRTS             rts

;*****************
;  TERMARG() - Build a hex argument from the
; terminal.  Characters are converted to binary
; and shifted into shftreg until a non-hex character
; is found.  On exit shftreg holds the last four
; digits read, count holds the number of digits
; read, and A holds the first non-hex character.
;*****************
; *initialize
; *while((a=inchar()) == hex)
;     if(a = cntlx or del)
;          abort;
;     else
;          hexbin(a); countu1++;
; *return();

TERMARG             clr       COUNT
                    clr       SHFTREG
                    clr       SHFTREG+1
TERM0               jsr       INCHAR
                    cmpa      #CTLX
                    beq       TERM1               ; jump if controlx
                    cmpa      #DEL
                    bne       TERM2               ; jump if not delete
TERM1               jmp       MAIN                ; abort

TERM2               clr       TMP1                ; hex indicator
                    bsr       HEXBIN
                    tst       TMP1
                    bne       TERM3               ; jump if not hex
                    inc       COUNT
                    bra       TERM0

TERM3               rts

;*****************
;   CHGBYT() - If shftreg is not empty, put
; contents of shftreg at address in X.  If X
; is an address in EEPROM then program it.
;*****************
; *if(count != 0)
;   (x) = a;
CHGBYT              tst       COUNT
                    beq       CHGBYT4             ; quit if shftreg empty
                    ldaa      SHFTREG+1           ; get data into a
                    bsr       WRITE
CHGBYT4             rts


;*****************
; WRITE() - This routine is used to write the
; *contents of A to the address of X.  If the
; *address is in EEPROM, it will be programmed
; *and if it is already programmed, it will be
; *byte erased first.
;******************
; *if(X == config) then
;   byte erase config;
; *if(X is eeprom)then
;   if(not erased) then erase;
;   program (x) = A;
; *write (x) = A;
; *if((x) != A) error(rom);
WRITE               equ       *
                    cpx       #CONFIG
                    beq       WRITE0              ; jump if config
                    cpx       STREE               ; start of EE
                    blo       WRITE2              ; jump if not EE
                    cpx       ENDEE               ; end of EE
                    bhi       WRITE2              ; jump if not EE
WRITEE              pshb                          ; check if byte erased
                    ldab      0,X
                    cmpb      #$FF
                    pulb
                    beq       WRITE1              ; jump if erased
WRITE0              bsr       EEBYTE              ; byte erase
WRITE1              bsr       EEWRIT              ; byte program
WRITE2              staa      0,X                 ; write for non EE
                    cmpa      0,X
                    beq       WRITE3              ; jump if write ok
                    pshx
                    ldx       #MSG6               ; "rom"
                    jsr       OUTSTRG
                    pulx
WRITE3              rts


;*****************
;   EEWRIT(), EEBYTE(), EEBULK() -
; These routines are used to program and eeprom
; *locations.  eewrite programs the address in X with
; *the value in A, eebyte does a byte address at X,
; *and eebulk does a bulk of eeprom.  Whether eebulk
; *erases the config or not depends on the address it
; *receives in X.
;****************
EEWRIT              equ       *                   ; program one byte at x
                    pshb
                    ldab      #$02
                    stab      PPROG
                    staa      0,X
                    ldab      #$03
                    bra       EEPROG

;***

EEBYTE              equ       *                   ; byte erase address x
                    pshb
                    ldab      #$16
                    stab      PPROG
                    ldab      #$FF
                    stab      0,X
                    ldab      #$17
                    bra       EEPROG

;***

EEBULK              equ       *                   ; bulk erase eeprom
                    pshb
                    ldab      #$06
                    stab      PPROG
                    staa      0,X                 ; erase config or not ...
                    ldab      #$07                ; ... depends on X addr
EEPROG              bne       ACL1
                    clrb                          ; fail safe
ACL1                stab      PPROG
                    pulb
;***
DLY10MS             equ       *                   ; delay 10ms at E = 2MHz
                    pshx
                    ldx       #$0D06
DLYLP               dex
                    bne       DLYLP
                    pulx
                    clr       PPROG
                    rts


;*****************
;  READBUFF() -  Read the character in INBUFF
; pointed at by ptrbuff into A.  Returns ptrbuff
; unchanged.
;*****************
READBUFF            pshx
                    ldx       PTR0
                    ldaa      0,X
                    pulx
                    rts

;*****************
;  INCBUFF(), DECBUFF() - Increment or decrement
; ptrbuff.
;*****************
INCBUFF             pshx
                    ldx       PTR0
                    inx
                    bra       INCDEC

DECBUFF             pshx
                    ldx       PTR0
                    dex
INCDEC              stx       PTR0
                    pulx
                    rts

;*****************
;  WSKIP() - Read from the INBUFF until a
; non whitespace (space, comma, tab) character
; is found.  Returns ptrbuff pointing to the
; first non-whitespace character and a holds
; that character.  WSKIP also compares a to
; $0D (CR) and cond codes indicating the
; results of that compare.
;*****************
WSKIP               bsr       READBUFF            ; read character
                    bsr       WCHEK
                    bne       WSKIP1              ; jump if not wspc
                    bsr       INCBUFF             ; move pointer
                    bra       WSKIP               ; loop

WSKIP1              cmpa      #$0D
                    rts

;*****************
;  WCHEK(a) - Returns z=1 if a holds a
; whitespace character, else z=0.
;*****************
WCHEK               cmpa      #$2C                ; comma
                    beq       WCHEK1
                    cmpa      #$20                ; space
                    beq       WCHEK1
                    cmpa      #$09                ; tab
WCHEK1              rts

;*****************
;   DCHEK(a) - Returns Z=1 if a = whitespace
; or carriage return.  Else returns z=0.
;*****************
DCHEK               bsr       WCHEK
                    beq       DCHEK1              ; jump if whitespace
                    cmpa      #$0D
DCHEK1              rts

;*****************
;  CHKABRT() - Checks for a control x or delete
; from the terminal.  If found, the stack is
; reset and the control is transferred to main.
; Note that this is an abnormal termination.
;   If the input from the terminal is a control W
; then this routine keeps waiting until any other
; character is read.
;*****************
; *a=input();
; *if(a=cntl w) wait until any other key;
; *if(a = cntl x or del) abort;

CHKABRT             bsr       INPUT
                    beq       CHK4                ; jump if no input
                    cmpa      #CTLW
                    bne       CHK2                ; jump in not cntlw
CHKABRT1            bsr       INPUT
                    beq       CHKABRT1            ; jump if no input
CHK2                cmpa      #DEL
                    beq       CHK3                ; jump if delete
                    cmpa      #CTLX
                    beq       CHK3                ; jump if control x
                    cmpa      #CTLA
                    bne       CHK4                ; jump not control a
CHK3                jmp       MAIN                ; abort

CHK4                rts                           ; return

;***********************
;  HOSTCO - connect sci to host for evb board.
;  TARGCO - connect sci to target for evb board.
;***********************
; *HOSTCO   PSHA
;         LDAA #$01
;         STAA DFLOP     send 1 to d-flop
;         PULA
;         RTS

; *TARGCO   PSHA
;         LDAA #$00
;         STAA DFLOP     send 0 to d-flop
;         PULA
;         RTS

;
;**********
;
;     VECINIT - This routine checks for
;        vectors in the RAM table.  All
;        uninitialized vectors are programmed
;        to JMP STOPIT
;
;**********
;
VECINIT             ldx       #JSCI               ; Point to First RAM Vector
                    ldy       #STOPIT             ; Pointer to STOPIT routine
                    ldd       #$7E03              ; A=JMP opcode; B=offset
VECLOOP             cmpa      0,X
                    beq       VECNEXT             ; If vector already in
                    staa      0,X                 ; install JMP
                    sty       1,X                 ; to STOPIT routine
VECNEXT             abx                           ; Add 3 to point at next vector
                    cpx       #JCLM+3             ; Done?
                    bne       VECLOOP             ; If not, continue loop
                    rts

;

STOPIT              ldaa      #$50                ; Stop-enable; IRQ, XIRQ-Off
                    tap
                    stop                          ; You are lost! Shut down
                    bra       STOPIT              ; In case continue by XIRQ

;**********
;
;   I/O MODULE
;     Communications with the outside world.
; 3 I/O routines (INIT, INPUT, and OUTPUT) call
; drivers specified by IODEV (0=SCI, 1=ACIA,
; 2=DUARTA, 3=DUARTB).
;
;**********
;   INIT() - Initialize device specified by iodev.
;*********
;
INIT                equ       *
                    psha                          ; save registers
                    pshx
;         LDAA IODEV
;         CMPA #$00
;         BNE  INIT1     jump not sci
                    jsr       ONSCI               ; initialize sci
;         BRA  INIT4
; *INIT1    CMPA #$01
;         BNE  INIT2     jump not acia
;         JSR  ONACIA    initialize acia
;         BRA  INIT4
; *INIT2    LDX  #PORTA
;         CMPA #$02
;         BEQ  INIT3     jump duart a
;         LDX  #PORTB
; *INIT3    JSR  ONUART    initialize duart
INIT4               pulx                          ; restore registers
                    pula
                    rts

;**********
;  INPUT() - Read device. Returns a=char or 0.
;    This routine also disarms the cop.
;**********
INPUT               equ       *
                    pshx
                    ldaa      #$55                ; reset cop
                    staa      COPRST
                    ldaa      #$AA
                    staa      COPRST
                    ldaa      IODEV
                    bne       INPUT1              ; jump not sci
                    jsr       INSCI               ; read sci
                    bra       INPUT4

INPUT1              cmpa      #$01
                    bne       INPUT2              ; jump not acia
                    jsr       INACIA              ; read acia
                    bra       INPUT4

INPUT2              ldx       #PORTA
                    cmpa      #$02
                    beq       INPUT3              ; jump if duart a
                    ldx       #PORTB
INPUT3              bsr       INUART              ; read uart
INPUT4              pulx
                    rts

;**********
;   OUTPUT() - Output character in A.
; chrcnt indicates the current column on the
; *output display.  It is incremented every time
; *a character is outputted, and cleared whenever
; *the subroutine outcrlf is called.
;**********

OUTPUT              equ       *
                    psha                          ; save registers
                    pshb
                    pshx
                    ldab      IODEV
                    bne       OUTPUT1             ; jump not sci
                    jsr       OUTSCI              ; write sci
                    bra       OUTPUT4

OUTPUT1             cmpb      #$01
                    bne       OUTPUT2             ; jump not acia
                    jsr       OUTACIA             ; write acia
                    bra       OUTPUT4

OUTPUT2             ldx       #PORTA
                    cmpb      #$02
                    beq       OUTPUT3             ; jump if duart a
                    ldx       #PORTB
OUTPUT3             bsr       OUTUART             ; write uart
OUTPUT4             pulx
                    pulb
                    pula
                    inc       CHRCNT              ; increment column count
                    rts

;**********
;   ONUART(port) - Initialize a duart port.
; Sets duart to internal clock, divide by 16,
; 8 data + 1 stop bits.
;**********

ONUART              ldaa      #$22
                    staa      2,X                 ; reset receiver
                    ldaa      #$38
                    staa      2,X                 ; reset transmitter
                    ldaa      #$40
                    staa      2,X                 ; reset error status
                    ldaa      #$10
                    staa      2,X                 ; reset pointer
                    ldaa      #$00
                    staa      DUART+4             ; clock source
                    ldaa      #$00
                    staa      DUART+5             ; interrupt mask
                    ldaa      #$13
                    staa      0,X                 ; 8 data, no parity
                    ldaa      #$07
                    staa      0,X                 ; 1 stop bits
                    ldaa      #$BB                ; baud rate (9600)
                    staa      1,X                 ; tx and rcv baud rate
                    ldaa      #$05
                    staa      2,X                 ; enable tx and rcv
                    rts

;**********
;   INUART(port) - Check duart for any input.
;**********
INUART              ldaa      1,X                 ; read status
                    anda      #$01                ; check rxrdy
                    beq       INUART1             ; jump if no data
                    ldaa      3,X                 ; read data
                    anda      #$7F                ; mask parity
INUART1             rts

;**********
;   OUTUART(port) - Output the character in a.
;        if autolf=1, transmits cr or lf as crlf.
;**********
OUTUART             tst       AUTOLF
                    beq       OUTUART2            ; jump if no autolf
                    bsr       OUTUART2
                    cmpa      #$0D
                    bne       OUTUART1
                    ldaa      #$0A                ; if cr, output lf
                    bra       OUTUART2

OUTUART1            cmpa      #$0A
                    bne       OUTUART3
                    ldaa      #$0D                ; if lf, output cr
OUTUART2            ldab      1,X                 ; check status
                    andb      #$4
                    beq       OUTUART2            ; loop until tdre=1
                    anda      #$7F                ; mask parity
                    staa      3,X                 ; send character
OUTUART3            rts

;**********
;   ONSCI() - Initialize the SCI for 9600
;                 baud at 8 MHz Extal.
;**********
ONSCI               ldaa      #$30
                    staa      BAUD                ; baud register
                    ldaa      #$00
                    staa      SCCR1
                    ldaa      #$0C
                    staa      SCCR2               ; enable
                    rts

;**********
;   INSCI() - Read from SCI.  Return a=char or 0.
;**********
INSCI               ldaa      SCSR                ; read status reg
                    anda      #$20                ; check rdrf
                    beq       INSCI1              ; jump if no data
                    ldaa      SCDAT               ; read data
                    anda      #$7F                ; mask parity
INSCI1              rts

;**********
;  OUTSCI() - Output A to sci. IF autolf = 1,
;               cr and lf sent as crlf.
;**********
OUTSCI              tst       AUTOLF
                    beq       OUTSCI2             ; jump if autolf=0
                    bsr       OUTSCI2
                    cmpa      #$0D
                    bne       OUTSCI1
                    ldaa      #$0A                ; if cr, send lf
                    bra       OUTSCI2

OUTSCI1             cmpa      #$0A
                    bne       OUTSCI3
                    ldaa      #$0D                ; if lf, send cr
OUTSCI2             ldab      SCSR                ; read status
                    bitb      #$80
                    beq       OUTSCI2             ; loop until tdre=1
                    anda      #$7F                ; mask parity
                    staa      SCDAT               ; send character
OUTSCI3             rts

;**********
;   ONACIA - Initialize the ACIA for
; 8 data bits, 1 stop bit, divide by 64 clock.
;**********
ONACIA              ldx       #ACIA
                    ldaa      #$03
                    staa      0,X                 ; master reset
                    ldaa      #$16
                    staa      0,X                 ; setup
                    rts

;**********
;   INACIA - Read from the ACIA, Return a=char or 0.
; Tmp3 is used to flag overrun or framing error.
;**********
INACIA              ldx       #ACIA
                    ldaa      0,X                 ; read status register
                    psha
                    anda      #$30                ; check ov, fe
                    pula
                    beq       INACIA1             ; jump - no error
                    ldaa      #$01
                    staa      TMP3                ; flag reciever error
                    bra       INACIA2             ; read data to clear status

INACIA1             anda      #$01                ; check rdrf
                    beq       INACIA3             ; jump if no data
INACIA2             ldaa      1,X                 ; read data
                    anda      #$7F                ; mask parity
INACIA3             rts

;**********
;  OUTACIA - Output A to acia. IF autolf = 1,
;               cr or lf sent as crlf.
;**********
OUTACIA             bsr       OUTACIA3            ; output char
                    tst       AUTOLF
                    beq       OUTACIA2            ; jump no autolf
                    cmpa      #$0D
                    bne       OUTACIA1
                    ldaa      #$0A
                    bsr       OUTACIA3            ; if cr, output lf
                    bra       OUTACIA2

OUTACIA1            cmpa      #$0A
                    bne       OUTACIA2
                    ldaa      #$0D
                    bsr       OUTACIA3            ; if lf, output cr
OUTACIA2            rts

OUTACIA3            ldx       #ACIA
                    ldab      0,X
                    bitb      #$2
                    beq       OUTACIA3            ; loop until tdre
                    anda      #$7F                ; mask parity
                    staa      1,X                 ; output
                    rts

;

;        Space for modifying OUTACIA routine

;

                    fdb       $FFFF,$FFFF,$FFFF,$FFFF
;*******************************
;*** I/O UTILITY SUBROUTINES ***
;***These subroutines perform the neccesary
; data I/O operations.
; OUTLHLF-Convert left 4 bits of A from binary
;            to ASCII and output.
; OUTRHLF-Convert right 4 bits of A from binary
;            to ASCII and output.
; OUT1BYT-Convert byte addresed by X from binary
;           to ASCII and output.
; OUT1BSP-Convert byte addressed by X from binary
;           to ASCII and output followed by a space.
; OUT2BSP-Convert 2 bytes addressed by X from binary
;            to ASCII and  output followed by a space.
; OUTSPAC-Output a space.
;
; OUTCRLF-Output a line feed and carriage return.
;
; OUTSTRG-Output the string of ASCII bytes addressed
;            by X until $04.
; OUTA-Output the ASCII character in A.
;
; TABTO-Output spaces until column 20 is reached.
;
; INCHAR-Input to A and echo one character.  Loops
;            until character read.
;        *******************

;**********
;  OUTRHLF(), OUTLHLF(), OUTA()
; *Convert A from binary to ASCII and output.
; *Contents of A are destroyed..
;**********
OUTLHLF             lsra                          ; shift data to right
                    lsra
                    lsra
                    lsra
OUTRHLF             anda      #$0F                ; mask top half
                    adda      #$30                ; convert to ascii
                    cmpa      #$39
                    ble       OUTA                ; jump if 0-9
                    adda      #$07                ; convert to hex A-F
OUTA                jsr       OUTPUT              ; output character
                    rts

;**********
;  OUT1BYT(x) - Convert the byte at X to two
; ASCII characters and output. Return X pointing
; to next byte.
;**********
OUT1BYT             psha
                    ldaa      0,X                 ; get data in a
                    psha                          ; save copy
                    bsr       OUTLHLF             ; output left half
                    pula                          ; retrieve copy
                    bsr       OUTRHLF             ; output right half
                    pula
                    inx
                    rts

;**********
;  OUT1BSP(x), OUT2BSP(x) - Output 1 or 2 bytes
; at x followed by a space.  Returns x pointing to
; next byte.
;**********
OUT2BSP             bsr       OUT1BYT             ; do first byte
OUT1BSP             bsr       OUT1BYT             ; do next byte
OUTSPAC             ldaa      #$20                ; output a space
                    jsr       OUTPUT
                    rts

;**********
;  OUTCRLF() - Output a Carriage return and
; a line feed.  Returns a = cr.
;**********
OUTCRLF             ldaa      #$0D                ; cr
                    jsr       OUTPUT              ; output a
                    ldaa      #$00
                    jsr       OUTPUT              ; output padding
                    ldaa      #$0D
                    clr       CHRCNT              ; zero the column counter
                    rts

;**********
;  OUTSTRG(x) - Output string of ASCII bytes
; starting at x until end of text ($04).  Can
; be paused by control w (any char restarts).
;**********
OUTSTRG             bsr       OUTCRLF
OUTSTRG0            psha
OUTSTRG1            ldaa      0,X                 ; read char into a
                    cmpa      #EOT
                    beq       OUTSTRG3            ; jump if eot
                    jsr       OUTPUT              ; output character
                    inx
                    jsr       INPUT
                    beq       OUTSTRG1            ; jump if no input
                    cmpa      #CTLW
                    bne       OUTSTRG1            ; jump if not cntlw
OUTSTRG2            jsr       INPUT
                    beq       OUTSTRG2            ; jump if any input
                    bra       OUTSTRG1

OUTSTRG3            pula
                    rts


;*********
;  TABTO() - move cursor over to column 20.
; *while(chrcnt < 16) outspac.
TABTO               equ       *
                    psha
TABTOLP             bsr       OUTSPAC
                    ldaa      CHRCNT
                    cmpa      #20
                    ble       TABTOLP
                    pula
                    rts

;**********
;  INCHAR() - Reads input until character sent.
;    Echoes char and returns with a = char.
INCHAR              jsr       INPUT
                    tsta
                    beq       INCHAR              ; jump if no input
                    jsr       OUTPUT              ; echo
                    rts

;*********************
;*** COMMAND TABLE ***
COMTABL             fcb       5
                    fcc       'ASSEM'
                    fdb       ASSEM
                    fcb       5
                    fcc       'BREAK'
                    fdb       BREAK
                    fcb       4
                    fcc       'BULK'
                    fdb       BULK
                    fcb       7
                    fcc       'BULKALL'
                    fdb       BULKALL
                    fcb       4
                    fcc       'CALL'
                    fdb       CALL
                    fcb       4
                    fcc       'DUMP'
                    fdb       DUMP
                    fcb       5
                    fcc       'EEMOD'
                    fdb       EEMOD
                    fcb       4
                    fcc       'FILL'
                    fdb       FILL
                    fcb       2
                    fcc       'GO'
                    fdb       GO
                    fcb       4
                    fcc       'HELP'
                    fdb       HELP
                    fcb       4
                    fcc       'HOST'
                    fdb       HOST
                    fcb       4
                    fcc       'LOAD'
                    fdb       LOAD
                    fcb       6                   ; LENGTH OF COMMAND
                    fcc       'MEMORY'            ; ASCII COMMAND
                    fdb       MEMORY              ; COMMAND ADDRESS
                    fcb       4
                    fcc       'MOVE'
                    fdb       MOVE
                    fcb       6
                    fcc       'OFFSET'
                    fdb       OFFSET
                    fcb       7
                    fcc       'PROCEED'
                    fdb       PROCEED
                    fcb       8
                    fcc       'REGISTER'
                    fdb       REGISTER
                    fcb       6
                    fcc       'STOPAT'
                    fdb       STOPAT
                    fcb       5
                    fcc       'TRACE'
                    fdb       TRACE
                    fcb       6
                    fcc       'VERIFY'
                    fdb       VERIFY
                    fcb       1
                    fcc       '?'                 ; initial command
                    fdb       HELP
                    fcb       5
                    fcc       'XBOOT'
                    fdb       BOOT
                    fcb       1                   ; dummy command for load
                    fcc       '~'
                    fdb       TILDE
;
;*** Command names for evm compatability ***
;
                    fcb       3
                    fcc       'ASM'
                    fdb       ASSEM
                    fcb       2
                    fcc       'BF'
                    fdb       FILL
                    fcb       4
                    fcc       'COPY'
                    fdb       MOVE
                    fcb       5
                    fcc       'ERASE'
                    fdb       BULK
                    fcb       2
                    fcc       'MD'
                    fdb       DUMP
                    fcb       2
                    fcc       'MM'
                    fdb       MEMORY
                    fcb       2
                    fcc       'RD'
                    fdb       REGISTER
                    fcb       2
                    fcc       'RM'
                    fdb       REGISTER
                    fcb       4
                    fcc       'READ'
                    fdb       MOVE
                    fcb       2
                    fcc       'TM'
                    fdb       HOST
                    fcb       4
                    fcc       'TEST'
                    fdb       EVBTEST
                    fcb       $FF

;*******************
;*** TEXT TABLES ***

MSG1                fcc       'BUFFALO 3.4 (ext) - Bit User Fast Friendly Aid to Logical Operation',EOT
MSG2                fcc       'What?',EOT
MSG3                fcc       'Too Long',EOT
MSG4                fcc       'Full',EOT
MSG5                fcc       'Op- ',EOT
MSG6                fcc       'rom-',EOT
MSG8                fcc       'Command?',EOT
MSG9                fcc       'Bad argument',EOT
MSG10               fcc       'No host port',EOT
MSG11               fcc       'done',EOT
MSG12               fcc       'chksum error',EOT
MSG13               fcc       'error addr ',EOT
MSG14               fcc       'rcvr error',EOT

;**********
;   break [-][<addr>] . . .
; Modifies the breakpoint table.  More than
; one argument can be entered on the command
; line but the table will hold only 4 entries.
; 4 types of arguments are implied above:
; break           Prints table contents.
; break <addr>    Inserts <addr>.
; break -<addr>   Deletes <addr>.
; break -         Clears all entries.
;**********
; while 1
;     a = wskip();
;     switch(a)
;          case(cr):
;               bprint(); return;

BREAK               jsr       WSKIP
                    bne       BRKDEL              ; jump if not cr
                    jsr       BPRINT              ; print table
                    rts

;          case("-"):
;               incbuff(); readbuff();
;               if(dchek(a))          /* look for wspac or cr */
;                    bpclr();
;                    breaksw;
;               a = buffarg();
;               if( !dchek(a) ) return(bad argument);
;               b = bpsrch();
;               if(b >= 0)
;                    brktabl[b] = 0;
;               breaksw;

BRKDEL              cmpa      #'-'
                    bne       BRKDEF              ; jump if not -
                    jsr       INCBUFF
                    jsr       READBUFF
                    jsr       DCHEK
                    bne       BRKDEL1             ; jump if not delimeter
                    jsr       BPCLR               ; clear table
                    bra       BREAK               ; do next argument

BRKDEL1             jsr       BUFFARG             ; get address to delete
                    jsr       DCHEK
                    beq       BRKDEL2             ; jump if delimeter
                    ldx       #MSG9               ; "bad argument"
                    jsr       OUTSTRG
                    rts

BRKDEL2             bsr       BPSRCH              ; look for addr in table
                    tstb
                    bmi       BRKDEL3             ; jump if not found
                    ldx       #BRKTABL
                    abx
                    clr       0,X                 ; clear entry
                    clr       1,X
BRKDEL3             bra       BREAK               ; do next argument

;          default:
;               a = buffarg();
;               if( !dchek(a) ) return(bad argument);
;               b = bpsrch();
;               if(b < 0)            /* not already in table */
;                    x = shftreg;
;                    shftreg = 0;
;                    a = x[0]; x[0] = $3F
;                    b = x[0]; x[0] = a;
;                    if(b != $3F) return(rom);
;                    b = bpsrch();   /* look for hole */
;                    if(b >= 0) return(table full);
;                    brktabl[b] = x;
;               breaksw;

BRKDEF              jsr       BUFFARG             ; get argument
                    jsr       DCHEK
                    beq       BRKDEF1             ; jump if delimiter
                    ldx       #MSG9               ; "bad argument"
                    jsr       OUTSTRG
                    rts

BRKDEF1             bsr       BPSRCH              ; look for entry in table
                    tstb
                    bge       BREAK               ; jump if already in table

                    ldx       SHFTREG             ; x = new entry addr
                    ldaa      0,X                 ; save original contents
                    psha
                    ldaa      #SWI
                    jsr       WRITE               ; write to entry addr
                    ldab      0,X                 ; read back
                    pula
                    jsr       WRITE               ; restore original
                    cmpb      #SWI
                    beq       BRKDEF2             ; jump if writes ok
                    stx       PTR1                ; save address
                    ldx       #PTR1
                    jsr       OUT2BSP             ; print address
                    bsr       BPRINT
                    rts

BRKDEF2             clr       SHFTREG
                    clr       SHFTREG+1
                    pshx
                    bsr       BPSRCH              ; look for 0 entry
                    pulx
                    tstb
                    bpl       BRKDEF3             ; jump if table not full
                    ldx       #MSG4               ; "full"
                    jsr       OUTSTRG
                    bsr       BPRINT
                    rts

BRKDEF3             ldy       #BRKTABL
                    aby
                    stx       0,Y                 ; put new entry in
                    jmp       BREAK               ; do next argument

;**********
;   bprint() - print the contents of the table.
;**********
BPRINT              jsr       OUTCRLF
                    ldx       #BRKTABL
                    ldab      #4
BPRINT1             jsr       OUT2BSP
                    decb
                    bgt       BPRINT1             ; loop 4 times
                    rts

;**********
;   bpsrch() - search table for address in
; shftreg. Returns b = index to entry or
; b = -1 if not found.
;**********
; *for(b=0; b=6; b=+2)
;     x[] = brktabl + b;
;     if(x[0] = shftreg)
;          return(b);
; *return(-1);

BPSRCH              clrb
BPSRCH1             ldx       #BRKTABL
                    abx
                    ldx       0,X                 ; get table entry
                    cpx       SHFTREG
                    bne       BPSRCH2             ; jump if no match
                    rts

BPSRCH2             incb
                    incb
                    cmpb      #$6
                    ble       BPSRCH1             ; loop 4 times
                    ldab      #$FF
                    rts


;**********
;  bulk  - Bulk erase the eeprom not config.
; bulkall - Bulk erase eeprom and config.
;*********
BULK                equ       *
                    ldx       STREE
                    bra       BULK1

BULKALL             ldx       #CONFIG
BULK1               ldaa      #$FF
                    jsr       EEBULK
                    rts



;**********
;  dump [<addr1> [<addr2>]]  - Dump memory
; in 16 byte lines from <addr1> to <addr2>.
;   Default starting address is "current
; location" and default number of lines is 8.
;**********
; *ptr1 = ptrmem;        /* default start address */
; *ptr2 = ptr1 + $80;    /* default end address */
; *a = wskip();
; *if(a != cr)
;     a = buffarg();
;     if(countu1 = 0) return(bad argument);
;     if( !dchek(a) ) return(bad argument);
;     ptr1 = shftreg;
;     ptr2 = ptr1 + $80;  /* default end address */
;     a = wskip();
;     if(a != cr)
;          a = buffarg();
;          if(countu1 = 0) return(bad argument);
;          a = wskip();
;          if(a != cr) return(bad argument);
;          ptr2 = shftreg;

DUMP                ldx       PTRMEM              ; current location
                    stx       PTR1                ; default start
                    ldab      #$80
                    abx
                    stx       PTR2                ; default end
                    jsr       WSKIP
                    beq       DUMP1               ; jump - no arguments
                    jsr       BUFFARG             ; read argument
                    tst       COUNT
                    beq       DUMPERR             ; jump if no argument
                    jsr       DCHEK
                    bne       DUMPERR             ; jump if delimiter
                    ldx       SHFTREG
                    stx       PTR1
                    ldab      #$80
                    abx
                    stx       PTR2                ; default end address
                    jsr       WSKIP
                    beq       DUMP1               ; jump - 1 argument
                    jsr       BUFFARG             ; read argument
                    tst       COUNT
                    beq       DUMPERR             ; jump if no argument
                    jsr       WSKIP
                    bne       DUMPERR             ; jump if not cr
                    ldx       SHFTREG
                    stx       PTR2
                    bra       DUMP1               ; jump - 2 arguments

DUMPERR             ldx       #MSG9               ; "bad argument"
                    jsr       OUTSTRG
                    rts

; *ptrmem = ptr1;
; *ptr1 = ptr1 & $fff0;

DUMP1               ldd       PTR1
                    std       PTRMEM              ; new current location
                    andb      #$F0
                    std       PTR1                ; start dump at 16 byte boundary

;*** dump loop starts here ***
; *do:
;     output address of first byte;

DUMPLP              jsr       OUTCRLF
                    ldx       #PTR1
                    jsr       OUT2BSP             ; first address

;     x = ptr1;
;     for(b=0; b=16; b++)
;          output contents;

                    ldx       PTR1                ; base address
                    clrb                          ; loop counter
DUMPDAT             jsr       OUT1BSP             ; hex value loop
                    incb
                    cmpb      #$10
                    blt       DUMPDAT             ; loop 16 times

;     x = ptr1;
;     for(b=0; b=16; b++)
;          a = x[b];
;          if($7A < a < $20)  a = $20;
;          output ascii contents;

                    clrb                          ; loop counter
DUMPASC             ldx       PTR1                ; base address
                    abx
                    ldaa      ,X                  ; ascii value loop
                    cmpa      #$20
                    blo       DUMP3               ; jump if non printable
                    cmpa      #$7A
                    bls       DUMP4               ; jump if printable
DUMP3               ldaa      #$20                ; space for non printables
DUMP4               jsr       OUTPUT              ; output ascii value
                    incb
                    cmpb      #$10
                    blt       DUMPASC             ; loop 16 times

;     chkabrt();
;     ptr1 = ptr1 + $10;
; *while(ptr1 <= ptr2);
; *return;

                    jsr       CHKABRT             ; check abort or wait
                    ldd       PTR1
                    addd      #$10                ; point to next 16 byte bound
                    std       PTR1                ; update ptr1
                    cpd       PTR2
                    bhi       DUMP5               ; quit if ptr1 > ptr2
                    cpd       #$00                ; check wraparound at $ffff
                    bne       DUMPLP              ; jump - no wraparound
                    ldd       PTR2
                    cpd       #$FFF0
                    blo       DUMPLP              ; upper bound not at top
DUMP5               rts                           ; quit



;**********
;   eemod [<addr1> [<addr2>]]
; Modifies the eeprom address range.
;  EEMOD                 -show ee address range
;  EEMOD <addr1>         -set range to addr1 -> addr1+2k
;  EEMOD <addr1> <addr2> -set range to addr1 -> addr2
;**********
; *if(<addr1>)
;    stree = addr1;
;    endee = addr1 + 2k bytes;
; *if(<addr2>)
;    endee = addr2;
; *print(stree,endee);
EEMOD               equ       *
                    jsr       WSKIP
                    beq       EEMOD2              ; jump - no arguments
                    jsr       BUFFARG             ; read argument
                    tst       COUNT
                    beq       EEMODER             ; jump if no argument
                    jsr       DCHEK
                    bne       EEMODER             ; jump if no delimeter
                    ldd       SHFTREG
                    std       PTR1
                    addd      #$07FF              ; add 2k bytes to stree
                    std       PTR2                ; default endee address
                    jsr       WSKIP
                    beq       EEMOD1              ; jump - 1 argument
                    jsr       BUFFARG             ; read argument
                    tst       COUNT
                    beq       EEMODER             ; jump if no argument
                    jsr       WSKIP
                    bne       EEMODER             ; jump if not cr
                    ldx       SHFTREG
                    stx       PTR2
EEMOD1              ldx       PTR1
                    stx       STREE               ; new stree address
                    ldx       PTR2
                    stx       ENDEE               ; new endee address
EEMOD2              jsr       OUTCRLF             ; display ee range
                    ldx       #STREE
                    jsr       OUT2BSP
                    ldx       #ENDEE
                    jsr       OUT2BSP
                    rts

EEMODER             ldx       #MSG9               ; "bad argument"
                    jsr       OUTSTRG
                    rts




;**********
;  fill <addr1> <addr2> [<data>]  - Block fill
; *memory from addr1 to addr2 with data.  Data
; *defaults to $FF.
;**********
; *get addr1 and addr2
FILL                equ       *
                    jsr       WSKIP
                    jsr       BUFFARG
                    tst       COUNT
                    beq       FILLERR             ; jump if no argument
                    jsr       WCHEK
                    bne       FILLERR             ; jump if bad argument
                    ldx       SHFTREG
                    stx       PTR1                ; address1
                    jsr       WSKIP
                    jsr       BUFFARG
                    tst       COUNT
                    beq       FILLERR             ; jump if no argument
                    jsr       DCHEK
                    bne       FILLERR             ; jump if bad argument
                    ldx       SHFTREG
                    stx       PTR2                ; address2

; *Get data if it exists
                    ldaa      #$FF
                    staa      TMP2                ; default data
                    jsr       WSKIP
                    beq       FILL1               ; jump if default data
                    jsr       BUFFARG
                    tst       COUNT
                    beq       FILLERR             ; jump if no argument
                    jsr       WSKIP
                    bne       FILLERR             ; jump if bad argument
                    ldaa      SHFTREG+1
                    staa      TMP2

; *while(ptr1 <= ptr2)
;   *ptr1 = data
;   if(*ptr1 != data) abort

FILL1               equ       *
                    jsr       CHKABRT             ; check for abort
                    ldx       PTR1                ; starting address
                    ldaa      TMP2                ; data
                    jsr       WRITE               ; write the data to x
                    cmpa      0,X
                    bne       FILLBAD             ; jump if no write
                    cpx       PTR2
                    beq       FILL2               ; quit yet?
                    inx
                    stx       PTR1
                    bra       FILL1               ; loop

FILL2               rts

FILLERR             ldx       #MSG9               ; "bad argument"
                    jsr       OUTSTRG
                    rts

FILLBAD             equ       *
                    ldx       #PTR1               ; output bad address
                    jsr       OUT2BSP
                    rts



;*******************************************
;   MEMORY [<addr>]
;   [<addr>]/
; Opens memory and allows user to modify the
; *contents at <addr> or the last opened location.
;    Subcommands:
; [<data>]<cr>       - Close current location and exit.
; [<data>]<lf><+>    - Close current and open next.
; [<data>]<^><-><bs> - Close current and open previous.
; [<data>]<sp>       - Close current and open next.
; [<data>]</><=>     - Reopen current location.
;     The contents of the current location is only
;  changed if valid data is entered before each
;  subcommand.
; [<addr>]O - Compute relative offset from current
;     location to <addr>.  The current location must
;     be the address of the offset byte.
;**********
; *a = wskip();
; *if(a != cr)
;     a = buffarg();
;     if(a != cr) return(bad argument);
;     if(countu1 != 0) ptrmem[] = shftreg;

MEMORY              jsr       WSKIP
                    beq       MEM1                ; jump if cr
                    jsr       BUFFARG
                    jsr       WSKIP
                    beq       MSLASH              ; jump if cr
                    ldx       #MSG9               ; "bad argument"
                    jsr       OUTSTRG
                    rts

MSLASH              tst       COUNT
                    beq       MEM1                ; jump if no argument
                    ldx       SHFTREG
                    stx       PTRMEM              ; update "current location"

;**********
; Subcommands
;**********
; *outcrlf();
; *out2bsp(ptrmem[]);
; *out1bsp(ptrmem[0]);

MEM1                jsr       OUTCRLF
MEM2                ldx       #PTRMEM
                    jsr       OUT2BSP             ; output address
MEM3                ldx       PTRMEM
                    jsr       OUT1BSP             ; output contents
                    clr       SHFTREG
                    clr       SHFTREG+1
; *while 1
; *a = termarg();
;     switch(a)
;          case(space):
;             chgbyt();
;             ptrmem[]++;
;             if(ptrmem%16 == 0) start new line;
;          case(linefeed | +):
;             chgbyt();
;             ptrmem[]++;
;          case(up arrow | backspace | -):
;               chgbyt();
;               ptrmem[]--;
;          case('/' | '='):
;               chgbyt();
;               outcrlf();
;          case(O):
;               d = ptrmem[0] - (shftreg);
;               if($80 < d < $ff81)
;                    print(out of range);
;               countt1 = d-1;
;               out1bsp(countt1);
;          case(carriage return):
;               chgbyt();
;               return;
;          default: return(command?)

MEM4                jsr       TERMARG
                    jsr       UPCASE
                    ldx       PTRMEM
                    cmpa      #$20
                    beq       MEMSP               ; jump if space
                    cmpa      #$0A
                    beq       MEMLF               ; jump if linefeed
                    cmpa      #$2B
                    beq       MEMPLUS             ; jump if +
                    cmpa      #$5E
                    beq       MEMUA               ; jump if up arrow
                    cmpa      #$2D
                    beq       MEMUA               ; jump if -
                    cmpa      #$08
                    beq       MEMUA               ; jump if backspace
                    cmpa      #'/'
                    beq       MEMSL               ; jump if /
                    cmpa      #'='
                    beq       MEMSL               ; jump if =
                    cmpa      #'O'
                    beq       MEMOFF              ; jump if O
                    cmpa      #$0D
                    beq       MEMCR               ; jump if carriage ret
                    cmpa      #'.'
                    beq       MEMEND              ; jump if .
                    ldx       #MSG8               ; "command?"
                    jsr       OUTSTRG
                    bra       MEM1

MEMSP               jsr       CHGBYT
                    inx
                    stx       PTRMEM
                    xgdx
                    andb      #$0F
                    bne       MEM3                ; continue same line

MEMSP1              bra       MEM1                ; .. else start new line

MEMLF               jsr       CHGBYT
                    inx
                    stx       PTRMEM
                    bra       MEM2                ; output next address

MEMPLUS             jsr       CHGBYT
                    inx
                    stx       PTRMEM
                    bra       MEM1                ; output cr, next address

MEMUA               jsr       CHGBYT
                    dex
                    stx       PTRMEM
                    bra       MEM1                ; output cr, previous address

MEMSL               jsr       CHGBYT
                    bra       MEM1                ; output cr, same address

MEMOFF              ldd       SHFTREG             ; destination addr
                    subd      PTRMEM
                    cmpa      #$0
                    bne       MEMOFF1             ; jump if not 0
                    cmpb      #$80
                    bls       MEMOFF3             ; jump if in range
                    bra       MEMOFF2             ; out of range

MEMOFF1             cmpa      #$FF
                    bne       MEMOFF2             ; out of range
                    cmpb      #$81
                    bhs       MEMOFF3             ; in range
MEMOFF2             ldx       #MSG3               ; "Too long"
                    jsr       OUTSTRG
                    jmp       MEM1                ; output cr, addr, contents

MEMOFF3             subd      #$1                 ; b now has offset
                    stab      TMP4
                    jsr       OUTSPAC
                    ldx       #TMP4
                    jsr       OUT1BSP             ; output offset
                    jmp       MEM1                ; output cr, addr, contents

MEMCR               jsr       CHGBYT
MEMEND              rts                           ; exit task


;**********
;   move <src1> <src2> [<dest>]  - move
; *block at <src1> to <src2> to <dest>.
;  Moves block 1 byte up if no <dest>.
;**********
; *a = buffarg();
; *if(countu1 = 0) return(bad argument);
; *if( !wchek(a) ) return(bad argument);
; *ptr1 = shftreg;         /* src1 */

MOVE                equ       *
                    jsr       BUFFARG
                    tst       COUNT
                    beq       MOVERR              ; jump if no arg
                    jsr       WCHEK
                    bne       MOVERR              ; jump if no delim
                    ldx       SHFTREG             ; src1
                    stx       PTR1

; *a = buffarg();
; *if(countu1 = 0) return(bad argument);
; *if( !dchek(a) ) return(bad argument);
; *ptr2 = shftreg;         /* src2 */

                    jsr       BUFFARG
                    tst       COUNT
                    beq       MOVERR              ; jump if no arg
                    jsr       DCHEK
                    bne       MOVERR              ; jump if no delim
                    ldx       SHFTREG             ; src2
                    stx       PTR2

; *a = buffarg();
; *a = wskip();
; *if(a != cr) return(bad argument);
; *if(countu1 != 0) tmp2 = shftreg;  /* dest */
; *else tmp2 = ptr1 + 1;

                    jsr       BUFFARG
                    jsr       WSKIP
                    bne       MOVERR              ; jump if not cr
                    tst       COUNT
                    beq       MOVE1               ; jump if no arg
                    ldx       SHFTREG             ; dest
                    bra       MOVE2

MOVERR              ldx       #MSG9               ; "bad argument"
                    jsr       OUTSTRG
                    rts

MOVE1               ldx       PTR1
                    inx                           ; default dest
MOVE2               stx       PTR3

; *if(src1 < dest <= src2)
;     dest = dest+(src2-src1);
;     for(x = src2; x = src1; x--)
;          dest[0]-- = x[0]--;
                    ldx       PTR3                ; dest
                    cpx       PTR1                ; src1
                    bls       MOVE3               ; jump if dest =< src1
                    cpx       PTR2                ; src2
                    bhi       MOVE3               ; jump if dest > src2
                    ldd       PTR2
                    subd      PTR1
                    addd      PTR3
                    std       PTR3                ; dest = dest+(src2-src1)
                    ldx       PTR2
MOVELP1             jsr       CHKABRT             ; check for abort
                    ldaa      ,X                  ; char at src2
                    pshx
                    ldx       PTR3
                    jsr       WRITE               ; write a to x
                    cmpa      0,X
                    bne       MOVEBAD             ; jump if no write
                    dex
                    stx       PTR3
                    pulx
                    cpx       PTR1
                    beq       MOVRTS
                    dex
                    bra       MOVELP1             ; Loop SRC2 - SRC1 times

;

; else

;     for(x=src1; x=src2; x++)

;          dest[0]++ = x[0]++;


MOVE3               ldx       PTR1                ; srce1
MOVELP2             jsr       CHKABRT             ; check for abort
                    ldaa      ,X
                    pshx
                    ldx       PTR3                ; dest
                    jsr       WRITE               ; write a to x
                    cmpa      0,X
                    bne       MOVEBAD             ; jump if no write
                    inx
                    stx       PTR3
                    pulx
                    cpx       PTR2
                    beq       MOVRTS
                    inx
                    bra       MOVELP2             ; Loop SRC2-SRC1 times

MOVRTS              rts

MOVEBAD             pulx                          ; restore stack
                    ldx       #PTR3
                    jsr       OUT2BSP             ; output bad address
                    rts


;****************
;  assem(addr) -68HC11 line assembler/disassembler.
;       This routine will disassemble the opcode at
; *<addr> and then allow the user to enter a line for
; *assembly. Rules for assembly are as follows:
; -A '#' sign indicates immediate addressing.
; -A ',' (comma) indicates indexed addressing
;       and the next character must be X or Y.
; -All arguments are assumed to be hex and the
;       '$' sign shouldn't be used.
; -Arguments should be separated by 1 or more
;       spaces or tabs.
; -Any input after the required number of
;       arguments is ignored.
; -Upper or lower case makes no difference.
;
;       To signify end of input line, the following
; *commands are available and have the indicated action:
;   <cr>      - Finds the next opcode for
;          assembly.  If there was no assembly input,
;          the next opcode disassembled is retrieved
;          from the disassembler.
;   <lf><+>   - Works the same as carriage return
;          except if there was no assembly input, the
;          <addr> is incremented and the next <addr> is
;          disassembled.
;    <^><->   - Decrements <addr> and the previous
;          address is then disassembled.
;    </><=>   - Redisassembles the current address.
;
;       To exit the assembler use CONTROL A or . (period).
; *Of course control X and DEL will also allow you to abort.
;*** Equates for assembler ***
PAGE1               equ       $00                 ; values for page opcodes
PAGE2               equ       $18
PAGE3               equ       $1A
PAGE4               equ       $CD
IMMED               equ       $0                  ; addressing modes
INDX                equ       $1
INDY                equ       $2
LIMMED              equ       $3                  ; (long immediate)
OTHER               equ       $4

;*** Rename variables for assem/disassem ***
AMODE               equ       TMP2                ; addressing mode
YFLAG               equ       TMP3
PNORM               equ       TMP4                ; page for normal opcode
OLDPC               equ       PTR8
PC                  equ       PTR1                ; program counter
PX                  equ       PTR2                ; page for x indexed
PY                  equ       PTR2+1              ; page for y indexed
BASEOP              equ       PTR3                ; base opcode
CLASS               equ       PTR3+1              ; class
DISPC               equ       PTR4                ; pc for disassembler
BRADDR              equ       PTR5                ; relative branch offset
MNEPTR              equ       PTR6                ; pointer to table for dis
ASSCOMM             equ       PTR7                ; subcommand for assembler

;*** Error messages for assembler ***
MSGDIR              fdb       MSGA1               ; message table index
                    fdb       MSGA2
                    fdb       MSGA3
                    fdb       MSGA4
                    fdb       MSGA5
                    fdb       MSGA6
                    fdb       MSGA7
                    fdb       MSGA8
                    fdb       MSGA9
MSGA1               fcc       'Immed mode illegal',EOT
MSGA2               fcc       'Error in Mne table',EOT
MSGA3               fcc       'Illegal bit op',EOT
MSGA4               fcc       'Bad argument',EOT
MSGA5               fcc       'Mnemonic not found',EOT
MSGA6               fcc       'Unknown addressing mode',EOT
MSGA7               fcc       'Indexed addressing assumed',EOT
MSGA8               fcc       'Syntax error',EOT
MSGA9               fcc       'Branch out of range',EOT

;**********
; *oldpc = rambase;
; *a = wskip();
; *if (a != cr)
;   buffarg()
;   a = wskip();
;   if ( a != cr ) return(error);
;   oldpc = a;
ASSEM               equ       *
                    ldx       #RAMBS
                    stx       OLDPC
                    jsr       WSKIP
                    beq       ASSLOOP             ; jump if no argument
                    jsr       BUFFARG
                    jsr       WSKIP
                    beq       ASSEM1              ; jump if argument ok
                    ldx       #MSGA4              ; "bad argument"
                    jsr       OUTSTRG
                    rts

ASSEM1              ldx       SHFTREG
                    stx       OLDPC

; *repeat
;  pc = oldpc;
;  out2bsp(pc);
;  disassem();
;  a=readln();
;  asscomm = a;  /* save command */
;  if(a == [^,+,-,/,=]) outcrlf;
;  if(a == 0) return(error);

ASSLOOP             ldx       OLDPC
                    stx       PC
                    jsr       OUTCRLF
                    ldx       #PC
                    jsr       OUT2BSP             ; output the address
                    jsr       DISASSM             ; disassemble opcode
                    jsr       TABTO
                    ldaa      #PROMPT             ; prompt user
                    jsr       OUTA                ; output prompt character
                    jsr       READLN              ; read input for assembly
                    staa      ASSCOMM
                    cmpa      #'^'
                    beq       ASSLP0              ; jump if '^'
                    cmpa      #'+'
                    beq       ASSLP0              ; jump if '+'
                    cmpa      #'-'
                    beq       ASSLP0              ; jump if '-'
                    cmpa      #'/'
                    beq       ASSLP0              ; jump if '/'
                    cmpa      #'='
                    beq       ASSLP0              ; jump if '='
                    cmpa      #$00
                    bne       ASSLP1              ; jump if none of above
                    rts                           ; return if bad input

ASSLP0              jsr       OUTCRLF
ASSLP1              equ       *                   ; come here for cr or lf
                    jsr       OUTSPAC
                    jsr       OUTSPAC
                    jsr       OUTSPAC
                    jsr       OUTSPAC
                    jsr       OUTSPAC

;  b = parse(input); /* get mnemonic */
;  if(b > 5) print("not found"); asscomm='/';
;  elseif(b >= 1)
;     msrch();
;     if(class==$FF)
;        print("not found"); asscomm='/';
;     else
;        a = doop(opcode,class);
;        if(a == 0) dispc=0;
;        else process error; asscomm='/';

                    jsr       PARSE
                    cmpb      #$5
                    ble       ASSLP2              ; jump if mnemonic <= 5 chars
                    ldx       #MSGA5              ; "mnemonic not found"
                    jsr       OUTSTRG
                    bra       ASSLP5

ASSLP2              equ       *
                    cmpb      #$0
                    beq       ASSLP10             ; jump if no input
                    jsr       MSRCH
                    ldaa      CLASS
                    cmpa      #$FF
                    bne       ASSLP3
                    ldx       #MSGA5              ; "mnemonic not found"
                    jsr       OUTSTRG
                    bra       ASSLP5

ASSLP3              jsr       DOOP
                    cmpa      #$00
                    bne       ASSLP4              ; jump if doop error
                    ldx       #$00
                    stx       DISPC               ; indicate good assembly
                    bra       ASSLP10

ASSLP4              deca                          ; a = error message index
                    tab
                    ldx       #MSGDIR
                    abx
                    abx
                    ldx       0,X
                    jsr       OUTSTRG             ; output error message
ASSLP5              clr       ASSCOMM             ; error command

;  /* compute next address - asscomm holds subcommand
;     and dispc indicates if valid assembly occured. */
;  if(asscomm== ^ or -) oldpc--;
;  if(asscomm==(lf or + or cr)
;     if(dispc==0) oldpc=pc;   /* good assembly */
;     else
;        if(asscomm==lf or +) dispc= ++oldpc;
;        oldpc=dispc;
; *until(eot)
ASSLP10             equ       *
                    ldaa      ASSCOMM
                    cmpa      #'^'
                    beq       ASSLPA              ; jump if '^'
                    cmpa      #'-'
                    bne       ASSLP11             ; jump not '-'
ASSLPA              ldx       OLDPC               ; back up for '^' or '-'
                    dex
                    stx       OLDPC
                    bra       ASSLP15

ASSLP11             cmpa      #$0A
                    beq       ASSLP12             ; jump if linefeed
                    cmpa      #'+'
                    beq       ASSLP12             ; jump if '+'
                    cmpa      #$0D
                    bne       ASSLP15             ; jump if not cr
ASSLP12             ldx       DISPC
                    bne       ASSLP13             ; jump if dispc != 0
                    ldx       PC
                    stx       OLDPC
                    bra       ASSLP15

ASSLP13             cmpa      #$0A
                    beq       ASSLPB              ; jump not lf
                    cmpa      #'+'
                    bne       ASSLP14             ; jump not lf or '+'
ASSLPB              ldx       OLDPC
                    inx
                    stx       DISPC
ASSLP14             ldx       DISPC
                    stx       OLDPC
ASSLP15             jmp       ASSLOOP

;****************
;  readln() --- Read input from terminal into buffer
; until a command character is read (cr,lf,/,^).
; If more chars are typed than the buffer will hold,
; the extra characters are overwritten on the end.
;  On exit: b=number of chars read, a=0 if quit,
; else a=next command.
;****************
; *for(b==0;b<=bufflng;b++) inbuff[b] = cr;

READLN              clrb
                    ldaa      #$0D                ; carriage ret
RLN0                ldx       #INBUFF
                    abx
                    staa      0,X                 ; initialize input buffer
                    incb
                    cmpb      #BUFFLNG
                    blt       RLN0
; *b=0;
; *repeat
;  if(a == (ctla, cntlc, cntld, cntlx, del))
;     return(a=0);
;  if(a == backspace)
;     if(b > 0) b--;
;     else b=0;
;  else  inbuff[b] = upcase(a);
;  if(b < bufflng) b++;
; *until (a == [cr,lf,+,^,-,/,=])
; *return(a);

                    clrb
RLN1                jsr       INCHAR
                    cmpa      #DEL                ; Delete
                    beq       RLNQUIT
                    cmpa      #CTLX               ; Control X
                    beq       RLNQUIT
                    cmpa      #CTLA               ; Control A
                    beq       RLNQUIT
                    cmpa      #$2E                ; Period
                    beq       RLNQUIT
                    cmpa      #$03                ; Control C
                    beq       RLNQUIT
                    cmpa      #$04                ; Control D
                    beq       RLNQUIT
                    cmpa      #$08                ; backspace
                    bne       RLN2
                    decb
                    bgt       RLN1
                    bra       READLN              ; start over

RLN2                ldx       #INBUFF
                    abx
                    jsr       UPCASE
                    staa      0,X                 ; put char in buffer
                    cmpb      #BUFFLNG            ; max buffer length
                    bge       RLN3                ; jump if buffer full
                    incb                          ; move buffer pointer
RLN3                bsr       ASSCHEK             ; check for subcommand
                    bne       RLN1
                    rts

RLNQUIT             clra                          ; quit
                    rts                           ; return


;**********
;  parse() -parse out the mnemonic from INBUFF
; to COMBUFF. on exit: b=number of chars parsed.
;**********
; *combuff[3] = <space>;   initialize 4th character to space.
; *ptrbuff[] = inbuff[];
; *a=wskip();
; *for (b = 0; b = 5; b++)
;   a=readbuff(); incbuff();
;   if (a = (cr,lf,^,/,wspace)) return(b);
;   combuff[b] = upcase(a);
; *return(b);

PARSE               ldaa      #$20
                    staa      COMBUFF+3
                    ldx       #INBUFF             ; initialize buffer ptr
                    stx       PTR0
                    jsr       WSKIP               ; find first character
                    clrb
PARSLP              jsr       READBUFF            ; read character
                    jsr       INCBUFF
                    jsr       WCHEK
                    beq       PARSRT              ; jump if whitespace
                    bsr       ASSCHEK
                    beq       PARSRT              ; jump if end of line
                    jsr       UPCASE              ; convert to upper case
                    ldx       #COMBUFF
                    abx
                    staa      0,X                 ; store in combuff
                    incb
                    cmpb      #$5
                    ble       PARSLP              ; loop 6 times
PARSRT              rts


;****************
;  asschek() -perform compares for
; lf, cr, ^, /, +, -, =
;****************
ASSCHEK             cmpa      #$0A                ; linefeed
                    beq       ASSCHK1
                    cmpa      #$0D                ; carriage ret
                    beq       ASSCHK1
                    cmpa      #'^'                ; up arrow
                    beq       ASSCHK1
                    cmpa      #'/'                ; slash
                    beq       ASSCHK1
                    cmpa      #'+'                ; plus
                    beq       ASSCHK1
                    cmpa      #'-'                ; minus
                    beq       ASSCHK1
                    cmpa      #'='                ; equals
ASSCHK1             rts


;*********
;  msrch() --- Search MNETABL for mnemonic in COMBUFF.
; *stores base opcode at baseop and class at class.
;  Class = FF if not found.
;**********
; *while ( != EOF )
;   if (COMBUFF[0-3] = MNETABL[0-3])
;      return(MNETABL[4],MNETABL[5]);
;   else *MNETABL =+ 6

MSRCH               ldx       #MNETABL            ; pointer to mnemonic table
                    ldy       #COMBUFF            ; pointer to string
                    bra       MSRCH1

MSNEXT              equ       *
                    ldab      #6
                    abx                           ; point to next table entry
MSRCH1              ldaa      0,X                 ; read table
                    cmpa      #EOT
                    bne       MSRCH2              ; jump if not end of table
                    ldaa      #$FF
                    staa      CLASS               ; FF = not in table
                    rts

MSRCH2              cmpa      0,Y                 ; op[0] = tabl[0] ?
                    bne       MSNEXT
                    ldaa      1,X
                    cmpa      1,Y                 ; op[1] = tabl[1] ?
                    bne       MSNEXT
                    ldaa      2,X
                    cmpa      2,Y                 ; op[2] = tabl[2] ?
                    bne       MSNEXT
                    ldaa      3,X
                    cmpa      3,Y                 ; op[2] = tabl[2] ?
                    bne       MSNEXT
                    ldd       4,X                 ; opcode, class
                    staa      BASEOP
                    stab      CLASS
                    rts

;**********
;**   doop(baseop,class) --- process mnemonic.
;**   on exit: a=error code corresponding to error
;**                                     messages.
;**********
; *amode = OTHER; /* addressing mode */
; *yflag = 0;     /* ynoimm, nlimm, and cpd flag */
; *x[] = ptrbuff[]

DOOP                equ       *
                    ldaa      #OTHER
                    staa      AMODE               ; mode
                    clr       YFLAG
                    ldx       PTR0

; *while (*x != end of buffer)
;   if (x[0]++ == ',')
;      if (x[0] == 'y') amode = INDY;
;      else amod = INDX;
;      break;
; *a = wskip()
; *if( a == '#' ) amode = IMMED;

DOPLP1              cpx       #ENDBUFF            ; (end of buffer)
                    beq       DOOP1               ; jump if end of buffer
                    ldd       0,X                 ; read 2 chars from buffer
                    inx                           ; move pointer
                    cmpa      #','
                    bne       DOPLP1
                    cmpb      #'Y'                ; look for ",y"
                    bne       DOPLP2
                    ldaa      #INDY
                    staa      AMODE
                    bra       DOOP1

DOPLP2              cmpb      #'X'                ; look for ",x"
                    bne       DOOP1               ; jump if not x
                    ldaa      #INDX
                    staa      AMODE

DOOP1               jsr       WSKIP
                    cmpa      #'#'                ; look for immediate mode
                    bne       DOOP2
                    jsr       INCBUFF             ; point at argument
                    ldaa      #IMMED
                    staa      AMODE
DOOP2               equ       *

; *switch(class)
                    ldab      CLASS
                    cmpb      #P2INH
                    beq       DOP2I

DOSW1               cmpb      #INH
                    beq       DOINH

DOSW2               cmpb      #REL
                    beq       DOREL

DOSW3               cmpb      #LIMM
                    bne       DOSW4
                    jmp       DOLIM

DOSW4               cmpb      #NIMM
                    bne       DOSW5
                    jmp       DONOI

DOSW5               cmpb      #GEN
                    bne       DOSW6
                    jmp       DOGENE

DOSW6               cmpb      #GRP2
                    bne       DOSW7
                    jmp       DOGRP

DOSW7               cmpb      #CPD
                    bne       DOSW8
                    jmp       DOCPD

DOSW8               cmpb      #XNIMM
                    bne       DOSW9
                    jmp       DOXNOI

DOSW9               cmpb      #XLIMM
                    bne       DOSW10
                    jmp       DOXLI

DOSW10              cmpb      #YNIMM
                    bne       DOSW11
                    jmp       DOYNOI

DOSW11              cmpb      #YLIMM
                    bne       DOSW12
                    jmp       DOYLI

DOSW12              cmpb      #BTB
                    bne       DOSW13
                    jmp       DOBTB

DOSW13              cmpb      #SETCLR
                    bne       DODEF
                    jmp       DOSET

;   default: return("error in mnemonic table");

DODEF               ldaa      #$2
                    rts

;  case P2INH: emit(PAGE2)

DOP2I               ldaa      #PAGE2
                    jsr       EMIT

;  case INH: emit(baseop);
;       return(0);

DOINH               ldaa      BASEOP
                    jsr       EMIT
                    clra
                    rts

;  case REL: a = assarg();
;            if(a=4) return(a);
;            d = address - pc + 2;
;            if ($7f >= d >= $ff82)
;               return (out of range);
;            emit(opcode);
;            emit(offset);
;            return(0);

DOREL               jsr       ASSARG
                    cmpa      #$04
                    bne       DOREL1              ; jump if arg ok
                    rts

DOREL1              ldd       SHFTREG             ; get branch address
                    ldx       PC                  ; get program counter
                    inx
                    inx                           ; point to end of opcode
                    stx       BRADDR
                    subd      BRADDR              ; calculate offset
                    std       BRADDR              ; save result
                    cmpd      #$7F                ; in range ?
                    bls       DOREL2              ; jump if in range
                    cmpd      #$FF80
                    bhs       DOREL2              ; jump if in range
                    ldaa      #$09                ; 'Out of range'
                    rts

DOREL2              ldaa      BASEOP
                    jsr       EMIT                ; emit opcode
                    ldaa      BRADDR+1
                    jsr       EMIT                ; emit offset
                    clra                          ; normal return
                    rts

;  case LIMM: if (amode == IMMED) amode = LIMMED;

DOLIM               ldaa      AMODE
                    cmpa      #IMMED
                    bne       DONOI
                    ldaa      #LIMMED
                    staa      AMODE

;  case NIMM: if (amode == IMMED)
;                return("Immediate mode illegal");

DONOI               ldaa      AMODE
                    cmpa      #IMMED
                    bne       DOGENE              ; jump if not immediate
                    ldaa      #$1                 ; "immediate mode illegal"
                    rts

;  case GEN: dogen(baseop,amode,PAGE1,PAGE1,PAGE2);
;            return;

DOGENE              ldaa      #PAGE1
                    staa      PNORM
                    staa      PX
                    ldaa      #PAGE2
                    staa      PY
                    jsr       DOGEN
                    rts

;  case GRP2: if (amode == INDY)
;                emit(PAGE2);
;                amode = INDX;
;             if( amode == INDX )
;                doindx(baseop);
;             else a = assarg();
;                if(a=4) return(a);
;                emit(opcode+0x10);
;                emit(extended address);
;             return;

DOGRP               ldaa      AMODE
                    cmpa      #INDY
                    bne       DOGRP1
                    ldaa      #PAGE2
                    jsr       EMIT
                    ldaa      #INDX
                    staa      AMODE
DOGRP1              equ       *
                    ldaa      AMODE
                    cmpa      #INDX
                    bne       DOGRP2
                    jsr       DOINDEX
                    rts

DOGRP2              equ       *
                    ldaa      BASEOP
                    adda      #$10
                    jsr       EMIT
                    jsr       ASSARG
                    cmpa      #$04
                    beq       DOGRPRT             ; jump if bad arg
                    ldd       SHFTREG             ; extended address
                    jsr       EMIT
                    tba
                    jsr       EMIT
                    clra
DOGRPRT             rts

;  case CPD: if (amode == IMMED)
;               amode = LIMMED; /* cpd */
;            if( amode == INDY ) yflag = 1;
;            dogen(baseop,amode,PAGE3,PAGE3,PAGE4);
;            return;

DOCPD               ldaa      AMODE
                    cmpa      #IMMED
                    bne       DOCPD1
                    ldaa      #LIMMED
                    staa      AMODE
DOCPD1              ldaa      AMODE
                    cmpa      #INDY
                    bne       DOCPD2
                    inc       YFLAG
DOCPD2              ldaa      #PAGE3
                    staa      PNORM
                    staa      PX
                    ldaa      #PAGE4
                    staa      PY
                    jsr       DOGEN
                    rts

;  case XNIMM: if (amode == IMMED)      /* stx */
;                 return("Immediate mode illegal");

DOXNOI              ldaa      AMODE
                    cmpa      #IMMED
                    bne       DOXLI
                    ldaa      #$1                 ; "immediate mode illegal"
                    rts

;  case XLIMM: if (amode == IMMED)  /* cpx, ldx */
;                 amode = LIMMED;
;              dogen(baseop,amode,PAGE1,PAGE1,PAGE4);
;              return;

DOXLI               ldaa      AMODE
                    cmpa      #IMMED
                    bne       DOXLI1
                    ldaa      #LIMMED
                    staa      AMODE
DOXLI1              ldaa      #PAGE1
                    staa      PNORM
                    staa      PX
                    ldaa      #PAGE4
                    staa      PY
                    jsr       DOGEN
                    rts

;  case YNIMM: if (amode == IMMED)      /* sty */
;                 return("Immediate mode illegal");

DOYNOI              ldaa      AMODE
                    cmpa      #IMMED
                    bne       DOYLI
                    ldaa      #$1                 ; "immediate mode illegal"
                    rts

;  case YLIMM: if (amode == INDY) yflag = 1;/* cpy, ldy */
;              if(amode == IMMED) amode = LIMMED;
;              dogen(opcode,amode,PAGE2,PAGE3,PAGE2);
;              return;

DOYLI               ldaa      AMODE
                    cmpa      #INDY
                    bne       DOYLI1
                    inc       YFLAG
DOYLI1              cmpa      #IMMED
                    bne       DOYLI2
                    ldaa      #LIMMED
                    staa      AMODE
DOYLI2              ldaa      #PAGE2
                    staa      PNORM
                    staa      PY
                    ldaa      #PAGE3
                    staa      PX
                    jsr       DOGEN
                    rts

;  case BTB:        /* bset, bclr */
;  case SETCLR: a = bitop(baseop,amode,class);
;               if(a=0) return(a = 3);
;               if( amode == INDY )
;                  emit(PAGE2);
;                  amode = INDX;

DOBTB               equ       *
DOSET               bsr       BITOP
                    cmpa      #$00
                    bne       DOSET1
                    ldaa      #$3                 ; "illegal bit op"
                    rts

DOSET1              ldaa      AMODE
                    cmpa      #INDY
                    bne       DOSET2
                    ldaa      #PAGE2
                    jsr       EMIT
                    ldaa      #INDX
                    staa      AMODE
DOSET2              equ       *

;               emit(baseop);
;               a = assarg();
;               if(a = 4) return(a);
;               emit(index offset);
;               if( amode == INDX )
;                  Buffptr += 2;      /* skip ,x or ,y */

                    ldaa      BASEOP
                    jsr       EMIT
                    jsr       ASSARG
                    cmpa      #$04
                    bne       DOSET22             ; jump if arg ok
                    rts

DOSET22             ldaa      SHFTREG+1           ; index offset
                    jsr       EMIT
                    ldaa      AMODE
                    cmpa      #INDX
                    bne       DOSET3
                    jsr       INCBUFF
                    jsr       INCBUFF
DOSET3              equ       *

;               a = assarg();
;               if(a = 4) return(a);
;               emit(mask);   /* mask */
;               if( class == SETCLR )
;                  return;

                    jsr       ASSARG
                    cmpa      #$04
                    bne       DOSET33             ; jump if arg ok
                    rts

DOSET33             ldaa      SHFTREG+1           ; mask
                    jsr       EMIT
                    ldaa      CLASS
                    cmpa      #SETCLR
                    bne       DOSET4
                    clra
                    rts

DOSET4              equ       *

;               a = assarg();
;               if(a = 4) return(a);
;               d = (pc+1) - shftreg;
;               if ($7f >= d >= $ff82)
;                  return (out of range);
;               emit(branch offset);
;               return(0);

                    jsr       ASSARG
                    cmpa      #$04
                    bne       DOSET5              ; jump if arg ok
                    rts

DOSET5              ldx       PC                  ; program counter
                    inx                           ; point to next inst
                    stx       BRADDR              ; save pc value
                    ldd       SHFTREG             ; get branch address
                    subd      BRADDR              ; calculate offset
                    cmpd      #$7F
                    bls       DOSET6              ; jump if in range
                    cmpd      #$FF80
                    bhs       DOSET6              ; jump if in range
                    clra
                    jsr       EMIT
                    ldaa      #$09                ; 'out of range'
                    rts

DOSET6              tba                           ; offset
                    jsr       EMIT
                    clra
                    rts


;**********
;**   bitop(baseop,amode,class) --- adjust opcode on bit
;**       manipulation instructions.  Returns opcode in a
;**       or a = 0 if error
;**********
; *if( amode == INDX || amode == INDY ) return(op);
; *if( class == SETCLR ) return(op-8);
; *else if(class==BTB) return(op-12);
; *else fatal("bitop");

BITOP               equ       *
                    ldaa      AMODE
                    ldab      CLASS
                    cmpa      #INDX
                    bne       BITOP1
                    rts

BITOP1              cmpa      #INDY
                    bne       BITOP2              ; jump not indexed
                    rts

BITOP2              cmpb      #SETCLR
                    bne       BITOP3              ; jump not bset,bclr
                    ldaa      BASEOP              ; get opcode
                    suba      #8
                    staa      BASEOP
                    rts

BITOP3              cmpb      #BTB
                    bne       BITOP4              ; jump not bit branch
                    ldaa      BASEOP              ; get opcode
                    suba      #12
                    staa      BASEOP
                    rts

BITOP4              clra                          ; 0 = fatal bitop
                    rts

;**********
;**   dogen(baseop,mode,pnorm,px,py) - process
;** general addressing modes. Returns a = error #.
;**********
; *pnorm = page for normal addressing modes: IMM,DIR,EXT
; *px = page for INDX addressing
; *py = page for INDY addressing
; *switch(amode)
DOGEN               ldaa      AMODE
                    cmpa      #LIMMED
                    beq       DOGLIM
                    cmpa      #IMMED
                    beq       DOGIMM
                    cmpa      #INDY
                    beq       DOGINDY
                    cmpa      #INDX
                    beq       DOGINDX
                    cmpa      #OTHER
                    beq       DOGOTH

; *default: error("Unknown Addressing Mode");

DOGDEF              ldaa      #$06                ; unknown addre...
                    rts

; *case LIMMED: epage(pnorm);
;             emit(baseop);
;             a = assarg();
;             if(a = 4) return(a);
;             emit(2 bytes);
;             return(0);

DOGLIM              ldaa      PNORM
                    jsr       EPAGE
DOGLIM1             ldaa      BASEOP
                    jsr       EMIT
                    jsr       ASSARG              ; get next argument
                    cmpa      #$04
                    bne       DOGLIM2             ; jump if arg ok
                    rts

DOGLIM2             ldd       SHFTREG
                    jsr       EMIT
                    tba
                    jsr       EMIT
                    clra
                    rts

; *case IMMED: epage(pnorm);
;            emit(baseop);
;            a = assarg();
;            if(a = 4) return(a);
;            emit(lobyte);
;            return(0);

DOGIMM              ldaa      PNORM
                    jsr       EPAGE
                    ldaa      BASEOP
                    jsr       EMIT
                    bsr       ASSARG
                    cmpa      #$04
                    bne       DOGIMM1             ; jump if arg ok
                    rts

DOGIMM1             ldaa      SHFTREG+1
                    jsr       EMIT
                    clra
                    rts

; *case INDY: epage(py);
;           a=doindex(op+0x20);
;           return(a);

DOGINDY             ldaa      PY
                    jsr       EPAGE
                    ldaa      BASEOP
                    adda      #$20
                    staa      BASEOP
                    bsr       DOINDEX
                    rts

; *case INDX: epage(px);
;           a=doindex(op+0x20);
;           return(a);

DOGINDX             ldaa      PX
                    bsr       EPAGE
                    ldaa      BASEOP
                    adda      #$20
                    staa      BASEOP
                    bsr       DOINDEX
                    rts

; *case OTHER: a = assarg();
;            if(a = 4) return(a);
;            epage(pnorm);
;            if(countu1 <= 2 digits)   /* direct */
;               emit(op+0x10);
;               emit(lobyte(Result));
;               return(0);
;            else    emit(op+0x30);    /* extended */
;               eword(Result);
;               return(0)

DOGOTH              bsr       ASSARG
                    cmpa      #$04
                    bne       DOGOTH0             ; jump if arg ok
                    rts

DOGOTH0             ldaa      PNORM
                    bsr       EPAGE
                    ldaa      COUNT
                    cmpa      #$2
                    bgt       DOGOTH1
                    ldaa      BASEOP
                    adda      #$10                ; direct mode opcode
                    bsr       EMIT
                    ldaa      SHFTREG+1
                    bsr       EMIT
                    clra
                    rts

DOGOTH1             ldaa      BASEOP
                    adda      #$30                ; extended mode opcode
                    bsr       EMIT
                    ldd       SHFTREG
                    bsr       EMIT
                    tba
                    bsr       EMIT
                    clra
                    rts

;**********
;**  doindex(op) --- handle all wierd stuff for
;**   indexed addressing. Returns a = error number.
;**********
; *emit(baseop);
; *a=assarg();
; *if(a = 4) return(a);
; *if( a != ',' ) return("Syntax");
; *buffptr++
; *a=readbuff()
; *if( a != 'x' &&  != 'y') warn("Ind Addr Assumed");
; *emit(lobyte);
; *return(0);

DOINDEX             ldaa      BASEOP
                    bsr       EMIT
                    bsr       ASSARG
                    cmpa      #$04
                    bne       DOINDX0             ; jump if arg ok
                    rts

DOINDX0             cmpa      #','
                    beq       DOINDX1
                    ldaa      #$08                ; "syntax error"
                    rts

DOINDX1             jsr       INCBUFF
                    jsr       READBUFF
                    cmpa      #'Y'
                    beq       DOINDX2
                    cmpa      #'X'
                    beq       DOINDX2
                    ldx       MSGA7               ; "index addr assumed"
                    jsr       OUTSTRG
DOINDX2             ldaa      SHFTREG+1
                    bsr       EMIT
                    clra
                    rts

;**********
;**   assarg(); - get argument.  Returns a = 4 if bad
;** argument, else a = first non hex char.
;**********
; *a = buffarg()
; *if(asschk(aa) && countu1 != 0) return(a);
; *return(bad argument);

ASSARG              jsr       BUFFARG
                    jsr       ASSCHEK             ; check for command
                    beq       ASSARG1             ; jump if ok
                    jsr       WCHEK               ; check for whitespace
                    bne       ASSARG2             ; jump if not ok
ASSARG1             tst       COUNT
                    beq       ASSARG2             ; jump if no argument
                    rts

ASSARG2             ldaa      #$04                ; bad argument
                    rts

;**********
;**  epage(a) --- emit page prebyte
;**********
; *if( a != PAGE1 ) emit(a);

EPAGE               cmpa      #PAGE1
                    beq       EPAGRT              ; jump if page 1
                    bsr       EMIT
EPAGRT              rts

;**********
;   emit(a) --- emit contents of a
;**********
EMIT                ldx       PC
                    jsr       WRITE               ; write a to x
                    jsr       OUT1BSP
                    stx       PC
                    rts

; *Mnemonic table for hc11 line assembler
NULL                equ       $0                  ; nothing
INH                 equ       $1                  ; inherent
P2INH               equ       $2                  ; page 2 inherent
GEN                 equ       $3                  ; general addressing
GRP2                equ       $4                  ; group 2
REL                 equ       $5                  ; relative
IMM                 equ       $6                  ; immediate
NIMM                equ       $7                  ; general except for immediate
LIMM                equ       $8                  ; 2 byte immediate
XLIMM               equ       $9                  ; longimm for x
XNIMM               equ       $10                 ; no immediate for x
YLIMM               equ       $11                 ; longimm for y
YNIMM               equ       $12                 ; no immediate for y
BTB                 equ       $13                 ; bit test and branch
SETCLR              equ       $14                 ; bit set or clear
CPD                 equ       $15                 ; compare d
BTBD                equ       $16                 ; bit test and branch direct
SETCLRD             equ       $17                 ; bit set or clear direct

;**********
;   mnetabl - includes all '11 mnemonics, base opcodes,
; and type of instruction.  The assembler search routine
; *depends on 4 characters for each mnemonic so that 3 char
; *mnemonics are extended with a space and 5 char mnemonics
; *are truncated.
;**********

MNETABL             fcc       'ABA '              ; Mnemonic
                    fcb       $1B                 ; Base opcode
                    fcb       INH                 ; Class
                    fcc       'ABX '
                    fcb       $3A
                    fcb       INH
                    fcc       'ABY '
                    fcb       $3A
                    fcb       P2INH
                    fcc       'ADCA'
                    fcb       $89
                    fcb       GEN
                    fcc       'ADCB'
                    fcb       $C9
                    fcb       GEN
                    fcc       'ADDA'
                    fcb       $8B
                    fcb       GEN
                    fcc       'ADDB'
                    fcb       $CB
                    fcb       GEN
                    fcc       'ADDD'
                    fcb       $C3
                    fcb       LIMM
                    fcc       'ANDA'
                    fcb       $84
                    fcb       GEN
                    fcc       'ANDB'
                    fcb       $C4
                    fcb       GEN
                    fcc       'ASL '
                    fcb       $68
                    fcb       GRP2
                    fcc       'ASLA'
                    fcb       $48
                    fcb       INH
                    fcc       'ASLB'
                    fcb       $58
                    fcb       INH
                    fcc       'ASLD'
                    fcb       $05
                    fcb       INH
                    fcc       'ASR '
                    fcb       $67
                    fcb       GRP2
                    fcc       'ASRA'
                    fcb       $47
                    fcb       INH
                    fcc       'ASRB'
                    fcb       $57
                    fcb       INH
                    fcc       'BCC '
                    fcb       $24
                    fcb       REL
                    fcc       'BCLR'
                    fcb       $1D
                    fcb       SETCLR
                    fcc       'BCS '
                    fcb       $25
                    fcb       REL
                    fcc       'BEQ '
                    fcb       $27
                    fcb       REL
                    fcc       'BGE '
                    fcb       $2C
                    fcb       REL
                    fcc       'BGT '
                    fcb       $2E
                    fcb       REL
                    fcc       'BHI '
                    fcb       $22
                    fcb       REL
                    fcc       'BHS '
                    fcb       $24
                    fcb       REL
                    fcc       'BITA'
                    fcb       $85
                    fcb       GEN
                    fcc       'BITB'
                    fcb       $C5
                    fcb       GEN
                    fcc       'BLE '
                    fcb       $2F
                    fcb       REL
                    fcc       'BLO '
                    fcb       $25
                    fcb       REL
                    fcc       'BLS '
                    fcb       $23
                    fcb       REL
                    fcc       'BLT '
                    fcb       $2D
                    fcb       REL
                    fcc       'BMI '
                    fcb       $2B
                    fcb       REL
                    fcc       'BNE '
                    fcb       $26
                    fcb       REL
                    fcc       'BPL '
                    fcb       $2A
                    fcb       REL
                    fcc       'BRA '
                    fcb       $20
                    fcb       REL
                    fcc       'BRCL'              ; (BRCLR)
                    fcb       $1F
                    fcb       BTB
                    fcc       'BRN '
                    fcb       $21
                    fcb       REL
                    fcc       'BRSE'              ; (BRSET)
                    fcb       $1E
                    fcb       BTB
                    fcc       'BSET'
                    fcb       $1C
                    fcb       SETCLR
                    fcc       'BSR '
                    fcb       $8D
                    fcb       REL
                    fcc       'BVC '
                    fcb       $28
                    fcb       REL
                    fcc       'BVS '
                    fcb       $29
                    fcb       REL
                    fcc       'CBA '
                    fcb       $11
                    fcb       INH
                    fcc       'CLC '
                    fcb       $0C
                    fcb       INH
                    fcc       'CLI '
                    fcb       $0E
                    fcb       INH
                    fcc       'CLR '
                    fcb       $6F
                    fcb       GRP2
                    fcc       'CLRA'
                    fcb       $4F
                    fcb       INH
                    fcc       'CLRB'
                    fcb       $5F
                    fcb       INH
                    fcc       'CLV '
                    fcb       $0A
                    fcb       INH
                    fcc       'CMPA'
                    fcb       $81
                    fcb       GEN
                    fcc       'CMPB'
                    fcb       $C1
                    fcb       GEN
                    fcc       'COM '
                    fcb       $63
                    fcb       GRP2
                    fcc       'COMA'
                    fcb       $43
                    fcb       INH
                    fcc       'COMB'
                    fcb       $53
                    fcb       INH
                    fcc       'CPD '
                    fcb       $83
                    fcb       CPD
                    fcc       'CPX '
                    fcb       $8C
                    fcb       XLIMM
                    fcc       'CPY '
                    fcb       $8C
                    fcb       YLIMM
                    fcc       'DAA '
                    fcb       $19
                    fcb       INH
                    fcc       'DEC '
                    fcb       $6A
                    fcb       GRP2
                    fcc       'DECA'
                    fcb       $4A
                    fcb       INH
                    fcc       'DECB'
                    fcb       $5A
                    fcb       INH
                    fcc       'DES '
                    fcb       $34
                    fcb       INH
                    fcc       'DEX '
                    fcb       $09
                    fcb       INH
                    fcc       'DEY '
                    fcb       $09
                    fcb       P2INH
                    fcc       'EORA'
                    fcb       $88
                    fcb       GEN
                    fcc       'EORB'
                    fcb       $C8
                    fcb       GEN
                    fcc       'FDIV'
                    fcb       $03
                    fcb       INH
                    fcc       'IDIV'
                    fcb       $02
                    fcb       INH
                    fcc       'INC '
                    fcb       $6C
                    fcb       GRP2
                    fcc       'INCA'
                    fcb       $4C
                    fcb       INH
                    fcc       'INCB'
                    fcb       $5C
                    fcb       INH
                    fcc       'INS '
                    fcb       $31
                    fcb       INH
                    fcc       'INX '
                    fcb       $08
                    fcb       INH
                    fcc       'INY '
                    fcb       $08
                    fcb       P2INH
                    fcc       'JMP '
                    fcb       $6E
                    fcb       GRP2
                    fcc       'JSR '
                    fcb       $8D
                    fcb       NIMM
                    fcc       'LDAA'
                    fcb       $86
                    fcb       GEN
                    fcc       'LDAB'
                    fcb       $C6
                    fcb       GEN
                    fcc       'LDD '
                    fcb       $CC
                    fcb       LIMM
                    fcc       'LDS '
                    fcb       $8E
                    fcb       LIMM
                    fcc       'LDX '
                    fcb       $CE
                    fcb       XLIMM
                    fcc       'LDY '
                    fcb       $CE
                    fcb       YLIMM
                    fcc       'LSL '
                    fcb       $68
                    fcb       GRP2
                    fcc       'LSLA'
                    fcb       $48
                    fcb       INH
                    fcc       'LSLB'
                    fcb       $58
                    fcb       INH
                    fcc       'LSLD'
                    fcb       $05
                    fcb       INH
                    fcc       'LSR '
                    fcb       $64
                    fcb       GRP2
                    fcc       'LSRA'
                    fcb       $44
                    fcb       INH
                    fcc       'LSRB'
                    fcb       $54
                    fcb       INH
                    fcc       'LSRD'
                    fcb       $04
                    fcb       INH
                    fcc       'MUL '
                    fcb       $3D
                    fcb       INH
                    fcc       'NEG '
                    fcb       $60
                    fcb       GRP2
                    fcc       'NEGA'
                    fcb       $40
                    fcb       INH
                    fcc       'NEGB'
                    fcb       $50
                    fcb       INH
                    fcc       'NOP '
                    fcb       $01
                    fcb       INH
                    fcc       'ORAA'
                    fcb       $8A
                    fcb       GEN
                    fcc       'ORAB'
                    fcb       $CA
                    fcb       GEN
                    fcc       'PSHA'
                    fcb       $36
                    fcb       INH
                    fcc       'PSHB'
                    fcb       $37
                    fcb       INH
                    fcc       'PSHX'
                    fcb       $3C
                    fcb       INH
                    fcc       'PSHY'
                    fcb       $3C
                    fcb       P2INH
                    fcc       'PULA'
                    fcb       $32
                    fcb       INH
                    fcc       'PULB'
                    fcb       $33
                    fcb       INH
                    fcc       'PULX'
                    fcb       $38
                    fcb       INH
                    fcc       'PULY'
                    fcb       $38
                    fcb       P2INH
                    fcc       'ROL '
                    fcb       $69
                    fcb       GRP2
                    fcc       'ROLA'
                    fcb       $49
                    fcb       INH
                    fcc       'ROLB'
                    fcb       $59
                    fcb       INH
                    fcc       'ROR '
                    fcb       $66
                    fcb       GRP2
                    fcc       'RORA'
                    fcb       $46
                    fcb       INH
                    fcc       'RORB'
                    fcb       $56
                    fcb       INH
                    fcc       'RTI '
                    fcb       $3B
                    fcb       INH
                    fcc       'RTS '
                    fcb       $39
                    fcb       INH
                    fcc       'SBA '
                    fcb       $10
                    fcb       INH
                    fcc       'SBCA'
                    fcb       $82
                    fcb       GEN
                    fcc       'SBCB'
                    fcb       $C2
                    fcb       GEN
                    fcc       'SEC '
                    fcb       $0D
                    fcb       INH
                    fcc       'SEI '
                    fcb       $0F
                    fcb       INH
                    fcc       'SEV '
                    fcb       $0B
                    fcb       INH
                    fcc       'STAA'
                    fcb       $87
                    fcb       NIMM
                    fcc       'STAB'
                    fcb       $C7
                    fcb       NIMM
                    fcc       'STD '
                    fcb       $CD
                    fcb       NIMM
                    fcc       'STOP'
                    fcb       $CF
                    fcb       INH
                    fcc       'STS '
                    fcb       $8F
                    fcb       NIMM
                    fcc       'STX '
                    fcb       $CF
                    fcb       XNIMM
                    fcc       'STY '
                    fcb       $CF
                    fcb       YNIMM
                    fcc       'SUBA'
                    fcb       $80
                    fcb       GEN
                    fcc       'SUBB'
                    fcb       $C0
                    fcb       GEN
                    fcc       'SUBD'
                    fcb       $83
                    fcb       LIMM
                    fcc       'SWI '
                    fcb       $3F
                    fcb       INH
                    fcc       'TAB '
                    fcb       $16
                    fcb       INH
                    fcc       'TAP '
                    fcb       $06
                    fcb       INH
                    fcc       'TBA '
                    fcb       $17
                    fcb       INH
                    fcc       'TPA '
                    fcb       $07
                    fcb       INH
                    fcc       'TEST'
                    fcb       $00
                    fcb       INH
                    fcc       'TST '
                    fcb       $6D
                    fcb       GRP2
                    fcc       'TSTA'
                    fcb       $4D
                    fcb       INH
                    fcc       'TSTB'
                    fcb       $5D
                    fcb       INH
                    fcc       'TSX '
                    fcb       $30
                    fcb       INH
                    fcc       'TSY '
                    fcb       $30
                    fcb       P2INH
                    fcc       'TXS '
                    fcb       $35
                    fcb       INH
                    fcc       'TYS '
                    fcb       $35
                    fcb       P2INH
                    fcc       'WAI '
                    fcb       $3E
                    fcb       INH
                    fcc       'XGDX'
                    fcb       $8F
                    fcb       INH
                    fcc       'XGDY'
                    fcb       $8F
                    fcb       P2INH
                    fcc       'BRSE'              ; bit direct modes for
                    fcb       $12                 ; disassembler.
                    fcb       BTBD
                    fcc       'BRCL'
                    fcb       $13
                    fcb       BTBD
                    fcc       'BSET'
                    fcb       $14
                    fcb       SETCLRD
                    fcc       'BCLR'
                    fcb       $15
                    fcb       SETCLRD
                    fcb       EOT                 ; End of table

;**********************************************
PG1                 equ       $0
PG2                 equ       $1
PG3                 equ       $2
PG4                 equ       $3

;******************
; *disassem() - disassemble the opcode.
;******************
; *(check for page prebyte)
; *baseop=pc[0];
; *pnorm=PG1;
; *if(baseop==$18) pnorm=PG2;
; *if(baseop==$1A) pnorm=PG3;
; *if(baseop==$CD) pnorm=PG4;
; *if(pnorm != PG1) dispc=pc+1;
; *else dispc=pc; (dispc points to next byte)

DISASSM             equ       *
                    ldx       PC                  ; address
                    ldaa      0,X                 ; opcode
                    ldab      #PG1
                    cmpa      #$18
                    beq       DISP2               ; jump if page2
                    cmpa      #$1A
                    beq       DISP3               ; jump if page3
                    cmpa      #$CD
                    bne       DISP1               ; jump if not page4
DISP4               incb                          ; set up page value
DISP3               incb
DISP2               incb
                    inx
DISP1               stx       DISPC               ; point to opcode
                    stab      PNORM               ; save page

; *If(opcode == ($00-$5F or $8D or $8F or $CF))
;  if(pnorm == (PG3 or PG4))
;      disillop(); return();
;  b=disrch(opcode,NULL);
;  if(b==0) disillop(); return();

                    ldaa      0,X                 ; get current opcode
                    staa      BASEOP
                    inx
                    stx       DISPC               ; point to next byte
                    cmpa      #$5F
                    bls       DIS1                ; jump if in range
                    cmpa      #$8D
                    beq       DIS1                ; jump if bsr
                    cmpa      #$8F
                    beq       DIS1                ; jump if xgdx
                    cmpa      #$CF
                    beq       DIS1                ; jump if stop
                    jmp       DISGRP              ; try next part of map

DIS1                ldab      PNORM
                    cmpb      #PG3
                    blo       DIS2                ; jump if page 1 or 2
                    jsr       DISILLOP            ; "illegal opcode"
                    rts

DIS2                ldab      BASEOP              ; opcode
                    clrb                          ; class=null
                    jsr       DISRCH
                    tstb
                    bne       DISPEC              ; jump if opcode found
                    jsr       DISILLOP            ; "illegal opcode"
                    rts

;   if(opcode==$8D) dissrch(opcode,REL);
;   if(opcode==($8F or $CF)) disrch(opcode,INH);

DISPEC              ldaa      BASEOP
                    cmpa      #$8D
                    bne       DISPEC1
                    ldab      #REL
                    bra       DISPEC3             ; look for BSR opcode

DISPEC1             cmpa      #$8F
                    beq       DISPEC2             ; jump if XGDX opcode
                    cmpa      #$CF
                    bne       DISINH              ; jump not STOP opcode
DISPEC2             ldab      #INH
DISPEC3             jsr       DISRCH              ; find other entry in table

;   if(class==INH)           /* INH */
;      if(pnorm==PG2)
;         b=disrch(baseop,P2INH);
;         if(b==0) disillop(); return();
;      prntmne();
;      return();

DISINH              equ       *
                    ldab      CLASS
                    cmpb      #INH
                    bne       DISREL              ; jump if not inherent
                    ldab      PNORM
                    cmpb      #PG1
                    beq       DISINH1             ; jump if page1
                    ldaa      BASEOP              ; get opcode
                    ldab      #P2INH              ; class=p2inh
                    jsr       DISRCH
                    tstb
                    bne       DISINH1             ; jump if found
                    jsr       DISILLOP            ; "illegal opcode"
                    rts

DISINH1             jsr       PRNTMNE
                    rts

;   elseif(class=REL)       /* REL */
;      if(pnorm != PG1)
;         disillop(); return();
;      prntmne();
;      disrelad();
;      return();

DISREL              equ       *
                    ldab      CLASS
                    cmpb      #REL
                    bne       DISBTD
                    tst       PNORM
                    beq       DISREL1             ; jump if page1
                    jsr       DISILLOP            ; "illegal opcode"
                    rts

DISREL1             jsr       PRNTMNE             ; output mnemonic
                    jsr       DISRELAD            ; compute relative address
                    rts

;   else           /* SETCLR,SETCLRD,BTB,BTBD */
;      if(class == (SETCLRD or BTBD))
;         if(pnorm != PG1)
;            disillop(); return();   /* illop */
;         prntmne();           /* direct */
;         disdir();           /* output $byte */
;      else (class == (SETCLR or BTB))
;         prntmne();           /* indexed */
;         disindx();
;      outspac();
;      disdir();
;      outspac();
;      if(class == (BTB or BTBD))
;         disrelad();
;   return();

DISBTD              equ       *
                    ldab      CLASS
                    cmpb      #SETCLRD
                    beq       DISBTD1
                    cmpb      #BTBD
                    bne       DISBIT              ; jump not direct bitop
DISBTD1             tst       PNORM
                    beq       DISBTD2             ; jump if page 1
                    jsr       DISILLOP
                    rts

DISBTD2             jsr       PRNTMNE
                    jsr       DISDIR              ; operand(direct)
                    bra       DISBIT1

DISBIT              equ       *
                    jsr       PRNTMNE
                    jsr       DISINDX             ; operand(indexed)
DISBIT1             jsr       OUTSPAC
                    jsr       DISDIR              ; mask
                    ldab      CLASS
                    cmpb      #BTB
                    beq       DISBIT2             ; jump if btb
                    cmpb      #BTBD
                    bne       DISBIT3             ; jump if not bit branch
DISBIT2             jsr       DISRELAD            ; relative address
DISBIT3             rts


; *Elseif($60 <= opcode <= $7F)  /*  GRP2 */
;   if(pnorm == (PG3 or PG4))
;      disillop(); return();
;   if((pnorm==PG2) and (opcode != $6x))
;      disillop(); return();
;   b=disrch(baseop & $6F,NULL);
;   if(b==0) disillop(); return();
;   prntmne();
;   if(opcode == $6x)
;      disindx();
;   else
;      disext();
;   return();

DISGRP              equ       *
                    cmpa      #$7F                ; a=opcode
                    bhi       DISNEXT             ; try next part of map
                    ldab      PNORM
                    cmpb      #PG3
                    blo       DISGRP2             ; jump if page 1 or 2
                    jsr       DISILLOP            ; "illegal opcode"
                    rts

DISGRP2             anda      #$6F                ; mask bit 4
                    clrb                          ; class=null
                    jsr       DISRCH
                    tstb
                    bne       DISGRP3             ; jump if found
                    jsr       DISILLOP            ; "illegal opcode"
                    rts

DISGRP3             jsr       PRNTMNE
                    ldaa      BASEOP              ; get opcode
                    anda      #$F0
                    cmpa      #$60
                    bne       DISGRP4             ; jump if not 6x
                    jsr       DISINDX             ; operand(indexed)
                    rts

DISGRP4             jsr       DISEXT              ; operand(extended)
                    rts

; *Else  ($80 <= opcode <= $FF)
;   if(opcode == ($87 or $C7))
;      disillop(); return();
;   b=disrch(opcode&$CF,NULL);
;   if(b==0) disillop(); return();

DISNEXT             equ       *
                    cmpa      #$87                ; a=opcode
                    beq       DISNEX1
                    cmpa      #$C7
                    bne       DISNEX2
DISNEX1             jsr       DISILLOP            ; "illegal opcode"
                    rts

DISNEX2             anda      #$CF
                    clrb                          ; class=null
                    jsr       DISRCH
                    tstb
                    bne       DISNEW              ; jump if mne found
                    jsr       DISILLOP            ; "illegal opcode"
                    rts

;   if(opcode&$CF==$8D) disrch(baseop,NIMM; (jsr)
;   if(opcode&$CF==$8F) disrch(baseop,NIMM; (sts)
;   if(opcode&$CF==$CF) disrch(baseop,XNIMM; (stx)
;   if(opcode&$CF==$83) disrch(baseop,LIMM); (subd)

DISNEW              ldaa      BASEOP
                    anda      #$CF
                    cmpa      #$8D
                    bne       DISNEW1             ; jump not jsr
                    ldab      #NIMM
                    bra       DISNEW4

DISNEW1             cmpa      #$8F
                    bne       DISNEW2             ; jump not sts
                    ldab      #NIMM
                    bra       DISNEW4

DISNEW2             cmpa      #$CF
                    bne       DISNEW3             ; jump not stx
                    ldab      #XNIMM
                    bra       DISNEW4

DISNEW3             cmpa      #$83
                    bne       DISGEN              ; jump not subd
                    ldab      #LIMM
DISNEW4             bsr       DISRCH
                    tstb
                    bne       DISGEN              ; jump if found
                    jsr       DISILLOP            ; "illegal opcode"
                    rts

;   if(class == (GEN or NIMM or LIMM   ))   /* GEN,NIMM,LIMM,CPD */
;      if(opcode&$CF==$83)
;         if(pnorm==(PG3 or PG4)) disrch(opcode#$CF,CPD)
;         class=LIMM;
;      if((pnorm == (PG2 or PG4) and (opcode != ($Ax or $Ex)))
;         disillop(); return();
;      disgenrl();
;      return();

DISGEN              ldab      CLASS               ; get class
                    cmpb      #GEN
                    beq       DISGEN1
                    cmpb      #NIMM
                    beq       DISGEN1
                    cmpb      #LIMM
                    bne       DISXLN              ; jump if other class
DISGEN1             ldaa      BASEOP
                    anda      #$CF
                    cmpa      #$83
                    bne       DISGEN3             ; jump if not #$83
                    ldab      PNORM
                    cmpb      #PG3
                    blo       DISGEN3             ; jump not pg3 or 4
                    ldab      #CPD
                    bsr       DISRCH              ; look for cpd mne
                    ldab      #LIMM
                    stab      CLASS               ; set class to limm
DISGEN3             ldab      PNORM
                    cmpb      #PG2
                    beq       DISGEN4             ; jump if page 2
                    cmpb      #PG4
                    bne       DISGEN5             ; jump not page 2 or 4
DISGEN4             ldaa      BASEOP
                    anda      #$B0                ; mask bits 6,3-0
                    cmpa      #$A0
                    beq       DISGEN5             ; jump if $Ax or $Ex
                    jsr       DISILLOP            ; "illegal opcode"
                    rts

DISGEN5             jsr       DISGENRL            ; process general class
                    rts

;   else       /* XLIMM,XNIMM,YLIMM,YNIMM */
;      if(pnorm==(PG2 or PG3))
;         if(class==XLIMM) disrch(opcode&$CF,YLIMM);
;         else disrch(opcode&$CF,YNIMM);
;      if((pnorm == (PG3 or PG4))
;         if(opcode != ($Ax or $Ex))
;            disillop(); return();
;      class=LIMM;
;      disgen();
;   return();

DISXLN              ldab      PNORM
                    cmpb      #PG2
                    beq       DISXLN1             ; jump if page2
                    cmpb      #PG3
                    bne       DISXLN4             ; jump not page3
DISXLN1             ldaa      BASEOP
                    anda      #$CF
                    ldab      CLASS
                    cmpb      #XLIMM
                    bne       DISXLN2
                    ldab      #YLIMM
                    bra       DISXLN3             ; look for ylimm

DISXLN2             ldab      #YNIMM              ; look for ynimm
DISXLN3             bsr       DISRCH
DISXLN4             ldab      PNORM
                    cmpb      #PG3
                    blo       DISXLN5             ; jump if page 1 or 2
                    ldaa      BASEOP              ; get opcode
                    anda      #$B0                ; mask bits 6,3-0
                    cmpa      #$A0
                    beq       DISXLN5             ; jump opcode = $Ax or $Ex
                    jsr       DISILLOP            ; "illegal opcode"
                    rts

DISXLN5             ldab      #LIMM
                    stab      CLASS
                    bsr       DISGENRL            ; process general class
                    rts


;******************
; *disrch(a=opcode,b=class)
; *return b=0 if not found
;  else mneptr=points to mnemonic
;        class=class of opcode
;******************
; *x=#MNETABL
; *while(x[0] != eot)
;   if((opcode==x[4]) && ((class=NULL) || (class=x[5])))
;      mneptr=x;
;      class=x[5];
;      return(1);
;   x += 6;
; *return(0);      /* not found */

DISRCH              equ       *
                    ldx       #MNETABL            ; point to top of table
DISRCH1             cmpa      4,X                 ; test opcode
                    bne       DISRCH3             ; jump not this entry
                    tstb
                    beq       DISRCH2             ; jump if class=null
                    cmpb      5,X                 ; test class
                    bne       DISRCH3             ; jump not this entry
DISRCH2             ldab      5,X
                    stab      CLASS
                    stx       MNEPTR              ; return ptr to mnemonic
                    incb
                    rts                           ; return found

DISRCH3             pshb                          ; save class
                    ldab      #6
                    abx
                    ldab      0,X
                    cmpb      #EOT                ; test end of table
                    pulb
                    bne       DISRCH1
                    clrb
                    rts                           ; return not found

;******************
; *prntmne() - output the mnemonic pointed
; *at by mneptr.
;******************
; *outa(mneptr[0-3]);
; *outspac;
; *return();

PRNTMNE             equ       *
                    ldx       MNEPTR
                    ldaa      0,X
                    jsr       OUTA                ; output char1
                    ldaa      1,X
                    jsr       OUTA                ; output char2
                    ldaa      2,X
                    jsr       OUTA                ; output char3
                    ldaa      3,X
                    jsr       OUTA                ; output char4
                    jsr       OUTSPAC
                    rts

;******************
; *disindx() - process indexed mode
;******************
; *disdir();
; *outa(',');
; *if(pnorm == (PG2 or PG4)) outa('Y');
; *else outa('X');
; *return();

DISINDX             equ       *
                    bsr       DISDIR              ; output $byte
                    ldaa      #','
                    jsr       OUTA                ; output ,
                    ldab      PNORM
                    cmpb      #PG2
                    beq       DISIND1             ; jump if page2
                    cmpb      #PG4
                    bne       DISIND2             ; jump if not page4
DISIND1             ldaa      #'Y'
                    bra       DISIND3

DISIND2             ldaa      #'X'
DISIND3             jsr       OUTA                ; output x or y
                    rts

;******************
; *disrelad() - compute and output relative address.
;******************
; braddr = dispc[0] + (dispc++);( 2's comp arith)
; *outa('$');
; *out2bsp(braddr);
; *return();

DISRELAD            equ       *
                    ldx       DISPC
                    ldab      0,X                 ; get relative offset
                    inx
                    stx       DISPC
                    tstb
                    bmi       DISRLD1             ; jump if negative
                    abx
                    bra       DISRLD2

DISRLD1             dex
                    incb
                    bne       DISRLD1             ; subtract
DISRLD2             stx       BRADDR              ; save address
                    jsr       OUTSPAC
                    ldaa      #'$'
                    jsr       OUTA
                    ldx       #BRADDR
                    jsr       OUT2BSP             ; output address
                    rts


;******************
; *disgenrl() - output data for the general cases which
; *includes immediate, direct, indexed, and extended modes.
;******************
; *prntmne();
; *if(baseop == ($8x or $Cx))   /* immediate */
;   outa('#');
;   disdir();
;   if(class == LIMM)
;      out1byt(dispc++);
; *elseif(baseop == ($9x or $Dx))  /* direct */
;   disdir();
; *elseif(baseop == ($Ax or $Ex)) /* indexed */
;   disindx();
; *else  (baseop == ($Bx or $Fx)) /* extended */
;   disext();
; *return();

DISGENRL            equ       *
                    bsr       PRNTMNE             ; print mnemonic
                    ldaa      BASEOP              ; get opcode
                    anda      #$B0                ; mask bits 6,3-0
                    cmpa      #$80
                    bne       DISGRL2             ; jump if not immed
                    ldaa      #'#'                ; do immediate
                    jsr       OUTA
                    bsr       DISDIR
                    ldab      CLASS
                    cmpb      #LIMM
                    beq       DISGRL1             ; jump class = limm
                    rts

DISGRL1             ldx       DISPC
                    jsr       OUT1BYT
                    stx       DISPC
                    rts

DISGRL2             cmpa      #$90
                    bne       DISGRL3             ; jump not direct
                    bsr       DISDIR              ; do direct
                    rts

DISGRL3             cmpa      #$A0
                    bne       DISGRL4             ; jump not indexed
                    bsr       DISINDX             ; do extended
                    rts

DISGRL4             bsr       DISEXT              ; do extended
                    rts

;*****************
; *disdir() - output "$ next byte"
;*****************
DISDIR              equ       *
                    ldaa      #'$'
                    jsr       OUTA
                    ldx       DISPC
                    jsr       OUT1BYT
                    stx       DISPC
                    rts

;*****************
; *disext() - output "$ next 2 bytes"
;*****************
DISEXT              equ       *
                    ldaa      #'$'
                    jsr       OUTA
                    ldx       DISPC
                    jsr       OUT2BSP
                    stx       DISPC
                    rts


;*****************
; *disillop() - output "illegal opcode"
;*****************
DISMSG1             fcc       'ILLOP',EOT
DISILLOP            equ       *
                    pshx
                    ldx       #DISMSG1
                    jsr       OUTSTRG0            ; no cr
                    pulx
                    rts



;**********
;   help  -  List buffalo commands to terminal.
;**********
HELP                equ       *
                    ldx       #HELPMSG1
                    jsr       OUTSTRG             ; print help screen
                    rts

HELPMSG1            equ       *
                    fcc       'ASM [<addr>]  Line asm/disasm'
                    fcb       $0D
                    fcc       '  [/,=]  Same addr,       [^,-]  Prev addr,       [+,CTLJ] Next addr'
                    fcb       $0D
                    fcc       '  [CR]  Next opcode,                              [CTLA,.]  Quit'
                    fcb       $0D
                    fcc       'BF <addr1> <addr2> [<data>]  Block fill memory'
                    fcb       $0D
                    fcc       'BR [-][<addr>] Set up bkpt table'
                    fcb       $0D
                    fcc       'BULK  Erase EEPROM,                 BULKALL  Erase EEPROM and CONFIG'
                    fcb       $0D
                    fcc       'CALL [<addr>] Call subroutine'
                    fcb       $0D
                    fcc       'GO [<addr>] Execute code at addr,        PROCEED  Continue execution'
                    fcb       $0D
                    fcc       'EEMOD [<addr> [<addr>]] Modify EEPROM range'
                    fcb       $0D
                    fcc       'LOAD, VERIFY [T] <host dwnld command>  Load or verify S-records'
                    fcb       $0D
                    fcc       'MD [<addr1> [<addr2>]]  Memory dump'
                    fcb       $0D
                    fcc       'MM [<addr>] or [<addr>]/  Memory Modify'
                    fcb       $0D
                    fcc       '  [/,=]  Same addr,  [^,-,CTLH] Prev addr,  [+,CTLJ,SPACE] Next addr'
                    fcb       $0D
                    fcc       '  <addr>O Compute offset,                   [CR]  Quit'
                    fcb       $0D
                    fcc       'MOVE <s1> <s2> [<d>]  Block move'
                    fcb       $0D
                    fcc       'OFFSET [-]<arg>  Offset for download'
                    fcb       $0D
                    fcc       'RM [P,Y,X,A,B,C,S]  Register modify'
                    fcb       $0D
                    fcc       'STOPAT <addr>  Trace until addr'
                    fcb       $0D
                    fcc       'T [<n>]  Trace n instructions'
                    fcb       $0D
                    fcc       'TM  Transparent mode (CTLA = exit, CTLB = send brk)'
                    fcb       $0D
                    fcc       '[CTLW]  Wait,          [CTLX,DEL] Abort         [CR] Repeat last cmd'
                    fcb       $0D
                    fcb       4



;**********
;   call [<addr>] - Execute a jsr to <addr> or user
; *pc value.  Return to monitor via  rts or breakpoint.
;**********
; *a = wskip();
; *if(a != cr)
;     a = buffarg();
;     a = wskip();
;     if(a != cr) return(bad argument)
;     pc = shftreg;
CALL                jsr       WSKIP
                    beq       CALL3               ; jump if no arg
                    jsr       BUFFARG
                    jsr       WSKIP
                    beq       CALL2               ; jump if cr
                    ldx       #MSG9               ; "bad argument"
                    jsr       OUTSTRG
                    rts

CALL2               ldx       SHFTREG
                    stx       REGS                ; pc = <addr>

; *put return address on user stack
; *setbps();
; *restack();     /* restack and go*/
CALL3               ldx       SP
                    dex                           ; user stack pointer
                    ldd       #RETURN             ; return address
                    std       0,X
                    dex
                    stx       SP                  ; new user stack pointer
                    bsr       SETBPS
                    clr       TMP2                ; 1=go, 0=call
                    jmp       RESTACK             ; go to user code

;**********
;   return() - Return here from rts after
; *call command.
;**********
RETURN              psha                          ; save a register
                    tpa
                    staa      REGS+8              ; cc register
                    sei                           ; mask interrupts
                    pula
                    std       REGS+6              ; a and b registers
                    stx       REGS+4              ; x register
                    sty       REGS+2              ; y register
                    sts       SP                  ; user stack pointer
                    lds       PTR2                ; monitor stack pointer
                    jsr       REMBPS              ; remove breakpoints
                    jsr       OUTCRLF
                    jsr       RPRINT              ; print user registers
                    rts


;**********
;   proceed - Same as go except it ignores
; *a breakpoint at the first opcode.  Calls
; *runone for the first instruction only.
;**********
PROCEED             equ       *
                    jsr       RUNONE              ; run one instruction
                    jsr       CHKABRT             ; check for abort
                    clr       TMP2                ; flag for breakpoints
                    inc       TMP2                ; 1=go 0=call
                    bsr       SETBPS
                    jmp       RESTACK             ; go execute

;**********
;   go [<addr>] - Execute starting at <addr> or
; *user's pc value.  Executes an rti to user code.
; *Returns to monitor via an swi through swiin.
;**********
; *a = wskip();
; *if(a != cr)
;     a = buffarg();
;     a = wskip();
;     if(a != cr) return(bad argument)
;     pc = shftreg;
; *setbps();
; *restack();     /* restack and go*/
GO                  jsr       WSKIP
                    beq       GO2                 ; jump if no arg
                    jsr       BUFFARG
                    jsr       WSKIP
                    beq       GO1                 ; jump if cr
                    ldx       #MSG9               ; "bad argument"
                    jsr       OUTSTRG
                    rts

GO1                 ldx       SHFTREG
                    stx       REGS                ; pc = <addr>
GO2                 clr       TMP2
                    inc       TMP2                ; 1=go, 0=call
                    bsr       SETBPS
                    jmp       RESTACK             ; go to user code

;*****
;** SWIIN - Breakpoints from go or call commands enter here.
; *Remove breakpoints, save user registers, return
SWIIN               equ       *                   ; swi entry point
                    tsx                           ; user sp -> x
                    lds       PTR2                ; restore monitor sp
                    jsr       SAVSTACK            ; save user regs
                    bsr       REMBPS              ; remove breakpoints from code
                    ldx       REGS
                    dex
                    stx       REGS                ; save user pc value

; *if(call command) remove call return addr from user stack;
                    tst       TMP2                ; 1=go, 0=call
                    bne       GO3                 ; jump if go command
                    ldx       SP                  ; remove return address
                    inx                           ; user stack pointer
                    inx
                    stx       SP
GO3                 jsr       OUTCRLF             ; print register values
                    jsr       RPRINT
                    rts                           ; done

;**********
;  setbps - Replace user code with swi's at
; *breakpoint addresses.
;**********
; *for(b=0; b=6; b =+ 2)
;     x = brktabl[b];
;     if(x != 0)
;          optabl[b] = x[0];
;          x[0] = $3F;
; *Put monitor SWI vector into jump table

SETBPS              clrb
SETBPS1             ldx       #BRKTABL
                    ldy       #PTR4
                    abx
                    aby
                    ldx       0,X                 ; breakpoint table entry
                    beq       SETBPS2             ; jump if 0
                    ldaa      0,X                 ; save user opcode
                    staa      0,Y
                    ldaa      #SWI
                    jsr       WRITE               ; insert swi into code
SETBPS2             addb      #$2
                    cmpb      #$6
                    ble       SETBPS1             ; loop 4 times
                    ldx       JSWI+1
                    stx       PTR3                ; save user swi vector
                    ldaa      #$7E                ; jmp opcode
                    staa      JSWI
                    ldx       #SWIIN
                    stx       JSWI+1              ; monitor swi vector
                    rts

;**********
;   rembps - Remove breakpoints from user code.
;**********
; *for(b=0; b=6; b =+ 2)
;     x = brktabl[b];
;     if(x != 0)
;          x[0] = optabl[b];
; *Replace user's SWI vector
REMBPS              clrb
REMBPS1             ldx       #BRKTABL
                    ldy       #PTR4
                    abx
                    aby
                    ldx       0,X                 ; breakpoint table entry
                    beq       REMBPS2             ; jump if 0
                    ldaa      0,Y
                    jsr       WRITE               ; restore user opcode
REMBPS2             addb      #$2
                    cmpb      #$6
                    ble       REMBPS1             ; loop 4 times
                    ldx       PTR3                ; restore user swi vector
                    stx       JSWI+1
                    rts


;**********
;   trace <n> - Trace n instructions starting
; *at user's pc value. n is a hex number less than
; *$FF (defaults to 1).
;**********
; *a = wskip();
; *if(a != cr)
;     a = buffarg(); a = wskip();
;     if(a != cr) return(bad argument);
;     countt1 = n
TRACE               clr       TMP4
                    inc       TMP4                ; default count=1
                    clr       CHRCNT              ; set up for display
                    jsr       WSKIP
                    beq       TRACE2              ; jump if cr
                    jsr       BUFFARG
                    jsr       WSKIP
                    beq       TRACE1              ; jump if cr
                    ldx       #MSG9               ; "bad argument"
                    jsr       OUTSTRG
                    rts

TRACE1              ldaa      SHFTREG+1           ; n
                    staa      TMP4

; *Disassemble the line about to be traced
TRACE2              equ       *
                    ldab      TMP4
                    pshb
                    ldx       REGS
                    stx       PTR1                ; pc value for disass
                    jsr       DISASSM
                    pulb
                    stab      TMP4

; *run one instruction
; *rprint();
; *while(count > 0) continue trace;
                    bsr       RUNONE
                    jsr       CHKABRT             ; check for abort
                    jsr       TABTO               ; print registers for
                    jsr       RPRINT              ; result of trace
                    dec       TMP4
                    beq       TRACDON             ; quit if count=0
TRACE3              jsr       OUTCRLF
                    bra       TRACE2

TRACDON             rts


;**********
;   stopat <addr> - Trace instructions until <addr>
; *is reached.
;**********
; *if((a=wskip) != cr)
;     a = buffarg(); a = wskip();
;     if(a != cr) return(bad argument);
; *else return(bad argument);
STOPAT              equ       *
                    jsr       WSKIP
                    beq       STOPGO              ; jump if cr - no argument
                    jsr       BUFFARG
                    jsr       WSKIP
                    beq       STOPAT1             ; jump if cr
                    ldx       #MSG9               ; "bad argument"
                    jsr       OUTSTRG
                    rts

STOPAT1             tst       COUNT
                    beq       STOPGO              ; jump if no argument
                    ldx       SHFTREG
                    stx       PTRMEM              ; update "current location"

; *while(!(ptrmem <= userpc < ptrmem+10)) runone();
; *rprint();
STOPGO              ldd       REGS                ; userpc
                    cpd       PTRMEM
                    blo       STOPNEXT            ; if(userpc < ptrmem) runone
                    ldd       PTRMEM
                    addd      #10
                    cpd       REGS
                    bhi       STOPDON             ; quit if ptrmem+10 > userpc
STOPNEXT            bsr       RUNONE
                    jsr       CHKABRT             ; check for abort
                    bra       STOPGO

STOPDON             jsr       OUTCRLF
                    jsr       RPRINT              ; result of trace
                    rts                           ; done


;*************************
; runone - This routine is used by the trace and
; execute commands to run one only one user instruction.
;   Control is passed to the user code via an RTI.  OC5
; is then used to trigger an XIRQ as soon as the first user
; opcode is fetched.  Control then returns to the monitor
; through XIRQIN.
;  Externally, the OC5 pin must be wired to the XIRQ pin.
;************************
; Disable oc5 interrupts
; Put monitor XIRQ vector into jump table
; Unmask x bit in user ccr
; Setup OC5 to go low when first user instruction executed
RUNONE              equ       *
                    ldaa      #$7E                ; put "jmp xirqin" in jump table
                    staa      JTOC5
                    ldx       #XIRQIN
                    stx       JXIRQ+1
                    ldaa      REGS+8              ; x bit will be cleared when
                    anda      #$BF                ; rti is executed below
                    staa      REGS+8
                    ldab      #87                 ; cycles to end of rti
                    ldx       TCNT
                    abx                           ; 3~ \
                    stx       TOC5                ; oc5 match register 5~ \
                    ldaa      TCTL1               ; 4~ \
                    anda      #$FE                ; set up oc5 low on match 2~ \
                    staa      TCTL1               ; enable oc5 interrupt 4~ / 86~

;** RESTACK - Restore user stack and RTI to user code.
; This code is the pathway to execution of user code.
; *(Force extended addressing to maintain cycle count)
; *Restore user stack and rti to user code
RESTACK             equ       *                   ; 68~
                    sts       >PTR2               ; save monitor sp
                    lds       >SP                 ; user stack pointer
                    ldx       >REGS
                    pshx                          ; pc
                    ldx       >REGS+2
                    pshx                          ; y
                    ldx       >REGS+4
                    pshx                          ; x
                    ldd       >REGS+6
                    psha                          ; a
                    pshb                          ; b
                    ldaa      >REGS+8
                    psha                          ; ccr
                    rti

;** Return here from run one line of user code.
XIRQIN              equ       *
                    tsx                           ; user sp -> x
                    lds       PTR2                ; restore monitor sp

;** SAVSTACK - Save user's registers.
; On entry - x points to top of user stack.
SAVSTACK            equ       *
                    ldaa      0,X
                    staa      REGS+8              ; user ccr
                    ldd       1,X
                    staa      REGS+7              ; b
                    stab      REGS+6              ; a
                    ldd       3,X
                    std       REGS+4              ; x
                    ldd       5,X
                    std       REGS+2              ; y
                    ldd       7,X
                    std       REGS                ; pc
                    ldab      #8
                    abx
                    stx       SP                  ; user stack pointer
                    ldaa      TCTL1               ; force oc5 pin high which
                    oraa      #$03                ; is tied to xirq line
                    staa      TCTL1
                    ldaa      #$08
                    staa      CFORC
                    rts


;**********
;   HOST() - Establishes transparent link between
;       terminal and host.  Port used for host is
;       determined in the reset initialization routine
;       and stored in HOSTDEV.
;          To exit type control A.
;          To send break to host type control B.
; *if(no external device) return;
; *initialize host port;
; *While( !(control A))
;     input(terminal); output(host);
;     input(host); output(terminal);

HOST                ldaa      EXTDEV
                    bne       HOST0               ; jump if host port avail.
                    ldx       #MSG10              ; "no host port avail"
                    jsr       OUTSTRG
                    rts

HOST0               clr       AUTOLF              ; turn off autolf
;          JSR  HOSTCO    connect sci (evb board)
                    bsr       HOSTINIT            ; initialize host port
HOST1               jsr       INPUT               ; read terminal
                    tsta
                    beq       HOST3               ; jump if no char
                    cmpa      #CTLA
                    beq       HOSTEND             ; jump if control a
                    cmpa      #CTLB
                    bne       HOST2               ; jump if not control b
                    bsr       TXBREAK             ; send break to host
                    bra       HOST3

HOST2               bsr       HOSTOUT             ; echo to host
HOST3               bsr       HOSTIN              ; read host
                    tsta
                    beq       HOST1               ; jump if no char
                    jsr       OUTPUT              ; echo to terminal
                    bra       HOST1

HOSTEND             inc       AUTOLF              ; turn on autolf
;          JSR  TARGCO    disconnect sci (evb board)
                    rts                           ; return

;**********
; txbreak() - transmit break to host port.
; The duration of the transmitted break is
; approximately 200,000 E-clock cycles, or
; 100ms at 2.0 MHz.
;***********
TXBREAK             equ       *
                    ldaa      HOSTDEV
                    cmpa      #$03
                    beq       TXBDU               ; jump if duartb is host

TXBSCI              ldx       #SCCR2              ; sci is host
                    bset      0,X,$01             ; set send break bit
                    bsr       TXBWAIT
                    bclr      0,X,$01             ; clear send break bit
                    bra       TXB1

TXBDU               ldx       #PORTB              ; duart host port
                    ldaa      #$60                ; start break cmd
                    staa      2,X                 ; port b command register
                    bsr       TXBWAIT
                    ldaa      #$70                ; stop break cmd
                    staa      2,X                 ; port b command register

TXB1                ldaa      #$0D
                    bsr       HOSTOUT             ; send carriage return
                    ldaa      #$0A
                    bsr       HOSTOUT             ; send linefeed
                    rts

TXBWAIT             ldy       #$6F9B              ; loop count = 28571
TXBWAIT1            dey                           ; 7 cycle loop
                    bne       TXBWAIT1
                    rts


;**********
;   hostinit(), hostin(), hostout() - host i/o
; *routines.  Restores original terminal device.
;**********
HOSTINIT            ldab      IODEV               ; save terminal
                    pshb
                    ldab      HOSTDEV
                    stab      IODEV               ; point to host
                    jsr       INIT                ; initialize host
                    bra       TERMRES             ; restore terminal

HOSTIN              ldab      IODEV               ; save terminal
                    pshb
                    ldab      HOSTDEV
                    stab      IODEV               ; point to host
                    jsr       INPUT               ; read host
                    bra       TERMRES             ; restore terminal

HOSTOUT             ldab      IODEV               ; save terminal
                    pshb
                    ldab      HOSTDEV
                    stab      IODEV               ; point to host
                    jsr       OUTPUT              ; write to host
TERMRES             pulb                          ; restore terminal device
                    stab      IODEV
                    rts


;**********
;   load(ptrbuff[]) - Load s1/s9 records from
; *host to memory.  Ptrbuff[] points to string in
; *input buffer which is a command to output s1/s9
; *records from the host ("cat filename" for unix).
;    Returns error and address if it can't write
; *to a particular location.
;**********
;   verify(ptrbuff[]) - Verify memory from load
; *command.  Ptrbuff[] is same as for load.
; tmp3 is used as an error indication, 0=no errors,
; 1=receiver, 2=rom error, 3=checksum error.
;**********
VERIFY              clr       TMP2
                    inc       TMP2                ; TMP2=1=verify
                    bra       LOAD1

LOAD                clr       TMP2                ; 0=load

; *a=wskip();
; *if(a = cr) goto transparent mode;
; *if(t option) hostdev = iodev;
LOAD1               equ       *
                    clr       TMP3                ; clear error flag
                    jsr       WSKIP
                    bne       LOAD2
                    jmp       HOST                ; go to host if no args

LOAD2               jsr       UPCASE
                    cmpa      #'T'                ; look for t option
                    bne       LOAD3               ; jump not t option
                    jsr       INCBUFF
                    jsr       READBUFF            ; get next character
                    jsr       DECBUFF
                    cmpa      #$0D
                    bne       LOAD3               ; jump if not t option
                    clr       AUTOLF
                    ldaa      IODEV
                    staa      HOSTDEV             ; set host port = terminal
                    bra       LOAD10              ; go wait for s1 records

; *else while(not cr)
;     read character from input buffer;
;     send character to host;
LOAD3               clr       AUTOLF
;          JSR HOSTCO    connect sci (evb board)
                    bsr       HOSTINIT            ; initialize host port
LOAD4               jsr       READBUFF            ; get next char
                    jsr       INCBUFF
                    psha                          ; save char
                    bsr       HOSTOUT             ; output to host
                    jsr       OUTPUT              ; echo to terminal
                    pula
                    cmpa      #$0D
                    bne       LOAD4               ; jump if not cr

; *repeat:                           /* look for s records */
;      if(hostdev != iodev) check abort;
;      a = hostin();
;      if(a = 'S')
;          a = hostin;
;          if(a = '1')
;              checksum = 0;
;              get byte count in b;
;              get base address in x;
;              while(byte count > 0)
;                  byte();
;                  x++; b--;
;                  if(tmp3=0)           /* no error */
;                      if(load) x[0] = shftreg+1;
;                      if(x[0] != shftreg+1)
;                          tmp3 = 2;    /* rom error */
;                          ptr3 = x;    /* save address */
;              if(tmp3 = 0) do checksum;
;              if(checksum err) tmp3 = 3; /* checksum error */
;** Look for s-record header
LOAD10              equ       *
                    ldaa      HOSTDEV
                    cmpa      IODEV
                    beq       LOAD11              ; jump if hostdev=iodev
                    jsr       CHKABRT             ; check for abort
LOAD11              bsr       HOSTIN              ; read host
                    tsta
                    beq       LOAD10              ; jump if no input
                    cmpa      #'S'
                    bne       LOAD10              ; jump if not S
LOAD12              bsr       HOSTIN              ; read host
                    tsta
                    beq       LOAD12              ; jump if no input
                    cmpa      #'9'
                    beq       LOAD90              ; jump if S9 record
                    cmpa      #'1'
                    bne       LOAD10              ; jump if not S1
                    clr       TMP4                ; clear checksum
;** Get Byte Count and Starting Address
                    jsr       BYTE
                    ldab      SHFTREG+1
                    subb      #$2                 ; b = byte count
                    bsr       BYTE
                    bsr       BYTE
                    pshb                          ; save byte count
                    ldd       SHFTREG
                    addd      LDOFFST             ; add offset
                    xgdx                          ; x = address+offset
                    pulb                          ; restore byte count
                    dex                           ; condition for loop
;** Get and Store Incoming Data Byte
LOAD20              bsr       BYTE                ; get next byte
                    inx
                    decb                          ; check byte count
                    beq       LOAD30              ; if b=0, go do checksum
                    tst       TMP3
                    bne       LOAD10              ; jump if error flagged
                    tst       TMP2
                    bne       LOAD21              ; jump if verify
                    ldaa      SHFTREG+1
                    jsr       WRITE               ; load only
LOAD21              cmpa      0,X                 ; verify ram location
                    beq       LOAD20              ; jump if ram ok
                    ldaa      #$02
                    staa      TMP3                ; indicate rom error
                    stx       PTR3                ; save error address
                    bra       LOAD20              ; finish download

;** Get and Test Checksum

LOAD30              tst       TMP3
                    bne       LOAD10              ; jump if error already
                    ldaa      TMP4
                    inca                          ; do checksum
                    beq       LOAD10              ; jump if s1 record okay
                    ldaa      #$03
                    staa      TMP3                ; indicate checksum error
                    bra       LOAD10

;          if(a = '9')
;              read rest of record;
;              if(tmp3=2) return("[ptr3]");
;              if(tmp3=1) return("rcv error");
;              if(tmp3=3) return("checksum err");
;              else return("done");
LOAD90              bsr       BYTE
                    ldab      SHFTREG+1           ; b = byte count
LOAD91              bsr       BYTE
                    decb
                    bne       LOAD91              ; loop until end of record
                    ldab      #$64
LOAD91A             jsr       DLY10MS             ; delay 1 sec -let host finish
                    decb
                    bne       LOAD91A
                    jsr       INPUT               ; clear comm device
                    ldd       #$7E0D              ; put dummy command in inbuff
                    std       INBUFF
                    inc       AUTOLF              ; turn on autolf
;         JSR  TARGCO    disconnect sci (evb)
                    ldx       #MSG11              ; "done" default msg
                    ldaa      TMP3
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
LOAD94              jsr       OUTSTRG
LOAD95              rts


;**********
;  byte() -  Read 2 ascii bytes from host and
; *convert to one hex byte.  Returns byte
; *shifted into shftreg and added to tmp4.
;**********
BYTE                pshb
                    pshx
BYTE0               jsr       HOSTIN              ; read host (1st byte)
                    tsta
                    beq       BYTE0               ; loop until input
                    jsr       HEXBIN
BYTE1               jsr       HOSTIN              ; read host (2nd byte)
                    tsta
                    beq       BYTE1               ; loop until input
                    jsr       HEXBIN
                    ldaa      SHFTREG+1
                    adda      TMP4
                    staa      TMP4                ; add to checksum
                    pulx
                    pulb
                    rts


;**********
;   offset [<addr>]
; Specify offset to be added to s-record address when
; downloading from the host.
;  OFFSET                -show the current offset
;  OFFSET <data>         -current offset = data
;  OFFSET -<data>        -current offset = 0 - data
;**********
; *if(<data>) then offset = data;
; *print(offset);
OFFSET              equ       *
                    clr       TMP4                ; minus indicator
                    jsr       WSKIP
                    beq       OFFST3              ; jump if cr (no argument)
                    cmpa      #'-'
                    bne       OFFST1              ; jump not -
                    inc       TMP4                ; set minus sign flag
                    jsr       INCBUFF             ; move buffer pointer
                    jsr       WSKIP
OFFST1              jsr       BUFFARG             ; read argument
                    tst       COUNT
                    beq       OFFSTER             ; jump if bad argument
                    jsr       WSKIP
                    bne       OFFSTER             ; jump if not cr
                    ldd       SHFTREG             ; get offset value
                    tst       TMP4
                    beq       OFFST2              ; jump if positive
                    ldd       #$0000              ; negative - sub from 0
                    subd      SHFTREG
OFFST2              std       LDOFFST
OFFST3              jsr       OUTCRLF             ; display current offset
                    ldx       #LDOFFST
                    jsr       OUT2BSP
                    rts

OFFSTER             ldx       #MSG9               ; "bad argument"
                    jsr       OUTSTRG
                    rts





;**********
;   register [<name>]  - prints the user regs
; *and opens them for modification.  <name> is
; *the first register opened (default = P).
;   Subcommands:
; [<nn>]<space>  Opens the next register.
; [<nn>]<cr>     Return.
;    The register value is only changed if
;    <nn> is entered before the subcommand.
;**********
; *x[] = reglist
; *a = wskip(); a = upcase(a);
; *if(a != cr)
;     while( a != x[0] )
;          if( x[0] = "s") return(bad argument);
;          x[]++;
;     incbuff(); a = wskip();
;     if(a != cr) return(bad argument);

REGISTER            ldx       #REGLIST
                    jsr       WSKIP               ; a = first char of arg
                    jsr       UPCASE              ; convert to upper case
                    cmpa      #$D
                    beq       REG4                ; jump if no argument
REG1                cmpa      0,X
                    beq       REG3
                    ldab      0,X
                    inx
                    cmpb      #'S'
                    bne       REG1                ; jump if not "s"
REG2                ldx       #MSG9               ; "bad argument"
                    jsr       OUTSTRG
                    rts

REG3                pshx
                    jsr       INCBUFF
                    jsr       WSKIP               ; next char after arg
                    pulx
                    bne       REG2                ; jump if not cr

; *rprint();
;     while(x[0] != "s")
;          rprnt1(x);
;          a = termarg();    /* read from terminal */
;          if( ! dchek(a) ) return(bad argument);
;          if(countu1 != 0)
;               if(x[14] = 1)
;                    regs[x[7]++ = shftreg;
;               regs[x[7]] = shftreg+1;
;          if(a = cr) break;
; *return;

REG4                jsr       RPRINT              ; print all registers
REG5                jsr       OUTCRLF
                    jsr       RPRNT1              ; print reg name
                    clr       SHFTREG
                    clr       SHFTREG+1
                    jsr       TERMARG             ; read subcommand
                    jsr       DCHEK
                    beq       REG6                ; jump if delimeter
                    ldx       #MSG9               ; "bad argument"
                    jsr       OUTSTRG
                    rts

REG6                psha
                    pshx
                    tst       COUNT
                    beq       REG8                ; jump if no input
                    ldab      7,X                 ; get reg offset
                    ldaa      14,X                ; byte size
                    ldx       #REGS               ; user registers
                    abx
                    tsta
                    beq       REG7                ; jump if 1 byte reg
                    ldaa      SHFTREG
                    staa      0,X                 ; put in top byte
                    inx
REG7                ldaa      SHFTREG+1
                    staa      0,X                 ; put in bottom byte
REG8                pulx
                    pula
                    ldab      0,X                 ; CHECK FOR REGISTER S
                    cmpb      #'S'
                    beq       REG9                ; jump if "s"
                    inx                           ; point to next register
                    cmpa      #$D
                    bne       REG5                ; jump if not cr
REG9                rts


; Equates
JPORTD              equ       $08
JDDRD               equ       $09
JBAUD               equ       $2B
JSCCR1              equ       $2C
JSCCR2              equ       $2D
JSCSR               equ       $2E
JSCDAT              equ       $2F
;

;************
;  xboot [<addr1> [<addr2>]] - Use SCI to talk to an 'hc11 in
; boot mode.  Downloads bytes from addr1 thru addr2.
; Default addr1 = $C000 and addr2 = $C0ff.
;
; IMPORTANT:
; if talking to an 'A8 or 'A2: use either default addresses or ONLY
;    addr1 - this sends 256 bytes
; if talking to an 'E9: include BOTH addr1 and addr2 for variable
;    length
;************

; *Get arguments
; *If no args, default $C000
BOOT                jsr       WSKIP
                    bne       BOT1                ; jump if arguments
                    ldx       #$C0FF              ; addr2 default
                    stx       PTR5
                    ldy       #$C000              ; addr1 default
                    bra       BOT2                ; go - use default address

; *Else get arguments
BOT1                jsr       BUFFARG
                    tst       COUNT
                    beq       BOTERR              ; jump if no address
                    ldy       SHFTREG             ; start address (addr1)
                    jsr       WSKIP
                    bne       BOT1A               ; go get addr2
                    sty       PTR5                ; default addr2...
                    ldd       PTR5                ; ...by taking addr1...
                    addd      #$FF                ; ...and adding 255 to it...
                    std       PTR5                ; ...for a total download of 256
                    bra       BOT2                ; continue

;

BOT1A               jsr       BUFFARG
                    tst       COUNT
                    beq       BOTERR              ; jump if no address
                    ldx       SHFTREG             ; end address (addr2)
                    stx       PTR5
                    jsr       WSKIP
                    bne       BOTERR              ; go use addr1 and addr2
                    bra       BOT2

;
BOTERR              ldx       #MSG9               ; "bad argument"
                    jsr       OUTSTRG
                    rts

; *Boot routine
BOT2                ldab      #$FF                ; control character ($ff -> download)
                    bsr       BTSUB               ; set up SCI and send control char
;                        initializes X as register pointer
; *Download block
BLOP                ldaa      0,Y
                    staa      JSCDAT,X            ; write to transmitter
                    brclr     JSCSR,X,$80,*       ; wait for TDRE
                    cpy       PTR5                ; if last...
                    beq       BTDONE              ; ...quit
                    iny                           ; else...
                    bra       BLOP                ; ...send next

BTDONE              rts

;************************************************
; *Subroutine
;  btsub   - sets up SCI and outputs control character
; On entry, B = control character
; On exit,  X = $1000
;           A = $0C
;***************************

BTSUB               equ       *
                    ldx       #$1000              ; to use indexed addressing
                    ldaa      #$02
                    staa      JPORTD,X            ; drive transmitter line
                    staa      JDDRD,X             ; high
                    clr       JSCCR2,X            ; turn off XMTR and RCVR
                    ldaa      #$22                ; BAUD = /16
                    staa      JBAUD,X
                    ldaa      #$0C                ; TURN ON XMTR & RCVR
                    staa      JSCCR2,X
                    stab      JSCDAT,X
                    brclr     JSCSR,X,$80,*       ; wait for TDRE
                    rts


;***********
; TILDE - This command is put into the combuff by the
; load command so that extraneous carriage returns after
; the load will not hang up.
TILDE               rts

;******************
;
;       EVBTEST - This routine makes it a little easier
;       on us to test this board.
;
;******************

EVBTEST             ldaa      #$FF
                    staa      $1000               ; Write ones to port A
                    clr       AUTOLF              ; Turn off auto lf
;         JSR  HOSTCO    Connect host
                    jsr       HOSTINIT            ; Initialize host
                    ldaa      #$7f
                    jsr       HOSTOUT             ; Send Delete to Altos
                    ldaa      #$0d
                    jsr       HOSTOUT             ; Send <CR>
                    inc       AUTOLF              ; Turn on Auto LF
                    ldx       #INBUFF+5           ; Point at Load message
                    stx       PTR0                ; Set pointer for load command
                    ldy       #MSGEVB             ; Point at cat line
LOOP                ldaa      0,Y                 ; Loop to xfer command line
                    cmpa      #04                 ; Into buffalo line buffer
                    beq       DONE                ; Quit on $04
                    staa      0,X
                    inx                           ; next character
                    iny
                    bra       LOOP

DONE                clr       TMP2                ; Set load vs. verify
                    jsr       LOAD3               ; Jmp into middle of load
                    lds       #STACK              ; Reset Stack
                    jmp       $C0B3               ; Jump to Downloaded code

MSGEVB              fcc       'cat evbtest.out'
                    fcb       $0D
                    fcb       $04

;*** Jump table ***
                    org       ROMBS+$1F7C
.WARMST             jmp       MAIN                ; warm start
.BPCLR              jmp       BPCLR               ; clear breakpoint table
.RPRINT             jmp       RPRINT              ; display user registers
.HEXBIN             jmp       HEXBIN              ; convert ascii hex char to binary
.BUFFAR             jmp       BUFFARG             ; build hex argument from buffer
.TERMAR             jmp       TERMARG             ; read hex argument from terminal
.CHGBYT             jmp       CHGBYT              ; modify memory at address in x
.READBU             jmp       READBUFF            ; read character from buffer
.INCBUF             jmp       INCBUFF             ; increment buffer pointer
.DECBUF             jmp       DECBUFF             ; decrement buffer pointer
.WSKIP              jmp       WSKIP               ; find non-whitespace char in buffer
.CHKABR             jmp       CHKABRT             ; check for abort from terminal
.UPCASE             jmp       UPCASE              ; convert to upper case
.WCHEK              jmp       WCHEK               ; check for white space
.DCHEK              jmp       DCHEK               ; check for delimeter
.INIT               jmp       INIT                ; initialize i/o device
.INPUT              jmp       INPUT               ; low level input routine
.OUTPUT             jmp       OUTPUT              ; low level output routine
.OUTLHL             jmp       OUTLHLF             ; display top 4 bits as hex digit
.OUTRHL             jmp       OUTRHLF             ; display bottom 4 bits as hex digit
.OUTA               jmp       OUTA                ; output ascii character in A
.OUT1BY             jmp       OUT1BYT             ; display the hex value of byte at X
.OUT1BS             jmp       OUT1BSP             ; out1byt followed by space
.OUT2BS             jmp       OUT2BSP             ; display 2 hex bytes at x and a space
.OUTCRL             jmp       OUTCRLF             ; carriage return, line feed to terminal
.OUTSTR             jmp       OUTSTRG             ; display string at X (term with $04)
.OUTST0             jmp       OUTSTRG0            ; outstrg with no initial carr ret
.INCHAR             jmp       INCHAR              ; wait for and input a char from term
.VECINT             jmp       VECINIT             ; initialize RAM vector table

                    org       ROMBS+$1FD6
;*** Vectors ***
VSCI                fdb       JSCI
VSPI                fdb       JSPI
VPAIE               fdb       JPAIE
VPAO                fdb       JPAO
VTOF                fdb       JTOF
VTOC5               fdb       JTOC5
VTOC4               fdb       JTOC4
VTOC3               fdb       JTOC3
VTOC2               fdb       JTOC2
VTOC1               fdb       JTOC1
VTIC3               fdb       JTIC3
VTIC2               fdb       JTIC2
VTIC1               fdb       JTIC1
VRTI                fdb       JRTI
VIRQ                fdb       JIRQ
VXIRQ               fdb       JXIRQ
VSWI                fdb       JSWI
VILLOP              fdb       JILLOP
VCOP                fdb       JCOP
VCLM                fdb       JCLM
VRST                fdb       BUFFALO

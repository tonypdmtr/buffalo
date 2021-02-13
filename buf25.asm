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
;
;****************************************************
;    Although the information contained herein,    *
;    as well as any information provided relative  *
;    thereto, has been carefully reviewed and is   *
;    believed accurate, Motorola assumes no        *
;    liability arising out of its application or   *
;    use, neither does it convey any license under *
;    its patent rights nor the rights of others.   *
;****************************************************
                    #ExtraOn
                    #OptRtsOff
;***************
;   EQUATES   *
;***************
RAMBS               equ       $0000               ; start of ram
REGBS               equ       $1000               ; start of registers
ROMBS               equ       $E000               ; start of rom
PORTE               equ       REGBS+$0A           ; port e
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
;     RAM     *
;***************
                    org       $36
;*** Buffalo ram space ***
                    rmb       20                  ; user stack area
USTACK              rmb       30                  ; monitor stack area
STACK               rmb       1
INBUFF              rmb       BUFFLNG             ; input buffer
ENDBUFF             equ       *
COMBUFF             rmb       8                   ; command buffer
SHFTREG             rmb       2                   ; input shift register
BRKTABL             rmb       8                   ; breakpoint table
REGS                rmb       9                   ; user's pc,y,x,a,b,c
SP                  rmb       2                   ; user's sp
AUTOLF              rmb       1                   ; auto lf flag for i/o
IODEV               rmb       1                   ; 0=sci, 1=acia, 2=duartA, 3=duartB
EXTDEV              rmb       1                   ; 0=none, 1=acia, 2=duart,
HOSTDEV             rmb       1                   ; 0=sci, 1=acia, 3=duartB
COUNT               rmb       1                   ; # characters read
PTRMEM              rmb       2                   ; current memory location

;*** Buffalo variables - used by: ***
PTR0                rmb       2                   ; main,readbuff,incbuff,AS
PTR1                rmb       2                   ; main,BR,DU,MO,AS
PTR2                rmb       2                   ; DU,GO,MO,AS
PTR3                rmb       2                   ; HO,MO,AS
PTR4                rmb       2                   ; GO,AS
PTR5                rmb       2                   ; AS
PTR6                rmb       2                   ; GO,AS
PTR7                rmb       2                   ; GO,AS
TMP1                rmb       1                   ; main,hexbin,buffarg,termarg
TMP2                rmb       1                   ; GO,HO,AS
TMP3                rmb       1                   ; AS
TMP4                rmb       1                   ; GO,HO,ME,AS

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

;*****************
;
; ROM starts here *
;
;*****************

                    org       ROMBS

;*****************
;**  BUFFALO - This is where Buffalo starts
;** out of reset.  All initialization is done
;** here including determination of where the
;** user terminal is (SCI,ACIA, or DUART).
;*****************

BUFFALO             ldx       #PORTE
                    brclr     ,x,$01,BUFISIT      ; if bit 0 of port e is 1
                    jmp       $B600               ; then jump to the start of EEPROM

BUFISIT             lda       #$93
                    sta       OPTION              ; adpu, dly, irqe, cop
                    lda       #$00
                    sta       TMSK2               ; timer pre = %1 for trace
                    lds       #STACK              ; monitor stack pointer
                    jsr       VECINIT
                    ldx       #USTACK
                    stx       SP                  ; default user stack
                    lda       #$D0
                    sta       REGS+8              ; default user ccr
                    ldd       #$3F0D              ; initial command is ?
                    std       INBUFF
                    jsr       BPCLR               ; clear breakpoints
                    clr       AUTOLF
                    inc       AUTOLF              ; auto cr/lf = on

; Determine type of external comm device - none, or acia *

                    clr       EXTDEV              ; default is none
                    lda       HPRIO
                    anda      #$20
                    beq       BUFF2               ; jump if single chip mode
                    lda       #$03                ; see if external acia exists
                    sta       ACIA                ; master reset
                    lda       ACIA
                    anda      #$7F                ; mask irq bit from status register
                    bne       BUFF1               ; jump if status reg not 0
                    lda       #$12
                    sta       ACIA                ; turn on acia
                    lda       ACIA
                    anda      #$02
                    beq       BUFF1               ; jump if tdre not set
                    lda       #$01
                    sta       EXTDEV              ; external device is acia
                    bra       BUFF2

BUFF1               equ       *                   ; see if duart exists
                    lda       DUART+$0C           ; read IRQ vector register
                    cmpa      #$0F                ; should be out of reset
                    bne       BUFF2
                    lda       #$AA
                    sta       DUART+$0C           ; write irq vector register
                    lda       DUART+$0C           ; read irq vector register
                    cmpa      #$AA
                    bne       BUFF2
                    lda       #$02
                    sta       EXTDEV              ; external device is duart A

; Find terminal port - SCI or external. *

BUFF2               clr       IODEV
                    jsr       TARGCO              ; disconnect sci for evb board
                    bsr       SIGNON              ; initialize sci
                    lda       EXTDEV
                    beq       BUFF3               ; jump if no external device
                    sta       IODEV
                    bsr       SIGNON              ; initialize external device
BUFF3               clr       IODEV
                    jsr       INPUT               ; get input from sci port
                    cmpa      #$0D
                    beq       BUFF4               ; jump if cr - sci is terminal port
                    lda       EXTDEV
                    beq       BUFF3               ; jump if no external device
                    sta       IODEV
                    jsr       INPUT               ; get input from external device
                    cmpa      #$0D
                    beq       BUFF4               ; jump if cr - terminal found ext
                    bra       BUFF3

SIGNON              jsr       INIT                ; initialize device
                    ldx       #MSG1               ; buffalo message
                    jsr       OUTSTRG
                    rts

; Determine where host port should be. *

BUFF4               clr       HOSTDEV             ; default - host = sci port
                    lda       IODEV
                    cmpa      #$01
                    beq       BUFF5               ; default host if term = acia
                    lda       #$03
                    sta       HOSTDEV             ; else host is duart port b
BUFF5               equ       *

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

MAIN                lds       #STACK              ; initialize sp every time
                    clr       AUTOLF
                    inc       AUTOLF              ; auto cr/lf = on
                    jsr       OUTCRLF
                    lda       #PROMPT             ; prompt user
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
                    sta       ,x                  ; put a in buffer
                    bra       COMM0

MAIN3               sta       ,x                  ; put a in buffer
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
                    sta       ,x                  ; put in command buffer
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
                    stb       COUNT
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

SRCH                stb       COUNT               ; size of command entered
                    ldx       #COMTABL            ; pointer to table
                    stx       PTR1                ; pointer to next entry
SRCH1               ldx       PTR1
                    ldy       #COMBUFF            ; pointer to command buffer
                    ldb       ,x
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
                    lda       1,x                 ; read table
                    cmpa      ,y                  ; compare to combuff
                    bne       SRCH1               ; try next entry
                    inx                           ; move pointers
                    iny
                    cmpb      COUNT
                    blt       SRCHLP              ; loop countu1 times
                    ldx       PTR1
                    dex
                    dex
                    ldx       ,x                  ; jump address from table
EXEC                jsr       ,x                  ; call task as subroutine
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
                    ldb       #8
BPCLR1              clr       ,x
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
RPRNT1              lda       ,x
                    psha
                    pshx
                    jsr       OUTPUT              ; name
                    lda       #'-'
                    jsr       OUTPUT              ; dash
                    ldb       7,x                 ; contents offset
                    lda       14,x                ; bytesize
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
;   if(((x) != a) && (x == eeprom location))
;      if((x) != $FF) byte erase (x);
;      if(a != $FF) program(x) = a);
;   if((x) != a) error(ROM)
; *return;

CHGBYT              tst       COUNT
                    beq       CHGBYT4             ; jump if shftreg empty
                    lda       SHFTREG+1
                    sta       ,x                  ; attempt to write
                    lda       ,x
                    cmpa      SHFTREG+1
                    beq       CHGBYT3             ; jump if it worked
                    cpx       #CONFIG
                    beq       CHGBYT1             ; jump if config reg
                    cpx       #$B600
                    blo       CHGBYT3             ; jump if not EE
                    cpx       #$B7FF
                    bhi       CHGBYT3             ; jump if not EE
CHGBYT1             equ       *
                    lda       ,x
                    cmpa      #$FF
                    beq       CHGBYT2             ; jump if already erased
                    lda       #$16                ; do byte erase
                    sta       PPROG
                    lda       #$FF
                    sta       ,x
                    lda       #$17
                    bne       ACL1
                    clra                          ; fail safe
ACL1                sta       PPROG
                    bsr       CHGWAIT
                    lda       #$00
                    sta       PPROG               ; end of byte erase
CHGBYT2             equ       *
                    lda       SHFTREG+1
                    cmpa      #$FF
                    beq       CHGBYT3             ; jump if no need to program
                    lda       #$02                ; do byte program
                    sta       PPROG
                    lda       SHFTREG+1
                    sta       ,x
                    lda       #$03
                    bne       ACL2
                    clra                          ; fail safe
ACL2                sta       PPROG
                    bsr       CHGWAIT
                    lda       #$00
                    sta       PPROG               ; end of byte program
CHGBYT3             equ       *
                    lda       ,x
                    cmpa      SHFTREG+1
                    beq       CHGBYT4
                    pshx
                    ldx       #MSG6               ; "rom"
                    jsr       OUTSTRG
                    jsr       OUTCRLF
                    pulx
CHGBYT4             equ       *
                    rts

CHGWAIT             equ       *                   ; delay 10 ms at E = 2MHz
                    pshx
                    ldx       #$0D06
CHGWAIT1            dex
                    bne       CHGWAIT1
                    pulx
                    rts

;*****************
;  READBUFF() -  Read the character in INBUFF
; pointed at by ptrbuff into A.  Returns ptrbuff
; unchanged.
;*****************
READBUFF            pshx
                    ldx       PTR0
                    lda       ,x
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
; that character.
;*****************
WSKIP               bsr       READBUFF            ; read character
                    bsr       WCHEK
                    bne       WSKIP1              ; jump if not wspc
                    bsr       INCBUFF             ; move pointer
                    bra       WSKIP               ; loop

WSKIP1              rts

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
HOSTCO              psha
                    lda       #$01
                    sta       DFLOP               ; send 1 to d-flop
                    pula
                    rts

TARGCO              psha
                    lda       #$00
                    sta       DFLOP               ; send 0 to d-flop
                    pula
                    rts

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
VECLOOP             cmpa      ,x
                    beq       VECNEXT             ; If vector already in
                    sta       ,x                  ; install JMP
                    sty       1,x                 ; to STOPIT routine
VECNEXT             abx                           ; Add 3 to point at next vector
                    cpx       #JCLM+3             ; Done?
                    bne       VECLOOP             ; If not, continue loop
                    rts

;
STOPIT              lda       #$50                ; Stop-enable; IRQ, XIRQ-Off
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
                    lda       IODEV
                    cmpa      #$00
                    bne       INIT1               ; jump not sci
                    jsr       ONSCI               ; initialize sci
                    bra       INIT4

INIT1               cmpa      #$01
                    bne       INIT2               ; jump not acia
                    jsr       ONACIA              ; initialize acia
                    bra       INIT4

INIT2               ldx       #PORTA
                    cmpa      #$02
                    beq       INIT3               ; jump duart a
                    ldx       #PORTB
INIT3               bsr       ONUART              ; initialize duart
INIT4               pulx                          ; restore registers
                    pula
                    rts

;**********
;  INPUT() - Read device. Returns a=char or 0.
;    This routine also disarms the cop.
;**********
INPUT               equ       *
                    pshx
                    lda       #$55                ; reset cop
                    sta       COPRST
                    lda       #$AA
                    sta       COPRST
                    lda       IODEV
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
;**********

OUTPUT              equ       *
                    psha                          ; save registers
                    pshb
                    pshx
                    ldb       IODEV
                    bne       OUTPUT1             ; jump not sci
                    jsr       OUTSCI              ; write sci
                    bra       OUTPUT4

OUTPUT1             cmpb      #$01
                    bne       OUTPUT4             ; jump not acia
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
                    rts

;**********
;   ONUART(port) - Initialize a duart port.
; Sets duart to internal clock, divide by 16,
; 8 data + 1 stop bits.
;**********

ONUART              lda       #$22
                    sta       2,x                 ; reset receiver
                    lda       #$38
                    sta       2,x                 ; reset transmitter
                    lda       #$40
                    sta       2,x                 ; reset error status
                    lda       #$10
                    sta       2,x                 ; reset pointer
                    lda       #$00
                    sta       DUART+4             ; clock source
                    lda       #$00
                    sta       DUART+5             ; interrupt mask
                    lda       #$13
                    sta       ,x                  ; 8 data, no parity
                    lda       #$07
                    sta       ,x                  ; 1 stop bits
                    lda       #$BB                ; baud rate (9600)
                    sta       1,x                 ; tx and rcv baud rate
                    lda       #$05
                    sta       2,x                 ; enable tx and rcv
                    rts

;**********
;   INUART(port) - Check duart for any input.
;**********

INUART              lda       1,x                 ; read status
                    anda      #$01                ; check rdrf
                    beq       INUART1             ; jump if no data
                    lda       3,x                 ; read data
                    anda      #$7F                ; to mask parity
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
                    lda       #$0A                ; if cr, output lf
                    bra       OUTUART2

OUTUART1            cmpa      #$0A
                    bne       OUTUART3
                    lda       #$0D                ; if lf, output cr
OUTUART2            ldb       1,x                 ; check status
                    andb      #$4
                    beq       OUTUART2            ; loop until tdre=1
                    anda      #$7F                ; mask parity
                    sta       3,x                 ; send character
OUTUART3            rts

;**********
;   ONSCI() - Initialize the SCI for 9600
;                 baud at 8 MHz Extal.
;**********
ONSCI               lda       #$30
                    sta       BAUD                ; baud register
                    lda       #$00
                    sta       SCCR1
                    lda       #$0C
                    sta       SCCR2               ; enable
                    rts

;**********
;   INSCI() - Read from SCI.  Return a=char or 0.
;**********
INSCI               lda       SCSR                ; read status reg
                    anda      #$20
                    beq       INSCI1              ; jump if rdrf=0
                    lda       SCDAT               ; read data register
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
                    lda       #$0A                ; if cr, send lf
                    bra       OUTSCI2

OUTSCI1             cmpa      #$0A
                    bne       OUTSCI3
                    lda       #$0D                ; if lf, send cr
OUTSCI2             ldb       SCSR                ; read status
                    bitb      #$80
                    beq       OUTSCI2             ; loop until tdre=1
                    anda      #$7F                ; mask parity
                    sta       SCDAT               ; send character
OUTSCI3             rts

;**********
;   ONACIA - Initialize the ACIA for
; 8 data bits, 1 stop bit, divide by 64 clock.
;**********
ONACIA              ldx       #ACIA
                    lda       #$03
                    sta       ,x                  ; master reset
                    lda       #$16
                    sta       ,x                  ; setup
                    rts

;**********
;   INACIA - Read from the ACIA, Return a=char or 0.
;**********
INACIA              ldx       #ACIA
                    lda       ,x                  ; status
                    psha
                    anda      #$70                ; check pe, ov, fe
                    pula
                    beq       INACIA1             ; jump - no error
                    bsr       ONACIA              ; reinitialize and try again
                    bra       INACIA

INACIA1             lsra                          ; check rdrf
                    bcs       INACIA2             ; jump if data
                    clra                          ; return(no data)
                    rts

INACIA2             lda       1,x                 ; read data
                    anda      #$7F                ; mask parity
                    rts

;**********
;  OUTACIA - Output A to acia. IF autolf = 1,
;               cr or lf sent as crlf.
;**********
OUTACIA             bsr       OUTACIA3            ; output char
                    tst       AUTOLF
                    beq       OUTACIA2            ; jump no autolf
                    cmpa      #$0D
                    bne       OUTACIA1
                    lda       #$0A
                    bsr       OUTACIA3            ; if cr, output lf
                    bra       OUTACIA2

OUTACIA1            cmpa      #$0A
                    bne       OUTACIA2
                    lda       #$0D
                    bsr       OUTACIA3            ; if lf, output cr
OUTACIA2            rts

OUTACIA3            ldx       #ACIA
                    ldb       ,x
                    bitb      #$2
                    beq       OUTACIA3            ; loop until tdre
                    anda      #$7F                ; mask parity
                    sta       1,x                 ; output
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
                    lda       ,x                  ; get data in a
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
OUTSPAC             lda       #$20                ; output a space
                    jsr       OUTPUT
                    rts

;**********
;  OUTCRLF() - Output a Carriage return and
; a line feed.  Returns a = cr.
;**********
OUTCRLF             lda       #$0D                ; cr
                    jsr       OUTPUT              ; output a
                    lda       #$00
                    jsr       OUTPUT              ; output padding
                    lda       #$0D
                    rts

;**********
;  OUTSTRG(x) - Output string of ASCII bytes
; starting at x until end of text ($04).  Can
; be paused by control w (any char restarts).
;**********
OUTSTRG             bsr       OUTCRLF
OUTSTRG0            psha
OUTSTRG1            lda       ,x                  ; read char into a
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
COMTABL             equ       *
                    fcb       5
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
                    fcb       7
                    fcc       'PROCEED'
                    fdb       PROCEED
                    fcb       8
                    fcc       'REGISTER'
                    fdb       REGISTER
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
                    fcb       -1

;*******************
;*** TEXT TABLES ***

MSG1                fcc       'BUFFALO 2.5 (ext) - Bit User Fast Friendly Aid to Logical Operation'
                    fcb       EOT
MSG2                fcc       'What?'
                    fcb       EOT
MSG3                fcc       'Too Long'
                    fcb       EOT
MSG4                fcc       'Full'
                    fcb       EOT
MSG5                fcc       'Op- '
                    fcb       EOT
MSG6                fcc       'rom-'
                    fcb       EOT
MSG8                fcc       'Command?'
                    fcb       EOT
MSG9                fcc       'Bad argument'
                    fcb       EOT
MSG10               fcc       'No host port available'
                    fcb       EOT
MSG11               fcc       'done'
                    fcb       EOT
MSG12               fcc       'checksum error'
                    fcb       EOT
MSG13               fcc       'error addr '
                    fcb       EOT

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
                    cmpa      #$0D
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
                    clr       ,x                  ; clear entry
                    clr       1,x
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
                    ldx       SHFTREG             ; x = new entry
                    lda       ,x
                    ldb       #SWI
                    stb       ,x
                    ldb       ,x
                    sta       ,x
                    cmpb      #SWI
                    beq       BRKDEF2             ; jump if writes ok
                    stx       PTR1                ; save address
                    ldx       #MSG6               ; "ROM-"
                    jsr       OUTSTRG
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
                    stx       ,y                  ; put new entry in
                    jmp       BREAK               ; do next argument

;**********
;   bprint() - print the contents of the table.
;**********
BPRINT              jsr       OUTCRLF
                    ldx       #BRKTABL
                    ldb       #4
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
                    ldx       ,x                  ; get table entry
                    cpx       SHFTREG
                    bne       BPSRCH2             ; jump if no match
                    rts

BPSRCH2             incb
                    incb
                    cmpb      #$6
                    ble       BPSRCH1             ; loop 4 times
                    ldb       #$FF
                    rts

;**********
;  bulk  - Bulk erase the eeprom except the
; config register.
;**********
BULK                clr       TMP2
                    bra       BULK1

;**********
;  bulkall - Bulk erase the eeprom and the
; config register.
;**********
BULKALL             clr       TMP2
                    inc       TMP2

; *Set up PPROG register for erase
BULK1               psha
                    lda       #$06
                    sta       PPROG               ; set eelat, erase bits

; *If (EE only) write to $b600
; *Else write to config register
                    lda       #$FF
                    tst       TMP2
                    bne       BULK2               ; jump if config
                    sta       $B600               ; write to $b600
                    bra       BULK3

BULK2               sta       CONFIG
BULK3               equ       *

; *Start erasing
                    lda       #$07
                    bne       ACL3
                    clra                          ; fail safe
ACL3                sta       PPROG

; *Delay for 10 ms at E = 2 MHz
                    pshx
                    ldx       #$0D06              ; 6~ * 3334 = 20,004 * 0.5 MHz
BULKDLY             dex                           ; 2~
                    bne       BULKDLY             ; 3~
                    pulx

; *Stop programming
                    clr       PPROG
                    pula
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
                    ldb       #$80
                    abx
                    stx       PTR2                ; default end
                    jsr       WSKIP
                    cmpa      #$D
                    beq       DUMP1               ; jump - no arguments
                    jsr       BUFFARG             ; read argument
                    tst       COUNT
                    beq       DUMPERR             ; jump if no argument
                    jsr       DCHEK
                    bne       DUMPERR             ; jump if delimiter
                    ldx       SHFTREG
                    stx       PTR1
                    ldb       #$80
                    abx
                    stx       PTR2                ; default end address
                    jsr       WSKIP
                    cmpa      #$D
                    beq       DUMP1               ; jump - 1 argument
                    jsr       BUFFARG             ; read argument
                    tst       COUNT
                    beq       DUMPERR             ; jump if no argument
                    jsr       WSKIP
                    cmpa      #$0D
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
                    lda       ,x                  ; ascii value loop
                    cmpa      #$20
                    blo       DUMP3               ; jump if non printable
                    cmpa      #$7A
                    bls       DUMP4               ; jump if printable
DUMP3               lda       #$20                ; space for non printables
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
                    lda       #$FF
                    sta       TMP2                ; default data
                    jsr       WSKIP
                    cmpa      #$0D
                    beq       FILL1               ; jump if default data
                    jsr       BUFFARG
                    tst       COUNT
                    beq       FILLERR             ; jump if no argument
                    jsr       WSKIP
                    cmpa      #$0D
                    bne       FILLERR             ; jump if bad argument
                    lda       SHFTREG+1
                    sta       TMP2

; *while(ptr1 <= ptr2)
;   *ptr1 = data
;   if(*ptr1 != data) abort

FILL1               equ       *
                    jsr       CHKABRT             ; check for abort
                    ldx       PTR1                ; starting address
                    lda       TMP2                ; data
                    sta       ,x
                    cmpa      ,x
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

FILLBAD             ldx       #MSG6               ; "rom -"
                    jsr       OUTSTRG
                    ldx       #PTR1
                    jsr       OUT2BSP
                    rts

;**********
;   call [<addr>] - Execute a jsr to addr or
; *user's pc value.  Return to monitor by rts
; *or breakpoint.
;**********
; *a = wskip();
; *if(a != cr)
;     a = buffarg();
;     a = wskip();
;     if(a != cr) return(bad argument)
;     pc = shftreg;

CALL                jsr       WSKIP
                    cmpa      #$D
                    beq       CALL3               ; jump if no arg
                    jsr       BUFFARG
                    jsr       WSKIP
                    cmpa      #$D
                    beq       CALL2               ; jump if cr
                    ldx       #MSG9               ; "bad argument"
                    jsr       OUTSTRG
                    rts

CALL2               ldx       SHFTREG
                    stx       REGS                ; pc = <addr>

; *user_stack[0] = return_to_monitor;
; *setbps();
; *restack();     /* restack and go*/

CALL3               ldx       SP
                    dex                           ; user stack pointer
                    ldd       #RETURN             ; return address
                    std       ,x
                    dex
                    stx       SP                  ; new user stack pointer
                    jsr       SETBPS
                    clr       TMP2                ; flag for breakpoints
                    jmp       RESTACK             ; executes an rti

;**********
;   return() - Return here from rts after
; *call command.
;**********
RETURN              psha                          ; save a register
                    tpa
                    sta       REGS+8              ; cc register
                    pula
                    std       REGS+6              ; a and b registers
                    stx       REGS+4              ; x register
                    sty       REGS+2              ; y register
                    sts       SP                  ; user stack pointer
                    lds       #STACK              ; monitor stack pointer
                    jsr       REMBPS              ; remove breakpoints
                    jsr       OUTCRLF
                    jsr       RPRINT              ; print user registers
                    jmp       MAIN

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
                    cmpa      #$0D
                    beq       GO2                 ; jump if no arg
                    jsr       BUFFARG
                    jsr       WSKIP
                    cmpa      #$0D
                    beq       GO1                 ; jump if cr
                    ldx       #MSG9               ; "bad argument"
                    jsr       OUTSTRG
                    rts

GO1                 ldx       SHFTREG
                    stx       REGS                ; pc = <addr>
GO2                 clr       TMP2                ; flag for breakpoints
                    inc       TMP2                ; (1=go, 0=call)
                    jsr       SETBPS
                    jmp       RESTACK             ; execute an rti

;**********
;   swiin() - return from SWI.  Set up
; *stack pointers, save user registers, and
; *return to main.
;**********
SWIIN               tsx                           ; swi entry point
                    lds       #STACK
                    jsr       SAVSTACK            ; save user regs
                    ldx       REGS
                    dex
                    stx       REGS                ; save user pc
                    ldx       PTR4                ; restore user swi vector
                    stx       JSWI+1

; *if(flagt1 = 0) remove return addr from stack;

                    tst       TMP2                ; 0=call, 1=go
                    bne       GO3                 ; jump if go command
                    ldx       SP                  ; remove return address
                    inx
                    inx
                    stx       SP
GO3                 jsr       OUTCRLF             ; print register values
                    jsr       RPRINT
                    jsr       REMBPS
                    jmp       MAIN                ; return to monitor

;                      (sp destroyed above)

;**********
;   proceed - Same as go except it ignores
; *a breakpoint at the first opcode.  Calls
; *trace once and the go.
;**********
PROCEED             clr       TMP2                ; flag for breakpoints
                    inc       TMP2                ; 0=trace, 1=proceed
                    bra       TRACE3

;**********
;   trace <n> - Trace n instructions starting
; *at user's pc value. n is a hex number less than
; *$FF (defaults to 1).
;**********
; *countt1 = 1
; *a = wskip();
; *if(a != cr)
;     a = buffarg(); a = wskip();
;     if(a != cr) return(bad argument);
;     countt1 = n

TRACE               clr       TMP4
                    inc       TMP4                ; default countt1 = 1
                    clr       TMP2                ; 0 = trace
                    jsr       WSKIP
                    cmpa      #$0D
                    beq       TRACE2              ; jump if cr
                    jsr       BUFFARG
                    jsr       WSKIP
                    cmpa      #$0D
                    beq       TRACE1              ; jump if cr
                    ldx       #MSG9               ; "bad argument"
                    jsr       OUTSTRG
                    rts

TRACE1              lda       SHFTREG+1           ; n
                    sta       TMP4

; *Print opcode
TRACE2              jsr       OUTCRLF
                    ldx       #MSG5               ; "op-"
                    jsr       OUTSTRG
                    ldx       REGS
                    jsr       OUT1BSP             ; opcode

; *Save user OC5 regs, setup monitor OC5 regs
TRACE3              lda       TCTL1
                    sta       PTR2                ; save user mode/level
                    anda      #$FC
                    sta       TCTL1               ; disable oc5 output
                    lda       TMSK1
                    sta       PTR2+1              ; save user int masks
                    clr       TMSK2               ; disable tof and pac ints

; *Put monitor TOC5 vector into jump table
                    ldx       JTOC5+1
                    stx       PTR4                ; save user's vector
                    lda       #$7E                ; jmp opcode
                    sta       JTOC5
                    ldx       #TRACEIN
                    stx       JTOC5+1             ; monitor toc5 vector

; *Unmask i bit in user ccr
                    lda       REGS+8              ; user ccr
                    anda      #$EF                ; clear i bit
                    sta       REGS+8

; *Arm OC5 interrupt
                    ldb       #87                 ; cycles to end of rti
                    ldx       TCNT                ; timer count value
                    abx                           ; 3~ )
                    stx       TOC5                ; oc5 match register 5~ )
                    lda       #$08                ; 2~ )
                    sta       TFLG1               ; clear oc5 int flag 4~ ) 86~
                    sta       TMSK1               ; enable oc5 interrupt 4~ )
                    cli                           ; 2~ )
                    bra       RESTACK             ; execute an rti 66~ )

;**********
;   tracein - return from toc5 interrupt.
;**********
; *Disable toc5 interrupt
; *replace user's toc5 vector
TRACEIN             sei
                    clr       TMSK1               ; disable timer ints
                    tsx
                    lds       #STACK
                    jsr       SAVSTACK            ; save user regs
                    ldx       PTR4
                    stx       JTOC5+1
                    jsr       CHKABRT             ; check for abort

; *if(flagt1 = 1) jump to GO command ( proceed )
                    tst       TMP2
                    beq       TRACE9              ; jump if trace command
                    jmp       GO2

; *rprint();
; *while(countt1 >= 0) continue trace;

TRACE9              jsr       OUTCRLF             ; print registers for
                    jsr       RPRINT              ; trace only.
                    dec       TMP4
                    bhi       TRACE2              ; jump if countt1 >= 0
                    jmp       MAIN                ; return to monitor

;                         (sp destroyed above)

;**********
;  setbps - Replace user code with swi's at
; *breakpoint addresses.
;**********
; *for(b=0; b=6; b =+ 2)
;     x = brktabl[b];
;     if(x != 0)
;          optabl[b] = x[0];
;          x[0] = $3F;

SETBPS              clrb
SETBPS1             ldx       #BRKTABL
                    ldy       #PTR6
                    abx
                    aby
                    ldx       ,x                  ; breakpoint table entry
                    beq       SETBPS2             ; jump if 0
                    lda       ,x                  ; save user opcode
                    sta       ,y
                    lda       #SWI                ; insert swi into code
                    sta       ,x
SETBPS2             addb      #$2
                    cmpb      #$6
                    ble       SETBPS1             ; loop 4 times

; *Put monitor SWI vector into jump table
                    ldx       JSWI+1
                    stx       PTR4                ; save user swi vector
                    lda       #$7E                ; jmp opcode
                    sta       JSWI
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

REMBPS              clrb
REMBPS1             ldx       #BRKTABL
                    ldy       #PTR6
                    abx
                    aby
                    ldx       ,x                  ; breakpoint table entry
                    beq       REMBPS2             ; jump if 0
                    lda       ,y                  ; restore user's opcode
                    sta       ,x
REMBPS2             addb      #$2
                    cmpb      #$6
                    ble       REMBPS1             ; loop 4 times

; *Replace user's SWI vector
                    ldx       PTR4
                    stx       JSWI+1
                    rts

;**********
;  restack() - Restore user stack and
; *execute an RTI. Extended addressing forced
; *to ensure count value for trace.
;**********
RESTACK             lds       >SP                 ; stack pointer
                    ldx       >REGS
                    pshx                          ; pc
                    ldx       >REGS+2
                    pshx                          ; y
                    ldx       >REGS+4
                    pshx                          ; x
                    ldd       >REGS+6
                    psha                          ; a
                    pshb                          ; b
                    lda       >REGS+8
                    psha                          ; ccr
RESTACK1            rti

;**********
;  savstack() -  Save user's registers.
;**********
SAVSTACK            lda       ,x
                    sta       REGS+8              ; ccr
                    ldd       1,x
                    sta       REGS+7              ; b
                    stb       REGS+6              ; a
                    ldd       3,x
                    std       REGS+4              ; x
                    ldd       5,x
                    std       REGS+2              ; y
                    ldd       7,x
                    std       REGS                ; pc
                    ldb       #8
                    abx
                    stx       SP                  ; stack pointer
                    rts

;**********
;   help  -  List buffalo commands to terminal.
;**********
HELP                equ       *
                    ldx       #HELPMSG1
                    jsr       OUTSTRG             ; print help screen
                    rts

HELPMSG1            equ       *
                    fcc       'ASM [<addr>]  Line assembler/disassembler.'
                    fcb       $0D
                    fcc       '    /        Do same address.           ^        Do previous address.'
                    fcb       $0D
                    fcc       '    CTRL-J   Do next address.           RETURN   Do next opcode.'
                    fcb       $0D
                    fcc       '    CTRL-A   Quit.'
                    fcb       $0D
                    fcc       'BF <addr1> <addr2> [<data>]  Block fill.'
                    fcb       $0D
                    fcc       'BR [-][<addr>]  Set up breakpoint table.'
                    fcb       $0D
                    fcc       'BULK  Erase the EEPROM.                   BULKALL  Erase EEPROM and CONFIG.'
                    fcb       $0D
                    fcc       'CALL [<addr>]  Call user subroutine.      G [<addr>]  Execute user code.'
                    fcb       $0D
                    fcc       'LOAD, VERIFY [T] <host download command>  Load or verify S-records.'
                    fcb       $0D
                    fcc       'MD [<addr1> [<addr2>]]  Memory dump.'
                    fcb       $0D
                    fcc       'MM [<addr>]  Memory modify.'
                    fcb       $0D
                    fcc       '    /        Open same address.         CTRL-H or ^   Open previous address.'
                    fcb       $0D
                    fcc       '    CTRL-J   Open next address.         SPACE         Open next address.'
                    fcb       $0D
                    fcc       '    RETURN   Quit.                      <addr>O       Compute offset to <addr>.'
                    fcb       $0D
                    fcc       'MOVE <s1> <s2> [<d>]  Block move.'
                    fcb       $0D
                    fcc       'P  Proceed/continue execution.'
                    fcb       $0D
                    fcc       'RM [P, Y, X, A, B, C, or S]  Register modify.'
                    fcb       $0D
                    fcc       'T [<n>]  Trace n instructions.'
                    fcb       $0D
                    fcc       'TM  Transparent mode (CTRL-A = exit, CTRL-B = send break).'
                    fcb       $0D
                    fcc       'CTRL-H  Backspace.                      CTRL-W  Wait for any key.'
                    fcb       $0D
                    fcc       'CTRL-X or DELETE  Abort/cancel command.'
                    fcb       $0D
                    fcc       'RETURN  Repeat last command.'
                    fcb       4

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

HOST                lda       EXTDEV
                    bne       HOST0               ; jump if host port avail.
                    ldx       #MSG10              ; "no host port avail"
                    jsr       OUTSTRG
                    rts

HOST0               clr       AUTOLF              ; turn off autolf
                    jsr       HOSTCO              ; connect sci (evb board)
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
                    jsr       TARGCO              ; disconnect sci (evb board)
                    rts                           ; return

;**********
; txbreak() - transmit break to host port.
; The duration of the transmitted break is
; approximately 200,000 E-clock cycles, or
; 100ms at 2.0 MHz.
;***********
TXBREAK             equ       *
                    lda       HOSTDEV
                    cmpa      #$03
                    beq       TXBDU               ; jump if duartb is host

TXBSCI              ldx       #SCCR2              ; sci is host
                    bset      ,x,$01              ; set send break bit
                    bsr       TXBWAIT
                    bclr      ,x,$01              ; clear send break bit
                    bra       TXB1

TXBDU               ldx       #PORTB              ; duart host port
                    lda       #$60                ; start break cmd
                    sta       2,x                 ; port b command register
                    bsr       TXBWAIT
                    lda       #$70                ; stop break cmd
                    sta       2,x                 ; port b command register

TXB1                lda       #$0D
                    bsr       HOSTOUT             ; send carriage return
                    lda       #$0A
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
HOSTINIT            ldb       IODEV               ; save terminal
                    pshb
                    ldb       HOSTDEV
                    stb       IODEV               ; point to host
                    jsr       INIT                ; initialize host
                    bra       TERMRES             ; restore terminal

HOSTIN              ldb       IODEV               ; save terminal
                    pshb
                    ldb       HOSTDEV
                    stb       IODEV               ; point to host
                    jsr       INPUT               ; read host
                    bra       TERMRES             ; restore terminal

HOSTOUT             ldb       IODEV               ; save terminal
                    pshb
                    ldb       HOSTDEV
                    stb       IODEV               ; point to host
                    jsr       OUTPUT              ; write to host
TERMRES             pulb                          ; restore terminal device
                    stb       IODEV
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
;**********
VERIFY              clr       TMP2
                    inc       TMP2                ; flagt1 = 1 = verify
                    bra       LOAD1

LOAD                clr       TMP2                ; flagt1 = 0 = load

; *a=wskip();
; *if(a = cr) goto transparent mode;
; *if(t option) hostdev = iodev;

LOAD1               jsr       WSKIP
                    cmpa      #$0D
                    bne       LOAD1A
                    jmp       HOST                ; go to host if no args

LOAD1A              jsr       UPCASE
                    cmpa      #'T'                ; look for t option
                    bne       LOAD1B              ; jump not t option
                    jsr       INCBUFF
                    jsr       READBUFF            ; get next character
                    jsr       DECBUFF
                    cmpa      #$0D
                    bne       LOAD1B              ; jump if not t option
                    clr       AUTOLF
                    lda       IODEV
                    sta       HOSTDEV             ; set host port = terminal
                    bra       LOAD6               ; go wait for s1 records

; *else while(not cr)
;     read character from input buffer;
;     send character to host;

LOAD1B              clr       AUTOLF
                    jsr       HOSTCO              ; connect sci (evb board)
                    bsr       HOSTINIT            ; initialize host port
LOAD2               jsr       READBUFF            ; get next char
                    jsr       INCBUFF
                    psha                          ; save char
                    bsr       HOSTOUT             ; output to host
                    jsr       OUTPUT              ; echo to terminal
                    pula
                    cmpa      #$0D
                    bne       LOAD2               ; jump if not cr

; *repeat:
;      if(hostdev != iodev) check abort;
;      a = hostin();
;      if(a = 'S')
;          a = hostin;
;          if(a = '9')
;              read rest of record;
;              return(done);
;          if(a = '1')
;              checksum = 0;
;              byte(); b = shftreg+1;     /* byte count */
;              byte(); byte(); x = shftreg; /* base addr*/
;              do
;                  byte();
;                  if(flagt1 = 0)
;                        x[0] = shftreg+1
;                  if(x[0] != shftreg+1)
;                         return("rom-(x)");
;                  x++; b--;
;              until(b = 0)

LOAD6               equ       *
                    lda       HOSTDEV
                    cmpa      IODEV
                    beq       LOAD65              ; jump if hostdev=iodev
                    jsr       CHKABRT             ; check for abort
LOAD65              bsr       HOSTIN              ; read host
                    tsta
                    beq       LOAD6               ; jump if no input
                    cmpa      #'S'
                    bne       LOAD6               ; jump if not S
LOAD7               bsr       HOSTIN              ; read host
                    tsta
                    beq       LOAD7               ; jump if no input
                    cmpa      #'9'
                    bne       LOAD8               ; jump if not S9
                    bsr       BYTE
                    ldb       SHFTREG+1           ; b = byte count
LOAD75              bsr       BYTE
                    decb
                    bne       LOAD75              ; loop until end of record
                    inc       AUTOLF              ; turn on autolf
                    jsr       TARGCO              ; disconnect sci (evb)
                    ldx       #MSG11              ; "done"
                    jsr       OUTSTRG
                    rts

LOAD8               cmpa      #'1'
                    bne       LOAD6               ; jump if not S1
                    clr       TMP4                ; clear checksum
                    bsr       BYTE
                    ldb       SHFTREG+1
                    subb      #$2                 ; b = byte count
                    bsr       BYTE
                    bsr       BYTE
                    ldx       SHFTREG             ; x = base address
                    dex
LOAD10              bsr       BYTE                ; get next byte
                    inx
                    decb                          ; check byte count
                    beq       LOAD12              ; if 0, go do checksum
                    lda       SHFTREG+1
                    tst       TMP2
                    bne       LOAD11              ; jump if verify
                    sta       ,x                  ; load only
LOAD11              cmpa      ,x                  ; verify ram location
                    beq       LOAD10              ; jump if ram ok
                    stx       PTR3                ; save error address
                    inc       AUTOLF              ; turn on autolf
                    jsr       TARGCO              ; disconnect sci(evb)
                    jsr       OUTCRLF
                    ldx       #MSG13              ; "error addr"
                    jsr       OUTSTRG
                    ldx       #PTR3
                    jsr       OUT2BSP             ; address
                    rts

LOAD12              lda       TMP4
                    inca                          ; do checksum
                    bne       LOAD13              ; jump if s1 record okay
                    bra       LOAD6

LOAD13              inc       AUTOLF
                    jsr       TARGCO              ; disconnect sci(evb)
                    jsr       OUTCRLF
                    ldx       #MSG12              ; "checksum error"
                    jsr       OUTSTRG
                    rts

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
                    lda       SHFTREG+1
                    adda      TMP4
                    sta       TMP4                ; add to checksum
                    pulx
                    pulb
                    rts

;*******************************************
;   MEMORY [<addr>]
;   [<addr>]/
; Opens memory and allows user to modify the
; *contents at <addr> or the last opened location.
;    Subcommands:
; [<data>]<cr> - Close current location and exit.
; [<data>]<lf> - Close current and open next.
; [<data>]<^> - Close current and open previous.
; [<data>]<sp> - Close current and open next.
; [<data>]/ - Reopen current location.
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
                    cmpa      #$D
                    beq       MEM1                ; jump if cr
                    jsr       BUFFARG
                    jsr       WSKIP
                    cmpa      #$D
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
;          case(linefeed):
;             chgbyt();
;             ptrmem[]++;
;          case(up arrow):
;          case(backspace):
;               chgbyt();
;               ptrmem[]--;
;          case("/"):
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
                    cmpa      #$5E
                    beq       MEMUA               ; jump if up arrow
                    cmpa      #$08
                    beq       MEMBS               ; jump if backspace
                    cmpa      #'/'
                    beq       MEMSL               ; jump if /
                    cmpa      #'O'
                    beq       MEMOFF              ; jump if O
                    cmpa      #$0D
                    beq       MEMCR               ; jump if carriage ret
                    ldx       #MSG8               ; "command?"
                    jsr       OUTSTRG
                    bra       MEM1

MEMSP               jsr       CHGBYT
                    inx
                    stx       PTRMEM
                    bra       MEM3                ; output contents

MEMLF               jsr       CHGBYT
                    inx
                    stx       PTRMEM
                    bra       MEM2                ; output addr, contents

MEMUA               equ       *
MEMBS               jsr       CHGBYT
                    dex
                    stx       PTRMEM
                    bra       MEM1                ; output cr, addr, contents

MEMSL               jsr       CHGBYT
                    bra       MEM1                ; output cr, addr, contents

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
                    bra       MEM1                ; output cr, addr, contents

MEMOFF3             subd      #$1                 ; b now has offset
                    stb       TMP4
                    jsr       OUTSPAC
                    ldx       #TMP4
                    jsr       OUT1BSP             ; output offset
                    jmp       MEM1                ; output cr, addr, contents

MEMCR               jsr       CHGBYT
                    rts                           ; exit task

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
                    cmpa      #$0D
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
                    lda       ,x                  ; char at src2
                    pshx
                    ldx       PTR3
                    cpx       #$B600              ; jump if not eeprom
                    blo       MOVEA
                    cpx       #$B7FF              ; jump if not eeprom
                    bhi       MOVEA
                    bsr       MOVPROG             ; program eeprom
MOVEA               sta       ,x                  ; dest
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
                    lda       ,x
                    pshx
                    ldx       PTR3                ; dest
                    cpx       #$B600              ; jump if not eeprom
                    blo       MOVEB
                    cpx       #$B7FF              ; jump if not eeprom
                    bhi       MOVEB
                    bsr       MOVPROG             ; program eeprom
MOVEB               sta       ,x
                    inx
                    stx       PTR3
                    pulx
                    cpx       PTR2
                    beq       MOVRTS
                    inx
                    bra       MOVELP2             ; Loop SRC2-SRC1 times

MOVRTS              rts

;*************
;   MOVPROG - Program eeprom location in X with
;       data in A.
;*************
MOVPROG             pshb
                    pshx
                    ldb       #$02
                    stb       PPROG               ; set eelat
                    sta       ,x
                    ldb       #$03
                    bne       ACL4
                    clrb                          ; fail safe
ACL4                stb       PPROG               ; set pgm
                    ldx       #$0D06
MOVEDLY             dex
                    bne       MOVEDLY             ; delay 10 ms at E = 2 MHz
                    ldb       #$00
                    stb       PPROG
                    pulx
                    pulb
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
REG1                cmpa      ,x
                    beq       REG3
                    ldb       ,x
                    inx
                    cmpb      #'S'
                    bne       REG1                ; jump if not "s"
REG2                ldx       #MSG9               ; "bad argument"
                    jsr       OUTSTRG
                    rts

REG3                pshx
                    jsr       INCBUFF
                    jsr       WSKIP               ; next char after arg
                    cmpa      #$D
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
                    ldb       7,x                 ; get reg offset
                    lda       14,x                ; byte size
                    ldx       #REGS               ; user registers
                    abx
                    tsta
                    beq       REG7                ; jump if 1 byte reg
                    lda       SHFTREG
                    sta       ,x                  ; put in top byte
                    inx
REG7                lda       SHFTREG+1
                    sta       ,x                  ; put in bottom byte
REG8                pulx
                    pula
                    ldb       ,x                  ; CHECK FOR REGISTER S
                    cmpb      #'S'
                    beq       REG9                ; jump if "s"
                    inx                           ; point to next register
                    cmpa      #$D
                    bne       REG5                ; jump if not cr
REG9                rts

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
OLDPC               equ       PTRMEM
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
MSGA1               fcc       'Immediate mode illegal'
                    fcb       EOT
MSGA2               fcc       'Error in mnemonic table'
                    fcb       EOT
MSGA3               fcc       'Illegal bit op'
                    fcb       EOT
MSGA4               fcc       'Bad argument'
                    fcb       EOT
MSGA5               fcc       'Mnemonic not found'
                    fcb       EOT
MSGA6               fcc       'Unknown addressing mode'
                    fcb       EOT
MSGA7               fcc       'Indexed addressing assumed'
                    fcb       EOT
MSGA8               fcc       'Syntax error'
                    fcb       EOT
MSGA9               fcc       'Branch out of range'
                    fcb       EOT

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
;   <cr>  -Carriage return finds the next opcode for
;          assembly.  If there was no assembly input,
;          the next opcode disassembled is retrieved
;          from the disassembler.
;   <lf>  -Linefeed works the same as carriage return
;          except if there was no assembly input, the
;          <addr> is incremented and the next <addr> is
;          disassembled.
;    '^'  -Up arrow decrements <addr> and the previous
;          address is then disassembled.
;    '/'  -Slash redisassembles the current address.
;
;       To exit the assembler use CONTROL A.  Of course
; *control X and DEL will also allow you to abort.
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
                    cmpa      #$0D
                    beq       ASSLOOP             ; jump if no argument
                    jsr       BUFFARG
                    jsr       WSKIP
                    cmpa      #$0D
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
;  if(a == ('^' or '/')) outcrlf;
;  if(a == 0) return(error);

ASSLOOP             ldx       OLDPC
                    stx       PC
                    jsr       OUTCRLF
                    ldx       #PC
                    jsr       OUT2BSP             ; output the address
                    jsr       DISASSM             ; disassemble opcode
                    jsr       OUTCRLF
                    jsr       OUTSPAC
                    jsr       OUTSPAC
                    jsr       OUTSPAC
                    jsr       OUTSPAC
                    lda       #PROMPT             ; prompt user
                    jsr       OUTA                ; output prompt character
                    jsr       READLN              ; read input for assembly
                    sta       ASSCOMM
                    cmpa      #'^'
                    beq       ASSLP0              ; jump if up arrow
                    cmpa      #'/'
                    beq       ASSLP0              ; jump if slash
                    cmpa      #$00
                    bne       ASSLP1              ; jump if none of above
                    rts                           ; return if bad input

ASSLP0              jsr       OUTCRLF
ASSLP1              equ       *
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
                    lda       CLASS
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
                    ldx       ,x
                    jsr       OUTSTRG             ; output error message
ASSLP5              clr       ASSCOMM             ; error command

;  /* compute next address - asscomm holds subcommand
;     and dispc indicates if valid assembly occured. */
;  if(asscomm=='^') oldpc -= 1;
;  if(asscomm==(lf or cr)
;     if(dispc==0) oldpc=pc;
;     else
;        if(asscomm==lf) dispc=oldpc+1;
;        oldpc=dispc;
; *until(eot)

ASSLP10             equ       *
                    lda       ASSCOMM
                    cmpa      #'^'
                    bne       ASSLP11             ; jump if not up arrow
                    ldx       OLDPC
                    dex
                    stx       OLDPC               ; back up
                    bra       ASSLP15

ASSLP11             cmpa      #$0A
                    beq       ASSLP12             ; jump if linefeed
                    cmpa      #$0D
                    bne       ASSLP15             ; jump if not cr
ASSLP12             ldx       DISPC
                    bne       ASSLP13             ; jump if dispc != 0
                    ldx       PC
                    stx       OLDPC
                    bra       ASSLP15

ASSLP13             cmpa      #$0A
                    bne       ASSLP14             ; jump if not linefeed
                    ldx       OLDPC
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
                    lda       #$0D                ; carriage ret
RLN0                ldx       #INBUFF
                    abx
                    sta       ,x                  ; initialize input buffer
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
; *until (a == (cr,lf,^,/))
; *return(a);

                    clrb
RLN1                jsr       INCHAR
                    cmpa      #DEL                ; Delete
                    beq       RLNQUIT
                    cmpa      #CTLX               ; Control X
                    beq       RLNQUIT
                    cmpa      #CTLA               ; Control A
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
                    sta       ,x                  ; put char in buffer
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

PARSE               lda       #$20
                    sta       COMBUFF+3
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
                    sta       ,x                  ; store in combuff
                    incb
                    cmpb      #$5
                    ble       PARSLP              ; loop 6 times
PARSRT              rts

;****************
;  asschek() -perform compares for
; cr, lf, ^, /
;****************
ASSCHEK             cmpa      #$0A                ; linefeed
                    beq       ASSCHK1
                    cmpa      #$0D                ; carriage ret
                    beq       ASSCHK1
                    cmpa      #'^'                ; up arrow
                    beq       ASSCHK1
                    cmpa      #'/'                ; slash
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
                    ldb       #6
                    abx                           ; point to next table entry
MSRCH1              lda       ,x                  ; read table
                    cmpa      #EOT
                    bne       MSRCH2              ; jump if not end of table
                    lda       #$FF
                    sta       CLASS               ; FF = not in table
                    rts

MSRCH2              cmpa      ,y                  ; op[0] = tabl[0] ?
                    bne       MSNEXT
                    lda       1,x
                    cmpa      1,y                 ; op[1] = tabl[1] ?
                    bne       MSNEXT
                    lda       2,x
                    cmpa      2,y                 ; op[2] = tabl[2] ?
                    bne       MSNEXT
                    lda       3,x
                    cmpa      3,y                 ; op[2] = tabl[2] ?
                    bne       MSNEXT
                    ldd       4,x                 ; opcode, class
                    sta       BASEOP
                    stb       CLASS
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
                    lda       #OTHER
                    sta       AMODE               ; mode
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
                    ldd       ,x                  ; read 2 chars from buffer
                    inx                           ; move pointer
                    cmpa      #','
                    bne       DOPLP1
                    cmpb      #'Y'                ; look for ",y"
                    bne       DOPLP2
                    lda       #INDY
                    sta       AMODE
                    bra       DOOP1

DOPLP2              cmpb      #'X'                ; look for ",x"
                    bne       DOOP1               ; jump if not x
                    lda       #INDX
                    sta       AMODE
;                   bra       DOOP1

DOOP1               jsr       WSKIP
                    cmpa      #'#'                ; look for immediate mode
                    bne       DOOP2
                    jsr       INCBUFF             ; point at argument
                    lda       #IMMED
                    sta       AMODE
DOOP2               equ       *

; *switch(class)
                    ldb       CLASS
                    cmpb      #P2INH
                    bne       DOSW1
                    bra       DOP2I

DOSW1               cmpb      #INH
                    bne       DOSW2
                    bra       DOINH

DOSW2               cmpb      #REL
                    bne       DOSW3
                    bra       DOREL

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

DODEF               lda       #$2
                    rts

;  case P2INH: emit(PAGE2)

DOP2I               lda       #PAGE2
                    jsr       EMIT

;  case INH: emit(baseop);
;       return(0);

DOINH               lda       BASEOP
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
                    lda       #$09                ; 'Out of range'
                    rts

DOREL2              lda       BASEOP
                    jsr       EMIT                ; emit opcode
                    lda       BRADDR+1
                    jsr       EMIT                ; emit offset
                    clra                          ; normal return
                    rts

;  case LIMM: if (amode == IMMED) amode = LIMMED;

DOLIM               lda       AMODE
                    cmpa      #IMMED
                    bne       DONOI
                    lda       #LIMMED
                    sta       AMODE

;  case NIMM: if (amode == IMMED)
;                return("Immediate mode illegal");

DONOI               lda       AMODE
                    cmpa      #IMMED
                    bne       DOGENE              ; jump if not immediate
                    lda       #$1                 ; "immediate mode illegal"
                    rts

;  case GEN: dogen(baseop,amode,PAGE1,PAGE1,PAGE2);
;            return;

DOGENE              lda       #PAGE1
                    sta       PNORM
                    sta       PX
                    lda       #PAGE2
                    sta       PY
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

DOGRP               lda       AMODE
                    cmpa      #INDY
                    bne       DOGRP1
                    lda       #PAGE2
                    jsr       EMIT
                    lda       #INDX
                    sta       AMODE
DOGRP1              equ       *
                    lda       AMODE
                    cmpa      #INDX
                    bne       DOGRP2
                    jsr       DOINDEX
                    rts

DOGRP2              equ       *
                    lda       BASEOP
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

DOCPD               lda       AMODE
                    cmpa      #IMMED
                    bne       DOCPD1
                    lda       #LIMMED
                    sta       AMODE
DOCPD1              lda       AMODE
                    cmpa      #INDY
                    bne       DOCPD2
                    inc       YFLAG
DOCPD2              lda       #PAGE3
                    sta       PNORM
                    sta       PX
                    lda       #PAGE4
                    sta       PY
                    jsr       DOGEN
                    rts

;  case XNIMM: if (amode == IMMED)      /* stx */
;                 return("Immediate mode illegal");

DOXNOI              lda       AMODE
                    cmpa      #IMMED
                    bne       DOXLI
                    lda       #$1                 ; "immediate mode illegal"
                    rts

;  case XLIMM: if (amode == IMMED)  /* cpx, ldx */
;                 amode = LIMMED;
;              dogen(baseop,amode,PAGE1,PAGE1,PAGE4);
;              return;

DOXLI               lda       AMODE
                    cmpa      #IMMED
                    bne       DOXLI1
                    lda       #LIMMED
                    sta       AMODE
DOXLI1              lda       #PAGE1
                    sta       PNORM
                    sta       PX
                    lda       #PAGE4
                    sta       PY
                    jsr       DOGEN
                    rts

;  case YNIMM: if (amode == IMMED)      /* sty */
;                 return("Immediate mode illegal");

DOYNOI              lda       AMODE
                    cmpa      #IMMED
                    bne       DOYLI
                    lda       #$1                 ; "immediate mode illegal"
                    rts

;  case YLIMM: if (amode == INDY) yflag = 1;/* cpy, ldy */
;              if(amode == IMMED) amode = LIMMED;
;              dogen(opcode,amode,PAGE2,PAGE3,PAGE2);
;              return;

DOYLI               lda       AMODE
                    cmpa      #INDY
                    bne       DOYLI1
                    inc       YFLAG
DOYLI1              cmpa      #IMMED
                    bne       DOYLI2
                    lda       #LIMMED
                    sta       AMODE
DOYLI2              lda       #PAGE2
                    sta       PNORM
                    sta       PY
                    lda       #PAGE3
                    sta       PX
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
                    lda       #$3                 ; "illegal bit op"
                    rts

DOSET1              lda       AMODE
                    cmpa      #INDY
                    bne       DOSET2
                    lda       #PAGE2
                    jsr       EMIT
                    lda       #INDX
                    sta       AMODE
DOSET2              equ       *

;               emit(baseop);
;               a = assarg();
;               if(a = 4) return(a);
;               emit(index offset);
;               if( amode == INDX )
;                  Buffptr += 2;      /* skip ,x or ,y */

                    lda       BASEOP
                    jsr       EMIT
                    jsr       ASSARG
                    cmpa      #$04
                    bne       DOSET22             ; jump if arg ok
                    rts

DOSET22             lda       SHFTREG+1           ; index offset
                    jsr       EMIT
                    lda       AMODE
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

DOSET33             lda       SHFTREG+1           ; mask
                    jsr       EMIT
                    lda       CLASS
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
                    lda       #$09                ; 'out of range'
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
                    lda       AMODE
                    ldb       CLASS
                    cmpa      #INDX
                    bne       BITOP1
                    rts

BITOP1              cmpa      #INDY
                    bne       BITOP2              ; jump not indexed
                    rts

BITOP2              cmpb      #SETCLR
                    bne       BITOP3              ; jump not bset,bclr
                    lda       BASEOP              ; get opcode
                    suba      #8
                    sta       BASEOP
                    rts

BITOP3              cmpb      #BTB
                    bne       BITOP4              ; jump not bit branch
                    lda       BASEOP              ; get opcode
                    suba      #12
                    sta       BASEOP
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
DOGEN               lda       AMODE
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

DOGDEF              lda       #$06                ; unknown addre...
                    rts

; *case LIMMED: epage(pnorm);
;             emit(baseop);
;             a = assarg();
;             if(a = 4) return(a);
;             emit(2 bytes);
;             return(0);

DOGLIM              lda       PNORM
                    jsr       EPAGE
DOGLIM1             lda       BASEOP
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

DOGIMM              lda       PNORM
                    jsr       EPAGE
                    lda       BASEOP
                    jsr       EMIT
                    bsr       ASSARG
                    cmpa      #$04
                    bne       DOGIMM1             ; jump if arg ok
                    rts

DOGIMM1             lda       SHFTREG+1
                    jsr       EMIT
                    clra
                    rts

; *case INDY: epage(py);
;           a=doindex(op+0x20);
;           return(a);

DOGINDY             lda       PY
                    jsr       EPAGE
                    lda       BASEOP
                    adda      #$20
                    sta       BASEOP
                    bsr       DOINDEX
                    rts

; *case INDX: epage(px);
;           a=doindex(op+0x20);
;           return(a);

DOGINDX             lda       PX
                    bsr       EPAGE
                    lda       BASEOP
                    adda      #$20
                    sta       BASEOP
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

DOGOTH0             lda       PNORM
                    bsr       EPAGE
                    lda       COUNT
                    cmpa      #$2
                    bgt       DOGOTH1
                    lda       BASEOP
                    adda      #$10                ; direct mode opcode
                    bsr       EMIT
                    lda       SHFTREG+1
                    bsr       EMIT
                    clra
                    rts

DOGOTH1             lda       BASEOP
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

DOINDEX             lda       BASEOP
                    bsr       EMIT
                    bsr       ASSARG
                    cmpa      #$04
                    bne       DOINDX0             ; jump if arg ok
                    rts

DOINDX0             cmpa      #','
                    beq       DOINDX1
                    lda       #$08                ; "syntax error"
                    rts

DOINDX1             jsr       INCBUFF
                    jsr       READBUFF
                    cmpa      #'Y'
                    beq       DOINDX2
                    cmpa      #'X'
                    beq       DOINDX2
                    ldx       MSGA7               ; "index addr assumed"
                    jsr       OUTSTRG
DOINDX2             lda       SHFTREG+1
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

ASSARG2             lda       #$04                ; bad argument
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
                    sta       ,x
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

MNETABL             equ       *
                    fcc       'ABA '              ; Mnemonic
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
                    lda       ,x                  ; opcode
                    ldb       #PG1
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
                    stb       PNORM               ; save page

; *If(opcode == ($00-$5F or $8D or $8F or $CF))
;  if(pnorm == (PG3 or PG4))
;      disillop(); return();
;  b=disrch(opcode,NULL);
;  if(b==0) disillop(); return();

                    lda       ,x                  ; get current opcode
                    sta       BASEOP
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

DIS1                ldb       PNORM
                    cmpb      #PG3
                    blo       DIS2                ; jump if page 1 or 2
                    jsr       DISILLOP            ; "illegal opcode"
                    rts

DIS2                ldb       BASEOP              ; opcode
                    clrb                          ; class=null
                    jsr       DISRCH
                    tstb
                    bne       DISPEC              ; jump if opcode found
                    jsr       DISILLOP            ; "illegal opcode"
                    rts

;   if(opcode==$8D) dissrch(opcode,REL);
;   if(opcode==($8F or $CF)) disrch(opcode,INH);

DISPEC              lda       BASEOP
                    cmpa      #$8D
                    bne       DISPEC1
                    ldb       #REL
                    bra       DISPEC3             ; look for BSR opcode

DISPEC1             cmpa      #$8F
                    beq       DISPEC2             ; jump if XGDX opcode
                    cmpa      #$CF
                    bne       DISINH              ; jump not STOP opcode
DISPEC2             ldb       #INH
DISPEC3             jsr       DISRCH              ; find other entry in table

;   if(class==INH)           /* INH */
;      if(pnorm==PG2)
;         b=disrch(baseop,P2INH);
;         if(b==0) disillop(); return();
;      prntmne();
;      return();

DISINH              equ       *
                    ldb       CLASS
                    cmpb      #INH
                    bne       DISREL              ; jump if not inherent
                    ldb       PNORM
                    cmpb      #PG1
                    beq       DISINH1             ; jump if page1
                    lda       BASEOP              ; get opcode
                    ldb       #P2INH              ; class=p2inh
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
                    ldb       CLASS
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
                    ldb       CLASS
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
                    ldb       CLASS
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
                    ldb       PNORM
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
                    lda       BASEOP              ; get opcode
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

DISNEW              lda       BASEOP
                    anda      #$CF
                    cmpa      #$8D
                    bne       DISNEW1             ; jump not jsr
                    ldb       #NIMM
                    bra       DISNEW4

DISNEW1             cmpa      #$8F
                    bne       DISNEW2             ; jump not sts
                    ldb       #NIMM
                    bra       DISNEW4

DISNEW2             cmpa      #$CF
                    bne       DISNEW3             ; jump not stx
                    ldb       #XNIMM
                    bra       DISNEW4

DISNEW3             cmpa      #$83
                    bne       DISGEN              ; jump not subd
                    ldb       #LIMM
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

DISGEN              ldb       CLASS               ; get class
                    cmpb      #GEN
                    beq       DISGEN1
                    cmpb      #NIMM
                    beq       DISGEN1
                    cmpb      #LIMM
                    bne       DISXLN              ; jump if other class
DISGEN1             lda       BASEOP
                    anda      #$CF
                    cmpa      #$83
                    bne       DISGEN3             ; jump if not #$83
                    ldb       PNORM
                    cmpb      #PG3
                    blo       DISGEN3             ; jump not pg3 or 4
                    ldb       #CPD
                    bsr       DISRCH              ; look for cpd mne
                    ldb       #LIMM
                    stb       CLASS               ; set class to limm
DISGEN3             ldb       PNORM
                    cmpb      #PG2
                    beq       DISGEN4             ; jump if page 2
                    cmpb      #PG4
                    bne       DISGEN5             ; jump not page 2 or 4
DISGEN4             lda       BASEOP
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

DISXLN              ldb       PNORM
                    cmpb      #PG2
                    beq       DISXLN1             ; jump if page2
                    cmpb      #PG3
                    bne       DISXLN4             ; jump not page3
DISXLN1             lda       BASEOP
                    anda      #$CF
                    ldb       CLASS
                    cmpb      #XLIMM
                    bne       DISXLN2
                    ldb       #YLIMM
                    bra       DISXLN3             ; look for ylimm

DISXLN2             ldb       #YNIMM              ; look for ynimm
DISXLN3             bsr       DISRCH
DISXLN4             ldb       PNORM
                    cmpb      #PG3
                    blo       DISXLN5             ; jump if page 1 or 2
                    lda       BASEOP              ; get opcode
                    anda      #$B0                ; mask bits 6,3-0
                    cmpa      #$A0
                    beq       DISXLN5             ; jump opcode = $Ax or $Ex
                    jsr       DISILLOP            ; "illegal opcode"
                    rts

DISXLN5             ldb       #LIMM
                    stb       CLASS
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
DISRCH1             cmpa      4,x                 ; test opcode
                    bne       DISRCH3             ; jump not this entry
                    tstb
                    beq       DISRCH2             ; jump if class=null
                    cmpb      5,x                 ; test class
                    bne       DISRCH3             ; jump not this entry
DISRCH2             ldb       5,x
                    stb       CLASS
                    stx       MNEPTR              ; return ptr to mnemonic
                    incb
                    rts                           ; return found

DISRCH3             pshb                          ; save class
                    ldb       #6
                    abx
                    ldb       ,x
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
                    lda       ,x
                    jsr       OUTA                ; output char1
                    lda       1,x
                    jsr       OUTA                ; output char2
                    lda       2,x
                    jsr       OUTA                ; output char3
                    lda       3,x
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
                    lda       #','
                    jsr       OUTA                ; output ,
                    ldb       PNORM
                    cmpb      #PG2
                    beq       DISIND1             ; jump if page2
                    cmpb      #PG4
                    bne       DISIND2             ; jump if not page4
DISIND1             lda       #'Y'
                    bra       DISIND3

DISIND2             lda       #'X'
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
                    ldb       ,x                  ; get relative offset
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
                    lda       #'$'
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
                    lda       BASEOP              ; get opcode
                    anda      #$B0                ; mask bits 6,3-0
                    cmpa      #$80
                    bne       DISGRL2             ; jump if not immed
                    lda       #'#'                ; do immediate
                    jsr       OUTA
                    bsr       DISDIR
                    ldb       CLASS
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
                    lda       #'$'
                    jsr       OUTA
                    ldx       DISPC
                    jsr       OUT1BYT
                    stx       DISPC
                    rts

;*****************
; *disext() - output "$ next 2 bytes"
;*****************
DISEXT              equ       *
                    lda       #'$'
                    jsr       OUTA
                    ldx       DISPC
                    jsr       OUT2BSP
                    stx       DISPC
                    rts

;*****************
; *disillop() - output "illegal opcode"
;*****************
DISMSG1             fcc       'ILLOP'
                    fcb       EOT
DISILLOP            equ       *
                    pshx
                    ldx       #DISMSG1
                    jsr       OUTSTRG0            ; no cr
                    pulx
                    rts

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
;  boot [<addr>] - Use SCI to talk to an 'hc11 in
; boot mode.  Downloads 256 bytes starting at addr.
; Default addr = $2000.
;************

; *Get arguments
; *If no args, default $2000
BOOT                jsr       WSKIP
                    cmpa      #$0D
                    bne       BOT1                ; jump if arguments
                    ldy       #$2000
                    bra       BOT2                ; go - use default address

; *Else get arguments
BOT1                jsr       BUFFARG
                    tst       COUNT
                    beq       BOTERR              ; jump if no address
                    jsr       WSKIP
                    ldy       SHFTREG             ; start address
                    cmpa      #$D
                    beq       BOT2                ; go - use arguments
BOTERR              ldx       #MSG9               ; "bad argument"
                    jsr       OUTSTRG
                    rts

; *Boot routine
BOT2                ldb       #$FF                ; control character ($ff -> download)
                    bsr       BTSUB               ; set up SCI and send control char

; *Download 256 byte block
                    clrb                          ; counter
BLOP                lda       ,y
                    sta       JSCDAT,x            ; write to transmitter
                    iny
                    brclr     JSCSR,x,$80,*       ; wait for TDRE
                    decb
                    bne       BLOP
                    rts

;************************************************
; *Subroutine
;  btsub   - sets up SCI and outputs control character
; On entry, B = control character
; On exit,  X = $1000
;           A = $0C
;***************************

BTSUB               equ       *
                    ldx       #$1000              ; to use indexed addressing
                    lda       #$02
                    sta       JPORTD,x            ; drive transmitter line
                    sta       JDDRD,x             ; high
                    clr       JSCCR2,x            ; turn off XMTR and RCVR
                    lda       #$22                ; BAUD = /16
                    sta       JBAUD,x
                    lda       #$0C                ; TURN ON XMTR & RCVR
                    sta       JSCCR2,x
                    stb       JSCDAT,x
                    brclr     JSCSR,x,$80,*       ; wait for TDRE
                    rts

;******************
;
;       EVBTEST - This routine makes it a little easier
;       on us to test this board.
;
;******************

EVBTEST             lda       #$FF

                    sta       $1000               ; Write ones to port A

                    clr       AUTOLF              ; Turn off auto lf
                    jsr       HOSTCO              ; Connect host
                    jsr       HOSTINIT            ; Initialize host

                    lda       #$7f
                    jsr       HOSTOUT             ; Send Delete to Altos
                    lda       #$0d
                    jsr       HOSTOUT             ; Send <CR>
                    inc       AUTOLF              ; Turn on Auto LF
                    ldx       #INBUFF+5           ; Point at Load message
                    stx       PTR0                ; Set pointer for load command
                    ldy       #MSGEVB             ; Point at cat line
LOOP                lda       ,y                  ; Loop to xfer command line
                    cmpa      #04                 ; Into buffalo line buffer
                    beq       DONE                ; Quit on $04
                    sta       ,x
                    inx                           ; next character
                    iny
                    bra       LOOP

DONE                clr       TMP2                ; Set load vs. verify
                    jsr       LOAD1B              ; Jmp into middle of load
                    lds       #STACK              ; Reset Stack
                    jmp       $C0B3               ; Jump to Downloaded code

MSGEVB              fcc       'cat evbtest.out'
                    fcb       $0D
                    fcb       $04

                    org       ROMBS+$1FA0
;*** Jump table ***
.UPCASE             jmp       UPCASE

.WCHEK              jmp       WCHEK

.DCHEK              jmp       DCHEK

.INIT               jmp       INIT

.INPUT              jmp       INPUT

.OUTPUT             jmp       OUTPUT

.OUTLHL             jmp       OUTLHLF

.OUTRHL             jmp       OUTRHLF

.OUTA               jmp       OUTA

.OUT1BY             jmp       OUT1BYT

.OUT1BS             jmp       OUT1BSP

.OUT2BS             jmp       OUT2BSP

.OUTCRL             jmp       OUTCRLF

.OUTSTR             jmp       OUTSTRG

.OUTST0             jmp       OUTSTRG0

.INCHAR             jmp       INCHAR

.VECINT             jmp       VECINIT

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

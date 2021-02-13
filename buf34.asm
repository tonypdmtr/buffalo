;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
;*                  BUFFALO
;* "Bit User's Fast Friendly Aid to Logical Operation"
;*
;* Rev 2.0 - 4/23/85 - added disassembler.
;*                   - variables now PTRn and TMPn.
;* Rev 2.1 - 4/29/85 - added byte erase to chgbyt routine.
;* Rev 2.2 - 5/16/85 - added hooks for evb board - acia
;*                     drivers, init and host routines.
;*           7/8/85  - fixed dump wraparound problem.
;*           7/10/85 - added evm board commands.
;*                   - added fill instruction.
;*           7/18/85 - added jump to EEPROM.
;* Rev 2.3 - 8/22/85 - call targco to disconnect sci from host
;*                     in reset routine for evb board.
;*           10/3/85 - modified load for download through terminal.
;* Rev 2.4 - 7/1/86  - Changed DFLOP address to fix conflicts with
;*                     EEPROM.  (was at A000)
;* Rev 2.5 - 9/8/86  - Modified to provide additional protection from
;*                     program run-away on power down.  Also fixed bugs
;*                     in MM and MOVE.  Changed to 1 stop bit from 2.
;* Rev 2.6 - 9/25/86 - Modified boot routine for variable length download
;*                     for use with 'HC11E8.
;* Rev 3.0   1/15/87 - EEPROM programming routines consolidated into WRITE.
;*                     Fill, Assem, and breakpoints will now do EEPROM.
;*                   - Added compare a to $0D to WSKIP routine.
;*           2/11/87 - Set up load to detect receiver error.
;* Rev 3.2   7/7/87  - Add disassembly to trace.
;*                   - Add entries to jump table.
;*           9/20/87 - Rewrote trace to use XIRQ, added STOPAT Command
;*           11/24/87- Write block protect reg for 'E9 version
;*                   - Modified variable length download for use
;*                       with 'E9 bootloader (XBOOT command)
;* Rev 3.3   3/17/88 - Set I bit to block interrupts on Warm Start and
;*                       return from CALL command.
;*                   - Added EEMOD Command.
;*                   - Rearranged source so that HELP command overlaps
;*                       EEPROM in test mode.
;*           3/24/88 - Added '+', '-', '=', '.' to MEM and ASM commands.
;*                   - Added check for 16 byte boundary to MEM
;*                       space sub-command.
;*                   - LOAD command now puts dummy (~) command into
;*                       inbuff so that any stray cr's won`t hang.
;* Rev 3.4   8/15/88 - Changed WRITE subroutine so that config register
;*                       gets byte erased before programmed.  The original
;*                       value of config is used for EEBYTE so that config
;*                       RAM value doesn't get changed in test mode.
;*           8/17/88 - Fixed MOVE command so that it doesn't hang when move
;*                       is done to a ROM location.
;*                   - Added OFFSET command for download offset capability.
;*
;*******************************************************************************
;* Although the information contained herein, as well as any information       *
;* provided relative thereto, has been carefully reviewed and is believed      *
;* accurate, Motorola assumes no liability arising out of its application or   *
;* use, neither does it convey any license under its patent rights nor the     *
;* rights of others.                                                           *
;*******************************************************************************

;***************
;*   EQUATES   *
;***************
;Author             equ       Tony Fourcroy

RAM                 equ       $0000               ; start of ram
REGS                equ       $1000               ; start of registers
ROM                 equ       $E000               ; start of rom
DSTREE              equ       $B600               ; start of eeprom
DENDEE              equ       $B7FF               ; end of eeprom

;-------------------------------------------------------------------------------

?                   macro     Offset
                    mreq      1:Offset
~label~             set       REGS+~1~
                    endm

;-------------------------------------------------------------------------------

PORTA               @?        $00                 ; port a
PORTD               @?        $08                 ; port d
DDRD                @?        $09                 ; ddrd
PORTE               @?        $0A                 ; port e
CFORC               @?        $0B                 ; force output compare
TCNT                @?        $0E                 ; timer count
TOC5                @?        $1E                 ; oc5 reg
TCTL1               @?        $20                 ; timer control 1
TMSK1               @?        $22                 ; timer mask 1
TFLG1               @?        $23                 ; timer flag 1
TMSK2               @?        $24                 ; timer mask 2
BAUD                @?        $2B                 ; sci baud reg
SCCR1               @?        $2C                 ; sci control1 reg
SCCR2               @?        $2D                 ; sci control2 reg
SCSR                @?        $2E                 ; sci status reg
SCDR                @?        $2F                 ; sci data reg
BPROT               @?        $35                 ; block protect reg
OPTION              @?        $39                 ; option reg
COPRST              @?        $3A                 ; cop reset reg
PPROG               @?        $3B                 ; ee prog reg
HPRIO               @?        $3C                 ; hprio reg
CONFIG              @?        $3F                 ; config register
XINIT               equ       $103D               ; Out-of-reset INIT

DFLOP               equ       $4000               ; evb d flip flop
DUART               equ       $D000               ; duart address

DPORTA              equ       DUART
DPORTB              equ       DUART+8
ACIA                equ       $9800               ; ACIA address

PROMPT              equ       '>'
BUFFLNG             equ       35
CTLA                equ       $01                 ; exit host or assembler
CTLB                equ       $02                 ; send break to host
CTLW                equ       $17                 ; wait
CTLX                equ       $18                 ; abort
DEL                 equ       $7F                 ; abort
EOT                 equ       $04                 ; end of text/table
SWI                 equ       $3F                 ; SWI OPCODE
JMP                 equ       $7E                 ; JMP OPCODE

CR                  equ       13
LF                  equ       10

;*******************************************************************************
; Macros

OUTA                macro     [#]Character
                    lda       ~@~
                    jsr       ~0~
                    endm

;-------------------------------------------------------------------------------

cmd                 macro     'CMD'[,CMD]
                    mreq      1:'CMD'[,CMD]
                    mstr      1
                    mdef      2,~1.2.{:1-2}~
                    fcb       :1-2                ; LENGTH OF COMMAND
                    fcc       ~1~                 ; ASCII COMMAND STRING
                    dw        ~2~                 ; COMMAND ADDRESS
                    endm

;*******************************************************************************

;***************
;*     RAM     *
;***************
                    #RAM
                    org       $2D                 ;*** Buffalo ram space ***

                    rmb       20                  ; user stack area
USTACK              rmb       30                  ; monitor stack area
STACK               rmb       1
REGISTERS           rmb       9                   ; user's pc,y,x,a,b,c
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

;*******************************************************************************
;* ROM starts here
;*******************************************************************************

                    #ROM
                    org       ROM

;*******************************************************************************
;**  BUFFALO - This is where Buffalo starts
;** out of reset.  All initialization is done
;** here including determination of where the
;** user terminal is (SCI,ACIA, or DUART).
;*******************************************************************************

BUFFALO             proc
                    ldx       #PORTE
                    brclr     ,X,#1,Skip@@        ; if bit 0 of port e is 1
                    jmp       DSTREE              ; then jump to the start of EEPROM

Skip@@              lda       #$93
                    sta       OPTION              ; adpu, dly, irqe, cop
                    clra
                    sta       TMSK2               ; timer pre = %1 for trace
                    sta       BPROT               ; clear 'E9 eeprom block protect
                    ldx       #DSTREE             ; set up default eeprom address range
                    stx       STREE
                    ldx       #DENDEE
                    stx       ENDEE
                    clrx                          ; set up default download offset
                    stx       LDOFFST
                    lds       #STACK              ; monitor stack pointer
                    jsr       VECINIT
                    ldx       #USTACK
                    stx       SP                  ; default user stack
                    lda       TCTL1
                    ora       #$03
                    sta       TCTL1               ; force oc5 pin high for trace
                    lda       #$D0
                    sta       REGISTERS+8         ; default user ccr
                    ldd       #$3F0D              ; initial command is ?
                    std       INBUFF
                    jsr       BPCLR               ; clear breakpoints
                    clr       AUTOLF
                    inc       AUTOLF              ; auto cr/lf = on

          ; Determine type of external comm device - none, or ACIA

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
                    lda       #1
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
                    lda       #2
                    sta       EXTDEV              ; external device is duart A

; Find terminal port - SCI or external.

BUFF2               clr       IODEV
                    jsr       TARGCO              ; disconnect sci for evb board
                    bsr       SIGNON              ; initialize sci
                    lda       EXTDEV
                    beq       BUFF3               ; jump if no external device
                    sta       IODEV
                    bsr       SIGNON              ; initialize external device
BUFF3               clr       IODEV
                    jsr       INPUT               ; get input from sci port
                    cmpa      #CR
                    beq       BUFF4               ; jump if cr - sci is terminal port
                    lda       EXTDEV
                    beq       BUFF3               ; jump if no external device
                    sta       IODEV
                    jsr       INPUT               ; get input from external device
                    cmpa      #CR
                    beq       BUFF4               ; jump if cr - terminal found ext
                    bra       BUFF3

SIGNON              jsr       INIT                ; initialize device
                    ldx       #MSG1               ; buffalo message
                    jsr       OUTSTRG
                    rts

; Determine where host port should be.

BUFF4               clr       HOSTDEV             ; default - host = sci port
                    lda       IODEV
                    cmpa      #1
                    beq       BUFF5               ; default host if term = acia
                    lda       #3
                    sta       HOSTDEV             ; else host is duart port b
BUFF5

;*******************************************************************************
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
;*******************************************************************************
;* Prompt user
;*do
;*   a=input();
;*   if(a==(cntlx or del)) continue;
;*   elseif(a==backspace)
;*      b--;
;*      if(b<0) b=0;
;*   else
;*      if(a==cr && buffer empty)
;*         repeat last command;
;*      else put a into buffer;
;*         check if buffer full;
;*while(a != (cr or /)

MAIN                proc
                    sei                           ; block interrupts
                    lds       #STACK              ; initialize sp every time
                    clr       AUTOLF
                    inc       AUTOLF              ; auto cr/lf = on
                    jsr       OUTCRLF
                    lda       #PROMPT             ; prompt user
                    jsr       OUTPUT
                    clrb
Loop@@              jsr       INCHAR              ; read terminal
                    ldx       #INBUFF
                    abx                           ; pointer into buffer
                    cmpa      #CTLX
                    beq       MAIN                ; jump if cntl X
                    cmpa      #DEL
                    beq       MAIN                ; jump if del
                    cmpa      #8
                    bne       CR@@                ; jump if not bckspc
                    decb
                    blt       MAIN                ; jump if buffer empty
                    bra       Loop@@

CR@@                cmpa      #CR
                    bne       NotCR@@             ; jump if not cr
                    tstb
                    beq       COMM0               ; jump if buffer empty
                    sta       ,X                  ; put a in buffer
                    bra       COMM0

NotCR@@             sta       ,X                  ; put a in buffer
                    incb
                    cmpb      #BUFFLNG
                    ble       NotLong@@           ; jump if not long
                    ldx       #MSG3               ; "long"
                    jsr       OUTSTRG
                    bra       MAIN

NotLong@@           cmpa      #'/'
                    bne       Loop@@              ; jump if not "/"

;*******************************************************************************
;*  Parse out and evaluate the command field.
;*******************************************************************************
;*Initialize

COMM0               proc
                    clr       TMP1                ; Enable "/" command
                    clr       SHFTREG
                    clr       SHFTREG+1
                    clrb
                    ldx       #INBUFF             ; ptrbuff[] = inbuff[]
                    stx       PTR0
                    jsr       WSKIP               ; find first char

;*while((a=readbuff) != (cr or wspace))
;*     upcase(a);
;*     buffptr[b] = a
;*     b++
;*     if (b > 8) error(too long);
;*     if(a == "/")
;*          if(enabled) mslash();
;*          else error(command?);
;*     else hexbin(a);

COMM1               jsr       READBUFF            ; read from buffer
                    ldx       #COMBUFF
                    abx
                    bsr       UPCASE              ; convert to upper case
                    sta       ,X                  ; put in command buffer
                    cmpa      #CR
                    beq       SRCH                ; jump if cr
                    jsr       WCHEK
                    beq       SRCH                ; jump if wspac
                    jsr       INCBUFF             ; move buffer pointer
                    incb
                    cmpb      #8
                    ble       COMM2
                    ldx       #MSG3               ; "long"
                    jsr       OUTSTRG
                    bra       MAIN

COMM2               cmpa      #'/'
                    bne       COMM4               ; jump if not "/"
                    tst       TMP1
                    bne       COMM3               ; jump if not enabled
                    decb
                    stb       COUNT
                    ldx       #MSLASH
                    bra       EXEC                ; execute "/"

COMM3               ldx       #MSG8               ; "command?"
                    jsr       OUTSTRG
                    jmp       MAIN

COMM4               jsr       HEXBIN
                    bra       COMM1

;*******************************************************************************
;*   Search tables for command.  At this point,
;* COMBUFF holds the command field to be executed,
;* and B = # of characters in the command field.
;* The command table holds the whole command name
;* but only the first n characters of the command
;* must match what is in COMBUFF where n is the
;* number of characters entered by the user.
;*******************************************************************************
;*count = b;
;*ptr1 = comtabl;
;*while(ptr1[0] != end of table)
;*   ptr1 = next entry
;*   for(b=1; b=count; b++)
;*      if(ptr1[b] == combuff[b]) continue;
;*      else error(not found);
;*   execute task;
;*  return();
;*return(command not found);

SRCH                proc
                    stb       COUNT               ; size of command entered
                    ldx       #COMTABL            ; pointer to table
                    stx       PTR1                ; pointer to next entry
Again@@             ldx       PTR1
                    ldy       #COMBUFF            ; pointer to command buffer
                    ldb       ,x
                    cmpb      #$FF
                    bne       Skip@@
                    ldx       #MSG2               ; "command not found"
                    jsr       OUTSTRG
                    jmp       MAIN

Skip@@              pshx                          ; compute next table entry
                    addb      #3
                    abx
                    stx       PTR1
                    pulx
                    clrb
Loop@@              incb                          ; match characters loop
                    lda       1,X                 ; read table
                    cmpa      ,y                  ; compare to combuff
                    bne       Again@@             ; try next entry
                    inx                           ; move pointers
                    iny
                    cmpb      COUNT
                    blt       Loop@@              ; loop countu1 times
                    ldx       PTR1
                    dex:2
                    ldx       ,x                  ; jump address from table
EXEC                jsr       ,x                  ; call task as subroutine
                    jmp       MAIN

;*******************************************************************************
;*   UTILITY SUBROUTINES - These routines
;* are called by any of the task routines.
;*******************************************************************************
;*  UPCASE(a) - If the contents of A is alpha,
;* returns a converted to uppercase.
;*******************************************************************************

UPCASE              proc
                    cmpa      #'a'
                    blo       Exit@@              ; jump if < a

                    cmpa      #'z'
                    bhi       Exit@@              ; jump if > z

                    adda      #'A'-'a'            ; convert

Exit@@              rts

;*******************************************************************************
;* BPCLR() - Clear all entries in the table of breakpoints.
;*******************************************************************************

BPCLR               proc
                    ldx       #BRKTABL
                    ldb       #8
Loop@@              clr       ,x
                    inx
                    decb
                    bgt       Loop@@              ; loop 8 times
                    rts

;*******************************************************************************
;* RPRNT1(x) - Prints name and contents of a single
;* user register. On entry X points to name of register
;* in reglist.  On exit, a=register name.
;*******************************************************************************

REGLIST             fcc       'PYXABCS'           ; names
                    fcb       0,2,4,6,7,8,9       ; offset
                    fcb       1,1,1,0,0,0,1       ; size

RPRNT1              proc
                    lda       ,x
                    psha
                    pshx
                    jsr       OUTPUT              ; name
                    lda       #'-'
                    jsr       OUTPUT              ; dash
                    ldb       7,X                 ; contents offset
                    lda       14,X                ; bytesize
                    ldx       #REGISTERS          ; address
                    abx
                    tsta
                    beq       Skip@@              ; jump if 1 byte
                    jsr       OUT1BYT             ; 2 bytes
Skip@@              jsr       OUT1BSP
                    pulx
                    pula
                    rts

;*******************************************************************************
;* RPRINT() - Print the name and contents of all the user registers.
;*******************************************************************************

RPRINT              proc
                    pshx
                    ldx       #REGLIST
Loop@@              bsr       RPRNT1              ; print name
                    inx
                    cmpa      #'S'                ; s is last register
                    bne       Loop@@              ; jump if not done
                    pulx
                    rts

;*******************************************************************************
;* HEXBIN(a) - Convert the ASCII character in a
;* to binary and shift into shftreg.  Returns value
;* in tmp1 incremented if a is not hex.
;*******************************************************************************

HEXBIN              proc
                    psha
                    pshb
                    pshx
                    bsr       UPCASE              ; convert to upper case
                    cmpa      #'0'
                    blt       HEXNOT              ; jump if a < $30
                    cmpa      #'9'
                    ble       Number@@            ; jump if 0-9
                    cmpa      #'A'
                    blt       HEXNOT              ; jump if $39> a <$41
                    cmpa      #'F'
                    bgt       HEXNOT              ; jump if a > $46
                    adda      #9                  ; convert $A-$F
Number@@            anda      #$0F                ; convert to binary
                    ldx       #SHFTREG
                    ldb       #4
Loop@@              asl       1,X                 ; 2 byte shift through
                    rol       ,x                  ; carry bit
                    decb
                    bgt       Loop@@              ; shift 4 times
                    ora       1,X
                    sta       1,X
                    bra       HEXRTS

HEXNOT              inc       TMP1                ; indicate not hex
HEXRTS              pulx
                    pulb
                    pula
                    rts

;*******************************************************************************
;*  BUFFARG() - Build a hex argument from the
;* contents of the input buffer. Characters are
;* converted to binary and shifted into shftreg
;* until a non-hex character is found.  On exit
;* shftreg holds the last four digits read, count
;* holds the number of digits read, ptrbuff points
;* to the first non-hex character read, and A holds
;* that first non-hex character.
;*******************************************************************************
;*Initialize
;*while((a=readbuff()) not hex)
;*     hexbin(a);
;*return();

BUFFARG             proc
                    clr       TMP1                ; not hex indicator
                    clr       COUNT               ; # or digits
                    clr       SHFTREG
                    clr       SHFTREG+1
                    jsr       WSKIP
Loop@@              jsr       READBUFF            ; read char
                    bsr       HEXBIN
                    tst       TMP1
                    bne       :AnRTS              ; jump if not hex
                    inc       COUNT
                    jsr       INCBUFF             ; move buffer pointer
                    bra       Loop@@

;*******************************************************************************
;*  TERMARG() - Build a hex argument from the
;* terminal.  Characters are converted to binary
;* and shifted into shftreg until a non-hex character
;* is found.  On exit shftreg holds the last four
;* digits read, count holds the number of digits
;* read, and A holds the first non-hex character.
;*******************************************************************************
;*initialize
;*while((a=inchar()) == hex)
;*     if(a = cntlx or del)
;*          abort;
;*     else
;*          hexbin(a); countu1++;
;*return();

TERMARG             proc
                    clr       COUNT
                    clr       SHFTREG
                    clr       SHFTREG+1
Loop@@              jsr       INCHAR
                    cmpa      #CTLX
                    beq       CtrlX@@             ; jump if controlx
                    cmpa      #DEL
                    bne       Normal@@            ; jump if not delete
CtrlX@@             jmp       MAIN                ; abort

Normal@@            clr       TMP1                ; hex indicator
                    bsr       HEXBIN
                    tst       TMP1
                    bne       :AnRTS              ; jump if not hex
                    inc       COUNT
                    bra       Loop@@

;*******************************************************************************
;* CHGBYT() - If shftreg is not empty, put
;* contents of shftreg at address in X.  If X
;* is an address in EEPROM then program it.
;*******************************************************************************
;*if(count != 0)
;*   (x) = a;

CHGBYT              proc
                    tst       COUNT
                    beq       :AnRTS              ; quit if shftreg empty
                    lda       SHFTREG+1           ; get data into a
                    bsr       WRITE
                    rts

;*******************************************************************************
;* WRITE() - This routine is used to write the
;* contents of A to the address of X.  If the
;* address is in EEPROM, it will be programmed
;* and if it is already programmed, it will be
;* byte erased first.
;*******************************************************************************
;*if(X == config) then
;*   byte erase config;
;*if(X is eeprom)then
;*   if(not erased) then erase;
;*   program (x) = A;
;*write (x) = A;
;*if((x) != A) error(rom);

WRITE               proc
                    cpx       #CONFIG
                    beq       Config@@            ; jump if config
                    cpx       STREE               ; start of EE
                    blo       NotEE@@             ; jump if not EE
                    cpx       ENDEE               ; end of EE
                    bhi       NotEE@@             ; jump if not EE
                    pshb                          ; check if byte erased
                    ldb       ,x
                    cmpb      #$FF
                    pulb
                    beq       Skip@@              ; jump if erased
Config@@            bsr       EEBYTE              ; byte erase
Skip@@              bsr       EEWRIT              ; byte program
NotEE@@             sta       ,x                  ; write for non EE
                    cmpa      ,x
                    beq       :AnRTS              ; jump if write ok
                    pshx
                    ldx       #MSG6               ; "rom"
                    jsr       OUTSTRG
                    pulx
                    rts

;*******************************************************************************
;* EEWRIT(), EEBYTE(), EEBULK() -
;* These routines are used to program and EEPROM
;* locations.  eewrite programs the address in X with
;* the value in A, eebyte does a byte address at X,
;* and eebulk does a bulk of EEPROM.  Whether eebulk
;* erases the config or not depends on the address it
;* receives in X.
;*******************************************************************************

EEWRIT              proc
                    pshb                          ; program one byte at x
                    ldb       #$02
                    stb       PPROG
                    sta       ,x
                    incb
                    bra       EEPROG

EEBYTE              proc
                    pshb                          ; byte erase address x
                    ldb       #$16
                    stb       PPROG
                    ldb       #$FF
                    stb       ,x
                    ldb       #$17
                    bra       EEPROG

EEBULK              proc
                    pshb                          ; bulk erase eeprom
                    ldb       #$06
                    stb       PPROG
                    sta       ,x                  ; erase config or not ...
                    incb                          ; ... depends on X addr
EEPROG              bne       ACL1
                    clrb                          ; fail safe
ACL1                stb       PPROG
                    pulb

;*******************************************************************************

DLY10MS             proc
                    pshx                          ; delay 10ms at E = 2MHz
                    ldx       #3334
Loop@@              dex
                    bne       Loop@@
                    pulx
                    clr       PPROG
                    rts

;*******************************************************************************
;* READBUFF() -  Read the character in INBUFF
;* pointed at by ptrbuff into A.  Returns ptrbuff unchanged.
;*******************************************************************************

READBUFF            proc
                    pshx
                    ldx       PTR0
                    lda       ,x
                    pulx
                    rts

;*******************************************************************************
;* INCBUFF(), DECBUFF() - Increment or decrement ptrbuff.
;*******************************************************************************

INCBUFF             proc
                    pshx
                    ldx       PTR0
                    inx
                    bra       INCDEC

DECBUFF             proc
                    pshx
                    ldx       PTR0
                    dex

INCDEC              stx       PTR0
                    pulx
                    rts

;*******************************************************************************
;* WSKIP() - Read from the INBUFF until a
;* non whitespace (space, comma, tab) character
;* is found.  Returns ptrbuff pointing to the
;* first non-whitespace character and a holds
;* that character.  WSKIP also compares a to
;* CR and cond codes indicating the
;* results of that compare.
;*******************************************************************************

WSKIP               proc
                    bsr       READBUFF            ; read character
                    bsr       WCHEK
                    bne       Exit@@              ; jump if not wspc
                    bsr       INCBUFF             ; move pointer
                    bra       WSKIP               ; loop

Exit@@              cmpa      #CR
                    rts

;*******************************************************************************
;*  WCHEK(a) - Returns z=1 if a holds a
;* whitespace character, else z=0.
;*******************************************************************************

WCHEK               proc
                    cmpa      #','                ; comma
                    beq       :AnRTS

                    cmpa      #' '                ; space
                    beq       :AnRTS

                    cmpa      #9                  ; tab
                    rts

;*******************************************************************************
;* DCHEK(a) - Returns Z=1 if a = whitespace
;* or carriage return.  Else returns z=0.
;*******************************************************************************

DCHEK               proc
                    bsr       WCHEK
                    beq       :AnRTS              ; jump if whitespace
                    cmpa      #CR
                    rts

;*******************************************************************************
;*  CHKABRT() - Checks for a control x or delete
;* from the terminal.  If found, the stack is
;* reset and the control is transferred to main.
;* Note that this is an abnormal termination.
;*   If the input from the terminal is a control W
;* then this routine keeps waiting until any other
;* character is read.
;*******************************************************************************
;*a=input();
;*if(a=cntl w) wait until any other key;
;*if(a = cntl x or del) abort;

CHKABRT             proc
                    bsr       INPUT
                    beq       :AnRTS              ; jump if no input
                    cmpa      #CTLW
                    bne       CHK2                ; jump in not cntlw
CHKABRT1            bsr       INPUT
                    beq       CHKABRT1            ; jump if no input
CHK2                cmpa      #DEL
                    beq       CHK3                ; jump if delete
                    cmpa      #CTLX
                    beq       CHK3                ; jump if control x
                    cmpa      #CTLA
                    bne       :AnRTS              ; jump not control a
CHK3                jmp       MAIN                ; abort

;*******************************************************************************
;*  HOSTCO - connect sci to host for evb board.
;*  TARGCO - connect sci to target for evb board.
;*******************************************************************************

HOSTCO              proc
                    psha
                    lda       #1
                    sta       DFLOP               ; send 1 to d-flop
                    pula
                    rts

TARGCO              clr       DFLOP               ; send 0 to d-flop
                    rts                           ; return

;*******************************************************************************
;*     VECINIT - This routine checks for
;*        vectors in the RAM table.  All
;*        uninitialized vectors are programmed
;*        to JMP STOPIT
;*******************************************************************************

VECINIT             proc
                    ldx       #JSCI               ; Point to First RAM Vector
                    ldy       #STOPIT             ; Pointer to STOPIT routine
                    ldd       #JMP<8|3            ; A=JMP opcode; B=offset
Loop@@              cmpa      ,x
                    beq       Skip@@              ; If vector already in
                    sta       ,x                  ; install JMP
                    sty       1,X                 ; to STOPIT routine
Skip@@              abx                           ; Add 3 to point at next vector
                    cpx       #JCLM+3             ; Done?
                    bne       Loop@@              ; If not, continue loop
                    rts

STOPIT              proc
                    lda       #$50                ; Stop-enable; IRQ, XIRQ-Off
                    tap
                    stop                          ; You are lost! Shut down
                    bra       STOPIT              ; In case continue by XIRQ

;*******************************************************************************
;*   I/O MODULE
;*     Communications with the outside world.
;* 3 I/O routines (INIT, INPUT, and OUTPUT) call
;* drivers specified by IODEV (0=SCI, 1=ACIA,
;* 2=DUARTA, 3=DUARTB).
;*******************************************************************************
;*   INIT() - Initialize device specified by iodev.
;*******************************************************************************

INIT                proc
                    psha                          ; save registers
                    pshx
                    lda       IODEV
                    bne       INIT1               ; jump not sci
                    jsr       ONSCI               ; initialize sci
                    bra       Exit@@

INIT1               cmpa      #1
                    bne       INIT2               ; jump not acia
                    jsr       ONACIA              ; initialize acia
                    bra       Exit@@

INIT2               ldx       #DPORTA
                    cmpa      #2
                    beq       INIT3               ; jump duart a
                    ldx       #DPORTB
INIT3               bsr       ONUART              ; initialize duart
Exit@@              pulx                          ; restore registers
                    pula
                    rts

;*******************************************************************************
;*  INPUT() - Read device. Returns a=char or 0.
;*    This routine also disarms the cop.
;*******************************************************************************

INPUT               proc
                    pshx
                    lda       #$55                ; reset cop
                    sta       COPRST
                    coma
                    sta       COPRST
                    lda       IODEV
                    bne       INPUT1              ; jump not sci
                    jsr       INSCI               ; read sci
                    bra       Exit@@

INPUT1              cmpa      #1
                    bne       INPUT2              ; jump not acia
                    jsr       INACIA              ; read acia
                    bra       Exit@@

INPUT2              ldx       #DPORTA
                    cmpa      #2
                    beq       INPUT3              ; jump if duart a
                    ldx       #DPORTB
INPUT3              bsr       INUART              ; read uart
Exit@@              pulx
                    rts

;*******************************************************************************
;* OUTPUT() - Output character in A.
;* chrcnt indicates the current column on the
;* output display.  It is incremented every time
;* a character is outputted, and cleared whenever
;* the subroutine outcrlf is called.
;*******************************************************************************

OUTPUT              proc
                    pshd                          ; save registers
                    pshx
                    ldb       IODEV
                    bne       ?OUTPUT1            ; jump not sci
                    jsr       OUTSCI              ; write sci
                    bra       Exit@@

?OUTPUT1            cmpb      #1
                    bne       ?OUTPUT2            ; jump not acia
                    jsr       OUTACIA             ; write acia
                    bra       Exit@@

?OUTPUT2            ldx       #DPORTA
                    cmpb      #2
                    beq       ?OUTPUT3            ; jump if duart a
                    ldx       #DPORTB
?OUTPUT3            bsr       OUTUART             ; write uart
Exit@@              pulx
                    puld
                    inc       CHRCNT              ; increment column count
                    rts

;*******************************************************************************
;* ONUART(port) - Initialize a duart port.
;* Sets duart to internal clock, divide by 16,
;* 8 data + 1 stop bits.
;*******************************************************************************

ONUART              proc
                    lda       #$22
                    sta       2,X                 ; reset receiver
                    lda       #$38
                    sta       2,X                 ; reset transmitter
                    lda       #$40
                    sta       2,X                 ; reset error status
                    lda       #$10
                    sta       2,X                 ; reset pointer
                    clra
                    sta       DUART+4             ; clock source
                    sta       DUART+5             ; interrupt mask
                    lda       #$13
                    sta       ,x                  ; 8 data, no parity
                    lda       #7
                    sta       ,x                  ; 1 stop bits
                    lda       #$BB                ; baud rate (9600)
                    sta       1,X                 ; tx and rcv baud rate
                    lda       #5
                    sta       2,X                 ; enable tx and rcv
                    rts

;*******************************************************************************
;*   INUART(port) - Check duart for any input.
;*******************************************************************************

INUART              proc
                    lda       1,X                 ; read status
                    anda      #1                  ; check rxrdy
                    beq       :AnRTS              ; jump if no data
                    lda       3,X                 ; read data
                    anda      #$7F                ; mask parity
                    rts

;*******************************************************************************
;*   OUTUART(port) - Output the character in a.
;*        if autolf=1, transmits cr or lf as crlf.
;*******************************************************************************

OUTUART             proc
                    tst       AUTOLF
                    beq       Loop@@              ; jump if no autolf
                    bsr       Loop@@
                    cmpa      #CR
                    bne       Skip@@
                    lda       #LF                 ; if cr, output lf
                    bra       Loop@@

Skip@@              cmpa      #LF
                    bne       :AnRTS

                    lda       #CR                 ; if lf, output cr
Loop@@              ldb       1,X                 ; check status
                    andb      #$04
                    beq       Loop@@              ; loop until tdre=1
                    anda      #$7F                ; mask parity
                    sta       3,X                 ; send character
                    rts

;*******************************************************************************
;*   ONSCI() - Initialize the SCI for 9600
;*                 baud at 8 MHz Extal.
;*******************************************************************************

ONSCI               proc
                    lda       #$30
                    sta       BAUD                ; baud register
                    clr       SCCR1
                    lda       #$0C
                    sta       SCCR2               ; enable
                    rts

;*******************************************************************************
;*   INSCI() - Read from SCI.  Return a=char or 0.
;*******************************************************************************

INSCI               proc
                    lda       SCSR                ; read status reg
                    anda      #$20                ; check rdrf
                    beq       :AnRTS              ; jump if no data
                    lda       SCDR                ; read data
                    anda      #$7F                ; mask parity
                    rts

;*******************************************************************************
;*  OUTSCI() - Output A to sci. IF autolf = 1,
;*               cr and lf sent as crlf.
;*******************************************************************************

OUTSCI              proc
                    tst       AUTOLF
                    beq       OUTSCI2             ; jump if autolf=0
                    bsr       OUTSCI2
                    cmpa      #CR
                    bne       OUTSCI1
                    lda       #LF                 ; if cr, send lf
                    bra       OUTSCI2

OUTSCI1             cmpa      #LF
                    bne       :AnRTS

                    lda       #CR                 ; if lf, send cr
OUTSCI2             ldb       SCSR                ; read status
                    bitb      #$80
                    beq       OUTSCI2             ; loop until tdre=1
                    anda      #$7F                ; mask parity
                    sta       SCDR                ; send character
                    rts

;*******************************************************************************
;*   ONACIA - Initialize the ACIA for
;* 8 data bits, 1 stop bit, divide by 64 clock.
;*******************************************************************************

ONACIA              proc
                    ldx       #ACIA
                    lda       #$03
                    sta       ,X                  ; master reset
                    lda       #$16
                    sta       ,X                  ; setup
                    rts

;*******************************************************************************
;* INACIA - Read from the ACIA, Return a=char or 0.
;* Tmp3 is used to flag overrun or framing error.
;*******************************************************************************

INACIA              proc
                    ldx       #ACIA
                    lda       ,x                  ; read status register
                    psha
                    anda      #$30                ; check ov, fe
                    pula
                    beq       INACIA1             ; jump - no error
                    lda       #1
                    sta       TMP3                ; flag reciever error
                    bra       INACIA2             ; read data to clear status

INACIA1             anda      #$01                ; check rdrf
                    beq       :AnRTS              ; jump if no data
INACIA2             lda       1,X                 ; read data
                    anda      #$7F                ; mask parity
                    rts

;*******************************************************************************
;*  OUTACIA - Output A to acia. IF autolf = 1,
;*               cr or lf sent as crlf.
;*******************************************************************************

OUTACIA             proc
                    bsr       OUTACIA3            ; output char
                    tst       AUTOLF
                    beq       :AnRTS              ; jump no autolf
                    cmpa      #CR
                    bne       OUTACIA1
                    lda       #LF
                    bra       OUTACIA3            ; if cr, output lf

OUTACIA1            cmpa      #LF
                    bne       :AnRTS
                    lda       #CR                 ; if lf, output cr

OUTACIA3            ldx       #ACIA
                    ldb       ,x
                    bitb      #$2
                    beq       OUTACIA3            ; loop until tdre
                    anda      #$7F                ; mask parity
                    sta       1,X                 ; output
                    rts

;*******************************************************************************
;*** I/O UTILITY SUBROUTINES ***
;***These subroutines perform the neccesary data I/O operations.
;* OUTLHLF-Convert left 4 bits of A from binary to ASCII and output.
;* OUTRHLF-Convert right 4 bits of A from binary to ASCII and output.
;* OUT1BYT-Convert byte addresed by X from binary to ASCII and output.
;* OUT1BSP-Convert byte addressed by X from binary to ASCII and output followed by a space.
;* OUT2BSP-Convert 2 bytes addressed by X from binary to ASCII and  output followed by a space.
;* OUTSPAC-Output a space.
;* OUTCRLF-Output a line feed and carriage return.
;* OUTSTRG-Output the string of ASCII bytes addressed by X until EOT.
;* OUTA-Output the ASCII character in A.
;* TABTO-Output spaces until column 20 is reached.
;* INCHAR-Input to A and echo one character.  Loops until character read.
;*******************************************************************************

;*******************************************************************************
;* OUTRHLF(), OUTLHLF(), OUTA()
;* Convert A from binary to ASCII and output.
;* Contents of A are destroyed.
;*******************************************************************************

OUTLHLF             proc
                    lsra:4                        ; shift data to right

OUTRHLF             proc
                    anda      #$0F                ; mask top half
                    adda      #$90                ; convert to ascii
                    daa
                    adca      #$40
                    daa

OUTA                proc
                    jmp       OUTPUT              ; output character
;                   rts

;*******************************************************************************
;* OUT1BYT(x) - Convert the byte at X to two
;* ASCII characters and output. Return X pointing to next byte.
;*******************************************************************************

OUT1BYT             proc
                    psha
                    lda       ,x                  ; get data in a
                    bsr       OUTLHLF             ; output left half
                    lda       ,x                  ; get data in a
                    bsr       OUTRHLF             ; output right half
                    pula
                    inx
                    rts

;*******************************************************************************
;* OUT1BSP(x), OUT2BSP(x) - Output 1 or 2 bytes
;* at x followed by a space.  Returns x pointing to next byte.
;*******************************************************************************

OUT2BSP             proc
                    bsr       OUT1BYT             ; do first byte

OUT1BSP             proc
                    bsr       OUT1BYT             ; do next byte

OUTSPAC             proc
                    lda       #' '                ; output a space
                    jsr       OUTPUT
                    rts

;*******************************************************************************
;* OUTCRLF() - Output a Carriage return and a line feed.  Returns a = cr.
;*******************************************************************************

OUTCRLF             proc
                    lda       #CR                 ; cr
                    jsr       OUTPUT              ; output a
                    clra
                    jsr       OUTPUT              ; output padding
                    lda       #CR
                    clr       CHRCNT              ; zero the column counter
                    rts

;*******************************************************************************
;* OUTSTRG(x) - Output string of ASCII bytes
;* starting at x until end of text (EOT).  Can
;* be paused by control w (any char restarts).
;*******************************************************************************

OUTSTRG             proc
                    bsr       OUTCRLF

OUTSTRG0            proc
                    psha
Loop@@              lda       ,x                  ; read char into a
                    cmpa      #EOT
                    beq       Exit@@              ; jump if eot
                    jsr       OUTPUT              ; output character
                    inx
                    jsr       INPUT
                    beq       Loop@@              ; jump if no input
                    cmpa      #CTLW
                    bne       Loop@@              ; jump if not cntlw
WaitForInput@@      jsr       INPUT
                    beq       WaitForInput@@      ; jump if any input
                    bra       Loop@@

Exit@@              pula
                    rts

;*******************************************************************************
;* TABTO() - move cursor over to column 20.
;* while(chrcnt < 16) outspac.

TABTO               proc
                    psha
Loop@@              bsr       OUTSPAC
                    lda       CHRCNT
                    cmpa      #20
                    ble       Loop@@
                    pula
                    rts

;*******************************************************************************
;*  INCHAR() - Reads input until character sent.
;*    Echoes char and returns with a = char.

INCHAR              proc
                    jsr       INPUT
                    tsta
                    beq       INCHAR              ; jump if no input
                    jsr       OUTPUT              ; echo
                    rts

;*******************************************************************************
;*** COMMAND TABLE ***

COMTABL             @cmd      'ASSEM'
                    @cmd      'BREAK'
                    @cmd      'BULK'
                    @cmd      'BULKALL'
                    @cmd      'CALL'
                    @cmd      'DUMP'
                    @cmd      'EEMOD'
                    @cmd      'FILL'
                    @cmd      'GO'
                    @cmd      'HELP'
                    @cmd      'HOST'
                    @cmd      'LOAD'
                    @cmd      'MEMORY'
                    @cmd      'MOVE'
                    @cmd      'OFFSET'
                    @cmd      'PROCEED'
                    @cmd      'REGISTER'
                    @cmd      'STOPAT'
                    @cmd      'TRACE'
                    @cmd      'VERIFY'
                    @cmd      '?',HELP            ; initial command
                    @cmd      'XBOOT',BOOT
                    @cmd      '~',TILDE           ; dummy command for load

          ;*** Command names for EVM compatability ***

                    @cmd      'ASM',ASSEM
                    @cmd      'BF',FILL
                    @cmd      'COPY',MOVE
                    @cmd      'ERASE',BULK
                    @cmd      'MD',DUMP
                    @cmd      'MM',MEMORY
                    @cmd      'RD',REGISTER
                    @cmd      'RM',REGISTER
                    @cmd      'READ',MOVE
                    @cmd      'TM',HOST
                    @cmd      'TEST',EVBTEST

                    fcb       $FF

;*******************************************************************************
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

;*******************************************************************************
;*   break [-][<addr>] . . .
;* Modifies the breakpoint table.  More than
;* one argument can be entered on the command
;* line but the table will hold only 4 entries.
;* 4 types of arguments are implied above:
;* break           Prints table contents.
;* break <addr>    Inserts <addr>.
;* break -<addr>   Deletes <addr>.
;* break -         Clears all entries.
;*******************************************************************************
;* while 1
;*     a = wskip();
;*     switch(a)
;*          case(cr):
;*               bprint(); return;

BREAK               proc
                    jsr       WSKIP
                    bne       BRKDEL              ; jump if not cr
                    jsr       BPRINT              ; print table
                    rts

;*          case("-"):
;*               incbuff(); readbuff();
;*               if(dchek(a))          /* look for wspac or cr */
;*                    bpclr();
;*                    breaksw;
;*               a = buffarg();
;*               if( !dchek(a) ) return(bad argument);
;*               b = bpsrch();
;*               if(b >= 0)
;*                    brktabl[b] = 0;
;*               breaksw;

BRKDEL              proc
                    cmpa      #'-'
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
                    clr       1,X
BRKDEL3             bra       BREAK               ; do next argument

;*          default:
;*               a = buffarg();
;*               if( !dchek(a) ) return(bad argument);
;*               b = bpsrch();
;*               if(b < 0)            /* not already in table */
;*                    x = shftreg;
;*                    shftreg = 0;
;*                    a = x[0]; x[0] = $3F
;*                    b = x[0]; x[0] = a;
;*                    if(b != $3F) return(rom);
;*                    b = bpsrch();   /* look for hole */
;*                    if(b >= 0) return(table full);
;*                    brktabl[b] = x;
;*               breaksw;

BRKDEF              proc
                    jsr       BUFFARG             ; get argument
                    jsr       DCHEK
                    beq       BRKDEF1             ; jump if delimiter
                    ldx       #MSG9               ; "bad argument"
                    jsr       OUTSTRG
                    rts

BRKDEF1             bsr       BPSRCH              ; look for entry in table
                    tstb
                    bge       BREAK               ; jump if already in table

                    ldx       SHFTREG             ; x = new entry addr
                    lda       ,x                  ; save original contents
                    psha
                    lda       #SWI
                    jsr       WRITE               ; write to entry addr
                    ldb       ,x                  ; read back
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
                    stx       ,y                  ; put new entry in
                    jmp       BREAK               ; do next argument

;*******************************************************************************
;*   bprint() - print the contents of the table.
;*******************************************************************************

BPRINT              proc
                    jsr       OUTCRLF
                    ldx       #BRKTABL
                    ldb       #4
Loop@@              jsr       OUT2BSP
                    decb
                    bgt       Loop@@              ; loop 4 times
                    rts

;*******************************************************************************
;*   bpsrch() - search table for address in
;* shftreg. Returns b = index to entry or
;* b = -1 if not found.
;*******************************************************************************
;*for(b=0; b=6; b=+2)
;*     x[] = brktabl + b;
;*     if(x[0] = shftreg)
;*          return(b);
;*return(-1);

BPSRCH              proc
                    clrb
Loop@@              ldx       #BRKTABL
                    abx
                    ldx       ,x                  ; get table entry
                    cpx       SHFTREG
                    beq       :AnRTS              ; exit if match

                    incb:2
                    cmpb      #6
                    ble       Loop@@              ; loop 4 times
                    ldb       #$FF
                    rts

;*******************************************************************************
;* bulk  - Bulk erase the eeprom not config.
;* bulkall - Bulk erase eeprom and config.
;*******************************************************************************

BULK                proc
                    ldx       STREE
                    bra       BULK1

BULKALL             proc
                    ldx       #CONFIG
BULK1               lda       #$FF
                    jsr       EEBULK
                    rts

;*******************************************************************************
;*  dump [<addr1> [<addr2>]]  - Dump memory
;* in 16 byte lines from <addr1> to <addr2>.
;*   Default starting address is "current
;* location" and default number of lines is 8.
;*******************************************************************************
;*ptr1 = ptrmem;        /* default start address */
;*ptr2 = ptr1 + $80;    /* default end address */
;*a = wskip();
;*if(a != cr)
;*     a = buffarg();
;*     if(countu1 = 0) return(bad argument);
;*     if( !dchek(a) ) return(bad argument);
;*     ptr1 = shftreg;
;*     ptr2 = ptr1 + $80;  /* default end address */
;*     a = wskip();
;*     if(a != cr)
;*          a = buffarg();
;*          if(countu1 = 0) return(bad argument);
;*          a = wskip();
;*          if(a != cr) return(bad argument);
;*          ptr2 = shftreg;

DUMP                proc
                    ldx       PTRMEM              ; current location
                    stx       PTR1                ; default start
                    ldb       #$80
                    abx
                    stx       PTR2                ; default end
                    jsr       WSKIP
                    beq       DUMP1               ; jump - no arguments
                    jsr       BUFFARG             ; read argument
                    tst       COUNT
                    beq       Fail@@              ; jump if no argument
                    jsr       DCHEK
                    bne       Fail@@              ; jump if delimiter
                    ldx       SHFTREG
                    stx       PTR1
                    ldb       #$80
                    abx
                    stx       PTR2                ; default end address
                    jsr       WSKIP
                    beq       DUMP1               ; jump - 1 argument
                    jsr       BUFFARG             ; read argument
                    tst       COUNT
                    beq       Fail@@              ; jump if no argument
                    jsr       WSKIP
                    bne       Fail@@              ; jump if not cr
                    ldx       SHFTREG
                    stx       PTR2
                    bra       DUMP1               ; jump - 2 arguments

Fail@@              ldx       #MSG9               ; "bad argument"
                    jsr       OUTSTRG
                    rts

;*ptrmem = ptr1;
;*ptr1 = ptr1 & $fff0;

DUMP1               ldd       PTR1
                    std       PTRMEM              ; new current location
                    andb      #$F0
                    std       PTR1                ; start dump at 16 byte boundary

;*** dump loop starts here ***
;*do:
;*     output address of first byte;

Loop@@              jsr       OUTCRLF
                    ldx       #PTR1
                    jsr       OUT2BSP             ; first address

;*     x = ptr1;
;*     for(b=0; b=16; b++)
;*          output contents;

                    ldx       PTR1                ; base address
                    clrb                          ; loop counter
Dump@@              jsr       OUT1BSP             ; hex value loop
                    incb
                    cmpb      #$10
                    blt       Dump@@              ; loop 16 times

;*     x = ptr1;
;*     for(b=0; b=16; b++)
;*          a = x[b];
;*          if($7A < a < $20)  a = $20;
;*          output ascii contents;

                    clrb                          ; loop counter
DUMPASC             ldx       PTR1                ; base address
                    abx
                    lda       ,X                  ; ascii value loop
                    cmpa      #' '
                    blo       DUMP3               ; jump if non printable
                    cmpa      #$7A
                    bls       DUMP4               ; jump if printable
DUMP3               lda       #' '                ; space for non printables
DUMP4               jsr       OUTPUT              ; output ascii value
                    incb
                    cmpb      #$10
                    blt       DUMPASC             ; loop 16 times

;*     chkabrt();
;*     ptr1 = ptr1 + $10;
;*while(ptr1 <= ptr2);
;*return;

                    jsr       CHKABRT             ; check abort or wait
                    ldd       PTR1
                    addd      #$10                ; point to next 16 byte bound
                    std       PTR1                ; update ptr1
                    cpd       PTR2
                    bhi       :AnRTS              ; quit if ptr1 > ptr2
                    cpd       #0                  ; check wraparound at $ffff
                    bne       Loop@@              ; jump - no wraparound
                    ldd       PTR2
                    cpd       #$FFF0
                    blo       Loop@@              ; upper bound not at top
                    rts                           ; quit

;*******************************************************************************
;*   eemod [<addr1> [<addr2>]]
;* Modifies the eeprom address range.
;*  EEMOD                 -show ee address range
;*  EEMOD <addr1>         -set range to addr1 -> addr1+2k
;*  EEMOD <addr1> <addr2> -set range to addr1 -> addr2
;*******************************************************************************
;*if(<addr1>)
;*    stree = addr1;
;*    endee = addr1 + 2k bytes;
;*if(<addr2>)
;*    endee = addr2;
;*print(stree,endee);

EEMOD               proc
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

;*******************************************************************************
;*  fill <addr1> <addr2> [<data>]  - Block fill
;*memory from addr1 to addr2 with data.  Data
;*defaults to $FF.
;*******************************************************************************
;*get addr1 and addr2

FILL                proc
                    jsr       WSKIP
                    jsr       BUFFARG
                    tst       COUNT
                    beq       Fail@@              ; jump if no argument
                    jsr       WCHEK
                    bne       Fail@@              ; jump if bad argument
                    ldx       SHFTREG
                    stx       PTR1                ; address1
                    jsr       WSKIP
                    jsr       BUFFARG
                    tst       COUNT
                    beq       Fail@@              ; jump if no argument
                    jsr       DCHEK
                    bne       Fail@@              ; jump if bad argument
                    ldx       SHFTREG
                    stx       PTR2                ; address2

          ;Get data if it exists

                    lda       #$FF
                    sta       TMP2                ; default data
                    jsr       WSKIP
                    beq       FILL1               ; jump if default data
                    jsr       BUFFARG
                    tst       COUNT
                    beq       Fail@@              ; jump if no argument
                    jsr       WSKIP
                    bne       Fail@@              ; jump if bad argument
                    lda       SHFTREG+1
                    sta       TMP2

;*while(ptr1 <= ptr2)
;*   *ptr1 = data
;*   if(*ptr1 != data) abort

FILL1               jsr       CHKABRT             ; check for abort
                    ldx       PTR1                ; starting address
                    lda       TMP2                ; data
                    jsr       WRITE               ; write the data to x
                    cmpa      ,x
                    bne       FILLBAD             ; jump if no write
                    cpx       PTR2
                    beq       :AnRTS              ; quit yet?
                    inx
                    stx       PTR1
                    bra       FILL1               ; loop

Fail@@              ldx       #MSG9               ; "bad argument"
                    jsr       OUTSTRG
                    rts

FILLBAD             ldx       #PTR1               ; output bad address
                    jsr       OUT2BSP
                    rts

;*******************************************************************************
;*   MEMORY [<addr>]
;*   [<addr>]/
;* Opens memory and allows user to modify the
;*contents at <addr> or the last opened location.
;*    Subcommands:
;* [<data>]<cr>       - Close current location and exit.
;* [<data>]<lf><+>    - Close current and open next.
;* [<data>]<^><-><bs> - Close current and open previous.
;* [<data>]<sp>       - Close current and open next.
;* [<data>]</><=>     - Reopen current location.
;*     The contents of the current location is only
;*  changed if valid data is entered before each
;*  subcommand.
;* [<addr>]O - Compute relative offset from current
;*     location to <addr>.  The current location must
;*     be the address of the offset byte.
;*******************************************************************************
;*a = wskip();
;*if(a != cr)
;*     a = buffarg();
;*     if(a != cr) return(bad argument);
;*     if(countu1 != 0) ptrmem[] = shftreg;

MEMORY              proc
                    jsr       WSKIP
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

;*******************************************************************************
;* Subcommands
;*******************************************************************************
;*outcrlf();
;*out2bsp(ptrmem[]);
;*out1bsp(ptrmem[0]);

MEM1                jsr       OUTCRLF
MEM2                ldx       #PTRMEM
                    jsr       OUT2BSP             ; output address
MEM3                ldx       PTRMEM
                    jsr       OUT1BSP             ; output contents
                    clr       SHFTREG
                    clr       SHFTREG+1
;*while 1
;*a = termarg();
;*     switch(a)
;*          case(space):
;*             chgbyt();
;*             ptrmem[]++;
;*             if(ptrmem%16 == 0) start new line;
;*          case(linefeed | +):
;*             chgbyt();
;*             ptrmem[]++;
;*          case(up arrow | backspace | -):
;*               chgbyt();
;*               ptrmem[]--;
;*          case('/' | '='):
;*               chgbyt();
;*               outcrlf();
;*          case(O):
;*               d = ptrmem[0] - (shftreg);
;*               if($80 < d < $ff81)
;*                    print(out of range);
;*               countt1 = d-1;
;*               out1bsp(countt1);
;*          case(carriage return):
;*               chgbyt();
;*               return;
;*          default: return(command?)

MEM4                jsr       TERMARG
                    jsr       UPCASE
                    ldx       PTRMEM

                    cmpa      #' '
                    beq       MEMSP               ; jump if space

                    cmpa      #LF
                    beq       MEMLF               ; jump if linefeed

                    cmpa      #'+'
                    beq       MEMPLUS             ; jump if +

                    cmpa      #'^'
                    beq       MEMUA               ; jump if up arrow

                    cmpa      #'-'
                    beq       MEMUA               ; jump if -

                    cmpa      #8
                    beq       MEMUA               ; jump if backspace

                    cmpa      #'/'
                    beq       MEMSL               ; jump if /

                    cmpa      #'='
                    beq       MEMSL               ; jump if =

                    cmpa      #'O'
                    beq       MEMOFF              ; jump if O

                    cmpa      #CR
                    beq       MEMCR               ; jump if carriage ret

                    cmpa      #'.'
                    beq       :AnRTS              ; jump if .

                    ldx       #MSG8               ; "command?"
                    jsr       OUTSTRG
                    bra       MEM1

MEMSP               jsr       CHGBYT
                    inx
                    stx       PTRMEM
                    xgdx
                    andb      #$0F
                    bne       MEM3                ; continue same line
                    bra       MEM1                ; .. else start new line

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
                    tsta
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

MEMOFF3             decd                          ; b now has offset
                    stb       TMP4
                    jsr       OUTSPAC
                    ldx       #TMP4
                    jsr       OUT1BSP             ; output offset
                    jmp       MEM1                ; output cr, addr, contents

MEMCR               jsr       CHGBYT
                    rts                           ; exit task

;*******************************************************************************
;* move <src1> <src2> [<dest>]  - move
;* block at <src1> to <src2> to <dest>.
;* Moves block 1 byte up if no <dest>.
;*******************************************************************************
;*a = buffarg();
;*if(countu1 = 0) return(bad argument);
;*if( !wchek(a) ) return(bad argument);
;*ptr1 = shftreg;         /* src1 */

MOVE                proc
                    jsr       BUFFARG
                    tst       COUNT
                    beq       MOVERR              ; jump if no arg
                    jsr       WCHEK
                    bne       MOVERR              ; jump if no delim
                    ldx       SHFTREG             ; src1
                    stx       PTR1

;*a = buffarg();
;*if(countu1 = 0) return(bad argument);
;*if( !dchek(a) ) return(bad argument);
;*ptr2 = shftreg;         /* src2 */

                    jsr       BUFFARG
                    tst       COUNT
                    beq       MOVERR              ; jump if no arg
                    jsr       DCHEK
                    bne       MOVERR              ; jump if no delim
                    ldx       SHFTREG             ; src2
                    stx       PTR2

;*a = buffarg();
;*a = wskip();
;*if(a != cr) return(bad argument);
;*if(countu1 != 0) tmp2 = shftreg;  /* dest */
;*else tmp2 = ptr1 + 1;

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

;*if(src1 < dest <= src2)
;*     dest = dest+(src2-src1);
;*     for(x = src2; x = src1; x--)
;*          dest[0]-- = x[0]--;

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
                    lda       ,X                  ; char at src2
                    pshx
                    ldx       PTR3
                    jsr       WRITE               ; write a to x
                    cmpa      ,x
                    bne       MOVEBAD             ; jump if no write
                    dex
                    stx       PTR3
                    pulx
                    cpx       PTR1
                    beq       :AnRTS
                    dex
                    bra       MOVELP1             ; Loop SRC2 - SRC1 times

;* else
;*     for(x=src1; x=src2; x++)
;*          dest[0]++ = x[0]++;

MOVE3               ldx       PTR1                ; srce1
MOVELP2             jsr       CHKABRT             ; check for abort
                    lda       ,X
                    pshx
                    ldx       PTR3                ; dest
                    jsr       WRITE               ; write a to x
                    cmpa      ,x
                    bne       MOVEBAD             ; jump if no write
                    inx
                    stx       PTR3
                    pulx
                    cpx       PTR2
                    beq       :AnRTS
                    inx
                    bra       MOVELP2             ; Loop SRC2-SRC1 times

MOVEBAD             pulx                          ; restore stack
                    ldx       #PTR3
                    jsr       OUT2BSP             ; output bad address
                    rts

;*******************************************************************************
;*  assem(addr) -68HC11 line assembler/disassembler.
;*       This routine will disassemble the opcode at
;*<addr> and then allow the user to enter a line for
;*assembly. Rules for assembly are as follows:
;* -A '#' sign indicates immediate addressing.
;* -A ',' (comma) indicates indexed addressing
;*       and the next character must be X or Y.
;* -All arguments are assumed to be hex and the
;*       '$' sign shouldn't be used.
;* -Arguments should be separated by 1 or more
;*       spaces or tabs.
;* -Any input after the required number of
;*       arguments is ignored.
;* -Upper or lower case makes no difference.
;*
;*       To signify end of input line, the following
;*commands are available and have the indicated action:
;*   <cr>      - Finds the next opcode for
;*          assembly.  If there was no assembly input,
;*          the next opcode disassembled is retrieved
;*          from the disassembler.
;*   <lf><+>   - Works the same as carriage return
;*          except if there was no assembly input, the
;*          <addr> is incremented and the next <addr> is
;*          disassembled.
;*    <^><->   - Decrements <addr> and the previous
;*          address is then disassembled.
;*    </><=>   - Redisassembles the current address.
;*
;*       To exit the assembler use CONTROL A or . (period).
;*Of course control X and DEL will also allow you to abort.

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

MSGDIR              dw        MSGA1               ; message table index
                    dw        MSGA2
                    dw        MSGA3
                    dw        MSGA4
                    dw        MSGA5
                    dw        MSGA6
                    dw        MSGA7
                    dw        MSGA8
                    dw        MSGA9

MSGA1               fcc       'Immed mode illegal',EOT
MSGA2               fcc       'Error in Mne table',EOT
MSGA3               fcc       'Illegal bit op',EOT
MSGA4               fcc       'Bad argument',EOT
MSGA5               fcc       'Mnemonic not found',EOT
MSGA6               fcc       'Unknown addressing mode',EOT
MSGA7               fcc       'Indexed addressing assumed',EOT
MSGA8               fcc       'Syntax error',EOT
MSGA9               fcc       'Branch out of range',EOT

;*******************************************************************************
;*oldpc = rambase;
;*a = wskip();
;*if (a != cr)
;*   buffarg()
;*   a = wskip();
;*   if ( a != cr ) return(error);
;*   oldpc = a;

ASSEM               proc
                    ldx       #RAM
                    stx       OLDPC
                    jsr       WSKIP
                    beq       ASSLOOP             ; jump if no argument
                    jsr       BUFFARG
                    jsr       WSKIP
                    beq       Done@@              ; jump if argument ok
                    ldx       #MSGA4              ; "bad argument"
                    jsr       OUTSTRG
                    rts

Done@@              ldx       SHFTREG
                    stx       OLDPC

;*repeat
;*  pc = oldpc;
;*  out2bsp(pc);
;*  disassem();
;*  a=readln();
;*  asscomm = a;  /* save command */
;*  if(a == [^,+,-,/,=]) outcrlf;
;*  if(a == 0) return(error);

ASSLOOP             proc
                    ldx       OLDPC
                    stx       PC
                    jsr       OUTCRLF
                    ldx       #PC
                    jsr       OUT2BSP             ; output the address
                    jsr       DISASSM             ; disassemble opcode
                    jsr       TABTO
                    @outa     #PROMPT             ; prompt user
                    jsr       READLN              ; read input for assembly
                    sta       ASSCOMM

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

                    tsta
                    bne       ASSLP1              ; jump if none of above

                    rts                           ; return if bad input

ASSLP0              jsr       OUTCRLF
ASSLP1              jsr:5     OUTSPAC             ; come here for cr or lf

;*  b = parse(input); /* get mnemonic */
;*  if(b > 5) print("not found"); asscomm='/';
;*  elseif(b >= 1)
;*     msrch();
;*     if(class==$FF)
;*        print("not found"); asscomm='/';
;*     else
;*        a = doop(opcode,class);
;*        if(a == 0) dispc=0;
;*        else process error; asscomm='/';

                    jsr       PARSE
                    cmpb      #5
                    ble       ASSLP2              ; jump if mnemonic <= 5 chars
                    ldx       #MSGA5              ; "mnemonic not found"
                    jsr       OUTSTRG
                    bra       ASSLP5

ASSLP2              tstb
                    beq       ASSLP10             ; jump if no input
                    jsr       MSRCH
                    lda       CLASS
                    cmpa      #$FF
                    bne       ASSLP3
                    ldx       #MSGA5              ; "mnemonic not found"
                    jsr       OUTSTRG
                    bra       ASSLP5

ASSLP3              jsr       DOOP
                    tsta
                    bne       ASSLP4              ; jump if doop error
                    clrx
                    stx       DISPC               ; indicate good assembly
                    bra       ASSLP10

ASSLP4              deca                          ; a = error message index
                    tab
                    ldx       #MSGDIR
                    abx:2
                    ldx       ,x
                    jsr       OUTSTRG             ; output error message
ASSLP5              clr       ASSCOMM             ; error command

;*  /* compute next address - asscomm holds subcommand
;*     and dispc indicates if valid assembly occured. */
;*  if(asscomm== ^ or -) oldpc--;
;*  if(asscomm==(lf or + or cr)
;*     if(dispc==0) oldpc=pc;   /* good assembly */
;*     else
;*        if(asscomm==lf or +) dispc= ++oldpc;
;*        oldpc=dispc;
;*until(eot)

ASSLP10             lda       ASSCOMM
                    cmpa      #'^'
                    beq       ASSLPA              ; jump if '^'
                    cmpa      #'-'
                    bne       ASSLP11             ; jump not '-'
ASSLPA              ldx       OLDPC               ; back up for '^' or '-'
                    dex
                    stx       OLDPC
                    bra       ASSLP15

ASSLP11             cmpa      #LF
                    beq       ASSLP12             ; jump if linefeed
                    cmpa      #'+'
                    beq       ASSLP12             ; jump if '+'
                    cmpa      #CR
                    bne       ASSLP15             ; jump if not cr
ASSLP12             ldx       DISPC
                    bne       ASSLP13             ; jump if dispc != 0
                    ldx       PC
                    stx       OLDPC
                    bra       ASSLP15

ASSLP13             cmpa      #LF
                    beq       ASSLPB              ; jump not lf
                    cmpa      #'+'
                    bne       ASSLP14             ; jump not lf or '+'
ASSLPB              ldx       OLDPC
                    inx
                    stx       DISPC
ASSLP14             ldx       DISPC
                    stx       OLDPC
ASSLP15             jmp       ASSLOOP

;*******************************************************************************
;*  readln() --- Read input from terminal into buffer
;* until a command character is read (cr,lf,/,^).
;* If more chars are typed than the buffer will hold,
;* the extra characters are overwritten on the end.
;*  On exit: b=number of chars read, a=0 if quit,
;* else a=next command.
;*******************************************************************************
;*for(b==0;b<=bufflng;b++) inbuff[b] = cr;

READLN              proc
                    ldd       #CR<8               ; A = carriage ret, B = 0
Loop@@              ldx       #INBUFF
                    abx
                    sta       ,X                  ; initialize input buffer
                    incb
                    cmpb      #BUFFLNG
                    blt       Loop@@
;*b=0;
;*repeat
;*  if(a == (ctla, cntlc, cntld, cntlx, del))
;*     return(a=0);
;*  if(a == backspace)
;*     if(b > 0) b--;
;*     else b=0;
;*  else  inbuff[b] = upcase(a);
;*  if(b < bufflng) b++;
;*until (a == [cr,lf,+,^,-,/,=])
;*return(a);

                    clrb
MainLoop@@          jsr       INCHAR

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
                    bgt       MainLoop@@
                    bra       READLN              ; start over

RLN2                ldx       #INBUFF
                    abx
                    jsr       UPCASE
                    sta       ,X                  ; put char in buffer
                    cmpb      #BUFFLNG            ; max buffer length
                    bge       RLN3                ; jump if buffer full
                    incb                          ; move buffer pointer
RLN3                bsr       ASSCHEK             ; check for subcommand
                    bne       MainLoop@@
                    rts

RLNQUIT             clra                          ; quit
                    rts                           ; return

;*******************************************************************************
;* parse() -parse out the mnemonic from INBUFF
;* to COMBUFF. on exit: b=number of chars parsed.
;*******************************************************************************
;*combuff[3] = <space>;   initialize 4th character to space.
;*ptrbuff[] = inbuff[];
;*a=wskip();
;*for (b = 0; b = 5; b++)
;*   a=readbuff(); incbuff();
;*   if (a = (cr,lf,^,/,wspace)) return(b);
;*   combuff[b] = upcase(a);
;*return(b);

PARSE               proc
                    lda       #$20
                    sta       COMBUFF+3
                    ldx       #INBUFF             ; initialize buffer ptr
                    stx       PTR0
                    jsr       WSKIP               ; find first character
                    clrb
Loop@@              jsr       READBUFF            ; read character
                    jsr       INCBUFF
                    jsr       WCHEK
                    beq       :AnRTS              ; jump if whitespace
                    bsr       ASSCHEK
                    beq       :AnRTS              ; jump if end of line
                    jsr       UPCASE              ; convert to upper case
                    ldx       #COMBUFF
                    abx
                    sta       ,X                  ; store in combuff
                    incb
                    cmpb      #5
                    ble       Loop@@              ; loop 6 times
                    rts

;*******************************************************************************
;* asschek() -perform compares for lf, cr, ^, /, +, -, =
;*******************************************************************************

ASSCHEK             proc
                    cmpa      #LF                 ; linefeed
                    beq       :AnRTS

                    cmpa      #CR                 ; carriage ret
                    beq       :AnRTS

                    cmpa      #'^'                ; up arrow
                    beq       :AnRTS

                    cmpa      #'/'                ; slash
                    beq       :AnRTS

                    cmpa      #'+'                ; plus
                    beq       :AnRTS

                    cmpa      #'-'                ; minus
                    beq       :AnRTS

                    cmpa      #'='                ; equals
                    rts

;*******************************************************************************
;*  msrch() --- Search MNETABL for mnemonic in COMBUFF.
;*stores base opcode at baseop and class at class.
;*  Class = FF if not found.
;*******************************************************************************
;*while ( != EOF )
;*   if (COMBUFF[0-3] = MNETABL[0-3])
;*      return(MNETABL[4],MNETABL[5]);
;*   else *MNETABL =+ 6

MSRCH               proc
                    ldx       #MNETABL            ; pointer to mnemonic table
                    ldy       #COMBUFF            ; pointer to string
                    bra       MSRCH1

MSNEXT              ldb       #6
                    abx                           ; point to next table entry
MSRCH1              lda       ,X                  ; read table
                    cmpa      #EOT
                    bne       MSRCH2              ; jump if not end of table
                    lda       #$FF
                    sta       CLASS               ; FF = not in table
                    rts

MSRCH2              cmpa      ,Y                  ; op[0] = tabl[0] ?
                    bne       MSNEXT
                    lda       1,X
                    cmpa      1,Y                 ; op[1] = tabl[1] ?
                    bne       MSNEXT
                    lda       2,X
                    cmpa      2,Y                 ; op[2] = tabl[2] ?
                    bne       MSNEXT
                    lda       3,X
                    cmpa      3,Y                 ; op[2] = tabl[2] ?
                    bne       MSNEXT
                    ldd       4,X                 ; opcode, class
                    sta       BASEOP
                    stb       CLASS
                    rts

;*******************************************************************************
;**   doop(baseop,class) --- process mnemonic.
;**   on exit: a=error code corresponding to error messages.
;*******************************************************************************
;*amode = OTHER; /* addressing mode */
;*yflag = 0;     /* ynoimm, nlimm, and cpd flag */
;*x[] = ptrbuff[]

DOOP                proc
                    lda       #OTHER
                    sta       AMODE               ; mode
                    clr       YFLAG
                    ldx       PTR0

;*while (*x != end of buffer)
;*   if (x[0]++ == ',')
;*      if (x[0] == 'y') amode = INDY;
;*      else amod = INDX;
;*      break;
;*a = wskip()
;*if( a == '#' ) amode = IMMED;

DOPLP1              cpx       #ENDBUFF            ; (end of buffer)
                    beq       DOOP1               ; jump if end of buffer
                    ldd       ,X                  ; read 2 chars from buffer
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

DOOP1               jsr       WSKIP
                    cmpa      #'#'                ; look for immediate mode
                    bne       DOOP2
                    jsr       INCBUFF             ; point at argument
                    lda       #IMMED
                    sta       AMODE
DOOP2

;*switch(class)
                    ldb       CLASS
                    cmpb      #P2INH
                    beq       DOP2I

                    cmpb      #INH
                    beq       DOINH

                    cmpb      #REL
                    beq       DOREL

                    cmpb      #LIMM
                    jeq       DOLIM

                    cmpb      #NIMM
                    jeq       DONOI

                    cmpb      #GEN
                    jeq       DOGENE

                    cmpb      #GRP2
                    jeq       DOGRP

                    cmpb      #CPD
                    jeq       DOCPD

                    cmpb      #XNIMM
                    jeq       DOXNOI

                    cmpb      #XLIMM
                    jeq       DOXLI

                    cmpb      #YNIMM
                    jeq       DOYNOI

                    cmpb      #YLIMM
                    jeq       DOYLI

                    cmpb      #BTB
                    jeq       DOSET

                    cmpb      #SETCLR
                    jeq       DOSET

          ; default: return("error in mnemonic table");

                    lda       #2
                    rts

;*  case P2INH: emit(PAGE2)

DOP2I               proc
                    lda       #PAGE2
                    jsr       EMIT

;*  case INH: emit(baseop);
;*       return(0);

DOINH               proc
                    lda       BASEOP
                    jsr       EMIT
                    clra
                    rts

;*  case REL: a = assarg();
;*            if(a=4) return(a);
;*            d = address - pc + 2;
;*            if ($7f >= d >= $ff82)
;*               return (out of range);
;*            emit(opcode);
;*            emit(offset);
;*            return(0);

DOREL               proc
                    jsr       ASSARG
                    cmpa      #$04
                    beq       :AnRTS              ; exit if arg not ok

                    ldd       SHFTREG             ; get branch address
                    ldx       PC                  ; get program counter
                    inx:2                         ; point to end of opcode
                    stx       BRADDR
                    subd      BRADDR              ; calculate offset
                    std       BRADDR              ; save result
                    cpd       #$7F                ; in range ?
                    bls       DOREL2              ; jump if in range
                    cpd       #$FF80
                    bhs       DOREL2              ; jump if in range
                    lda       #$09                ; 'Out of range'
                    rts

DOREL2              lda       BASEOP
                    jsr       EMIT                ; emit opcode
                    lda       BRADDR+1
                    jsr       EMIT                ; emit offset
                    clra                          ; normal return
                    rts

;*  case LIMM: if (amode == IMMED) amode = LIMMED;

DOLIM               proc
                    lda       AMODE
                    cmpa      #IMMED
                    bne       DONOI
                    lda       #LIMMED
                    sta       AMODE

;*  case NIMM: if (amode == IMMED)
;*                return("Immediate mode illegal");

DONOI               proc
                    lda       AMODE
                    cmpa      #IMMED
                    bne       DOGENE              ; jump if not immediate
                    lda       #1                  ; "immediate mode illegal"
                    rts

;*  case GEN: dogen(baseop,amode,PAGE1,PAGE1,PAGE2);
;*            return;

DOGENE              proc
                    lda       #PAGE1
                    sta       PNORM
                    sta       PX
                    lda       #PAGE2
                    sta       PY
                    jsr       DOGEN
                    rts

;*  case GRP2: if (amode == INDY)
;*                emit(PAGE2);
;*                amode = INDX;
;*             if( amode == INDX )
;*                doindx(baseop);
;*             else a = assarg();
;*                if(a=4) return(a);
;*                emit(opcode+0x10);
;*                emit(extended address);
;*             return;

DOGRP               proc
                    lda       AMODE
                    cmpa      #INDY
                    bne       DOGRP1
                    lda       #PAGE2
                    jsr       EMIT
                    lda       #INDX
                    sta       AMODE

DOGRP1              lda       AMODE
                    cmpa      #INDX
                    bne       DOGRP2
                    jsr       DOINDEX
                    rts

DOGRP2              lda       BASEOP
                    adda      #16
                    jsr       EMIT
                    jsr       ASSARG
                    cmpa      #4
                    beq       :AnRTS              ; jump if bad arg
                    ldd       SHFTREG             ; extended address
                    jsr       EMIT
                    tba
                    jsr       EMIT
                    clra
                    rts

;*  case CPD: if (amode == IMMED)
;*               amode = LIMMED; /* cpd */
;*            if( amode == INDY ) yflag = 1;
;*            dogen(baseop,amode,PAGE3,PAGE3,PAGE4);
;*            return;

DOCPD               proc
                    lda       AMODE
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

;*  case XNIMM: if (amode == IMMED)      /* stx */
;*                 return("Immediate mode illegal");

DOXNOI              proc
                    lda       AMODE
                    cmpa      #IMMED
                    bne       DOXLI
                    lda       #1                  ; "immediate mode illegal"
                    rts

;*  case XLIMM: if (amode == IMMED)  /* cpx, ldx */
;*                 amode = LIMMED;
;*              dogen(baseop,amode,PAGE1,PAGE1,PAGE4);
;*              return;

DOXLI               proc
                    lda       AMODE
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

;*  case YNIMM: if (amode == IMMED)      /* sty */
;*                 return("Immediate mode illegal");

DOYNOI              proc
                    lda       AMODE
                    cmpa      #IMMED
                    bne       DOYLI
                    lda       #1                  ; "immediate mode illegal"
                    rts

;*  case YLIMM: if (amode == INDY) yflag = 1;/* cpy, ldy */
;*              if(amode == IMMED) amode = LIMMED;
;*              dogen(opcode,amode,PAGE2,PAGE3,PAGE2);
;*              return;

DOYLI               proc
                    lda       AMODE
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

;*  case BTB:        /* bset, bclr */
;*  case SETCLR: a = bitop(baseop,amode,class);
;*               if(a=0) return(a = 3);
;*               if( amode == INDY )
;*                  emit(PAGE2);
;*                  amode = INDX;

DOSET               proc
                    bsr       BITOP
                    tsta
                    bne       DOSET1
                    lda       #3                  ; "illegal bit op"
                    rts

DOSET1              lda       AMODE
                    cmpa      #INDY
                    bne       DOSET2
                    lda       #PAGE2
                    jsr       EMIT
                    lda       #INDX
                    sta       AMODE
DOSET2

;*               emit(baseop);
;*               a = assarg();
;*               if(a = 4) return(a);
;*               emit(index offset);
;*               if( amode == INDX )
;*                  Buffptr += 2;      /* skip ,x or ,y */

                    lda       BASEOP
                    jsr       EMIT
                    jsr       ASSARG
                    cmpa      #4
                    bne       DOSET22             ; jump if arg ok
                    rts

DOSET22             lda       SHFTREG+1           ; index offset
                    jsr       EMIT
                    lda       AMODE
                    cmpa      #INDX
                    bne       DOSET3
                    jsr       INCBUFF
                    jsr       INCBUFF
DOSET3

;*               a = assarg();
;*               if(a = 4) return(a);
;*               emit(mask);   /* mask */
;*               if( class == SETCLR )
;*                  return;

                    jsr       ASSARG
                    cmpa      #4
                    bne       DOSET33             ; jump if arg ok
                    rts

DOSET33             lda       SHFTREG+1           ; mask
                    jsr       EMIT
                    lda       CLASS
                    cmpa      #SETCLR
                    bne       DOSET4
                    clra
                    rts

;*               a = assarg();
;*               if(a = 4) return(a);
;*               d = (pc+1) - shftreg;
;*               if ($7f >= d >= $ff82)
;*                  return (out of range);
;*               emit(branch offset);
;*               return(0);

DOSET4              jsr       ASSARG
                    cmpa      #4
                    beq       :AnRTS              ; jump if arg not ok

                    ldx       PC                  ; program counter
                    inx                           ; point to next inst
                    stx       BRADDR              ; save pc value
                    ldd       SHFTREG             ; get branch address
                    subd      BRADDR              ; calculate offset
                    cpd       #$7F
                    bls       DOSET6              ; jump if in range
                    cpd       #$FF80
                    bhs       DOSET6              ; jump if in range
                    clra
                    jsr       EMIT
                    lda       #9                  ; 'out of range'
                    rts

DOSET6              tba                           ; offset
                    jsr       EMIT
                    clra
                    rts

;*******************************************************************************
;**   bitop(baseop,amode,class) --- adjust opcode on bit
;**       manipulation instructions.  Returns opcode in a
;**       or a = 0 if error
;*******************************************************************************
;*if( amode == INDX || amode == INDY ) return(op);
;*if( class == SETCLR ) return(op-8);
;*else if(class==BTB) return(op-12);
;*else fatal("bitop");

BITOP               proc
                    lda       AMODE
                    ldb       CLASS
                    cmpa      #INDX
                    bne       BITOP1
                    rts

BITOP1              cmpa      #INDY
                    beq       :AnRTS              ; jump indexed

                    cmpb      #SETCLR
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

;*******************************************************************************
;**   dogen(baseop,mode,pnorm,px,py) - process
;** general addressing modes. Returns a = error #.
;*******************************************************************************
;*pnorm = page for normal addressing modes: IMM,DIR,EXT
;*px = page for INDX addressing
;*py = page for INDY addressing
;*switch(amode)

DOGEN               proc
                    lda       AMODE
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

;*default: error("Unknown Addressing Mode");

DOGDEF              proc
                    lda       #6                  ; unknown addre...
                    rts

;*case LIMMED: epage(pnorm);
;*             emit(baseop);
;*             a = assarg();
;*             if(a = 4) return(a);
;*             emit(2 bytes);
;*             return(0);

DOGLIM              proc
                    lda       PNORM
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

;*case IMMED: epage(pnorm);
;*            emit(baseop);
;*            a = assarg();
;*            if(a = 4) return(a);
;*            emit(lobyte);
;*            return(0);

DOGIMM              proc
                    lda       PNORM
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

;*case INDY: epage(py);
;*           a=doindex(op+0x20);
;*           return(a);

DOGINDY             proc
                    lda       PY
                    jsr       EPAGE
                    lda       BASEOP
                    adda      #32
                    sta       BASEOP
                    bsr       DOINDEX
                    rts

;*case INDX: epage(px);
;*           a=doindex(op+0x20);
;*           return(a);

DOGINDX             proc
                    lda       PX
                    bsr       EPAGE
                    lda       BASEOP
                    adda      #32
                    sta       BASEOP
                    bsr       DOINDEX
                    rts

;*case OTHER: a = assarg();
;*            if(a = 4) return(a);
;*            epage(pnorm);
;*            if(countu1 <= 2 digits)   /* direct */
;*               emit(op+0x10);
;*               emit(lobyte(Result));
;*               return(0);
;*            else    emit(op+0x30);    /* extended */
;*               eword(Result);
;*               return(0)

DOGOTH              proc
                    bsr       ASSARG
                    cmpa      #4
                    bne       DOGOTH0             ; jump if arg ok
                    rts

DOGOTH0             lda       PNORM
                    bsr       EPAGE
                    lda       COUNT
                    cmpa      #2
                    bgt       DOGOTH1
                    lda       BASEOP
                    adda      #16                 ; direct mode opcode
                    bsr       EMIT
                    lda       SHFTREG+1
                    bsr       EMIT
                    clra
                    rts

DOGOTH1             lda       BASEOP
                    adda      #48                 ; extended mode opcode
                    bsr       EMIT
                    ldd       SHFTREG
                    bsr       EMIT
                    tba
                    bsr       EMIT
                    clra
                    rts

;*******************************************************************************
;**  doindex(op) --- handle all wierd stuff for
;**   indexed addressing. Returns a = error number.
;*******************************************************************************
;*emit(baseop);
;*a=assarg();
;*if(a = 4) return(a);
;*if( a != ',' ) return("Syntax");
;*buffptr++
;*a=readbuff()
;*if( a != 'x' &&  != 'y') warn("Ind Addr Assumed");
;*emit(lobyte);
;*return(0);

DOINDEX             proc
                    lda       BASEOP
                    bsr       EMIT
                    bsr       ASSARG
                    cmpa      #4
                    bne       DOINDX0             ; jump if arg ok
                    rts

DOINDX0             cmpa      #','
                    beq       DOINDX1
                    lda       #8                  ; "syntax error"
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

;*******************************************************************************
;**   assarg(); - get argument.  Returns a = 4 if bad
;** argument, else a = first non hex char.
;*******************************************************************************
;*a = buffarg()
;*if(asschk(aa) && countu1 != 0) return(a);
;*return(bad argument);

ASSARG              proc
                    jsr       BUFFARG
                    jsr       ASSCHEK             ; check for command
                    beq       ASSARG1             ; jump if ok
                    jsr       WCHEK               ; check for whitespace
                    bne       ASSARG2             ; jump if not ok
ASSARG1             tst       COUNT
                    beq       ASSARG2             ; jump if no argument
                    rts

ASSARG2             lda       #4                  ; bad argument
                    rts

;*******************************************************************************
;**  epage(a) --- emit page prebyte
;*******************************************************************************
;*if( a != PAGE1 ) emit(a);

EPAGE               proc
                    cmpa      #PAGE1
                    beq       :AnRTS              ; jump if page 1

;*******************************************************************************
;*   emit(a) --- emit contents of a
;*******************************************************************************

EMIT                proc
                    ldx       PC
                    jsr       WRITE               ; write a to x
                    jsr       OUT1BSP
                    stx       PC
                    rts

;*Mnemonic table for hc11 line assembler

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

;*******************************************************************************
;* mnetabl - includes all '11 mnemonics, base opcodes,
;* and type of instruction.  The assembler search routine
;* depends on 4 characters for each mnemonic so that 3 char
;* mnemonics are extended with a space and 5 char mnemonics
;* are truncated.
;*******************************************************************************

MNETABL             fcc       'ABA ',$1B,INH      ; Mnemonic, Base opcode, Class
                    fcc       'ABX ',$3A,INH
                    fcc       'ABY ',$3A,P2INH
                    fcc       'ADCA',$89,GEN
                    fcc       'ADCB',$C9,GEN
                    fcc       'ADDA',$8B,GEN
                    fcc       'ADDB',$CB,GEN
                    fcc       'ADDD',$C3,LIMM
                    fcc       'ANDA',$84,GEN
                    fcc       'ANDB',$C4,GEN
                    fcc       'ASL ',$68,GRP2
                    fcc       'ASLA',$48,INH
                    fcc       'ASLB',$58,INH
                    fcc       'ASLD',$05,INH
                    fcc       'ASR ',$67,GRP2
                    fcc       'ASRA',$47,INH
                    fcc       'ASRB',$57,INH
                    fcc       'BCC ',$24,REL
                    fcc       'BCLR',$1D,SETCLR
                    fcc       'BCS ',$25,REL
                    fcc       'BEQ ',$27,REL
                    fcc       'BGE ',$2C,REL
                    fcc       'BGT ',$2E,REL
                    fcc       'BHI ',$22,REL
                    fcc       'BHS ',$24,REL
                    fcc       'BITA',$85,GEN
                    fcc       'BITB',$C5,GEN
                    fcc       'BLE ',$2F,REL
                    fcc       'BLO ',$25,REL
                    fcc       'BLS ',$23,REL
                    fcc       'BLT ',$2D,REL
                    fcc       'BMI ',$2B,REL
                    fcc       'BNE ',$26,REL
                    fcc       'BPL ',$2A,REL
                    fcc       'BRA ',$20,REL
                    fcc       'BRCL',$1F,BTB      ; (BRCLR)
                    fcc       'BRN ',$21,REL
                    fcc       'BRSE',$1E,BTB      ; (BRSET)
                    fcc       'BSET',$1C,SETCLR
                    fcc       'BSR ',$8D,REL
                    fcc       'BVC ',$28,REL
                    fcc       'BVS ',$29,REL
                    fcc       'CBA ',$11,INH
                    fcc       'CLC ',$0C,INH
                    fcc       'CLI ',$0E,INH
                    fcc       'CLR ',$6F,GRP2
                    fcc       'CLRA',$4F,INH
                    fcc       'CLRB',$5F,INH
                    fcc       'CLV ',$0A,INH
                    fcc       'CMPA',$81,GEN
                    fcc       'CMPB',$C1,GEN
                    fcc       'COM ',$63,GRP2
                    fcc       'COMA',$43,INH
                    fcc       'COMB',$53,INH
                    fcc       'CPD ',$83,CPD
                    fcc       'CPX ',$8C,XLIMM
                    fcc       'CPY ',$8C,YLIMM
                    fcc       'DAA ',$19,INH
                    fcc       'DEC ',$6A,GRP2
                    fcc       'DECA',$4A,INH
                    fcc       'DECB',$5A,INH
                    fcc       'DES ',$34,INH
                    fcc       'DEX ',$09,INH
                    fcc       'DEY ',$09,P2INH
                    fcc       'EORA',$88,GEN
                    fcc       'EORB',$C8,GEN
                    fcc       'FDIV',$03,INH
                    fcc       'IDIV',$02,INH
                    fcc       'INC ',$6C,GRP2
                    fcc       'INCA',$4C,INH
                    fcc       'INCB',$5C,INH
                    fcc       'INS ',$31,INH
                    fcc       'INX ',$08,INH
                    fcc       'INY ',$08,P2INH
                    fcc       'JMP ',$6E,GRP2
                    fcc       'JSR ',$8D,NIMM
                    fcc       'LDAA',$86,GEN
                    fcc       'LDAB',$C6,GEN
                    fcc       'LDD ',$CC,LIMM
                    fcc       'LDS ',$8E,LIMM
                    fcc       'LDX ',$CE,XLIMM
                    fcc       'LDY ',$CE,YLIMM
                    fcc       'LSL ',$68,GRP2
                    fcc       'LSLA',$48,INH
                    fcc       'LSLB',$58,INH
                    fcc       'LSLD',$05,INH
                    fcc       'LSR ',$64,GRP2
                    fcc       'LSRA',$44,INH
                    fcc       'LSRB',$54,INH
                    fcc       'LSRD',$04,INH
                    fcc       'MUL ',$3D,INH
                    fcc       'NEG ',$60,GRP2
                    fcc       'NEGA',$40,INH
                    fcc       'NEGB',$50,INH
                    fcc       'NOP ',$01,INH
                    fcc       'ORAA',$8A,GEN
                    fcc       'ORAB',$CA,GEN
                    fcc       'PSHA',$36,INH
                    fcc       'PSHB',$37,INH
                    fcc       'PSHX',$3C,INH
                    fcc       'PSHY',$3C,P2INH
                    fcc       'PULA',$32,INH
                    fcc       'PULB',$33,INH
                    fcc       'PULX',$38,INH
                    fcc       'PULY',$38,P2INH
                    fcc       'ROL ',$69,GRP2
                    fcc       'ROLA',$49,INH
                    fcc       'ROLB',$59,INH
                    fcc       'ROR ',$66,GRP2
                    fcc       'RORA',$46,INH
                    fcc       'RORB',$56,INH
                    fcc       'RTI ',$3B,INH
                    fcc       'RTS ',$39,INH
                    fcc       'SBA ',$10,INH
                    fcc       'SBCA',$82,GEN
                    fcc       'SBCB',$C2,GEN
                    fcc       'SEC ',$0D,INH
                    fcc       'SEI ',$0F,INH
                    fcc       'SEV ',$0B,INH
                    fcc       'STAA',$87,NIMM
                    fcc       'STAB',$C7,NIMM
                    fcc       'STD ',$CD,NIMM
                    fcc       'STOP',$CF,INH
                    fcc       'STS ',$8F,NIMM
                    fcc       'STX ',$CF,XNIMM
                    fcc       'STY ',$CF,YNIMM
                    fcc       'SUBA',$80,GEN
                    fcc       'SUBB',$C0,GEN
                    fcc       'SUBD',$83,LIMM
                    fcc       'SWI ',$3F,INH
                    fcc       'TAB ',$16,INH
                    fcc       'TAP ',$06,INH
                    fcc       'TBA ',$17,INH
                    fcc       'TPA ',$07,INH
                    fcc       'TEST',$00,INH
                    fcc       'TST ',$6D,GRP2
                    fcc       'TSTA',$4D,INH
                    fcc       'TSTB',$5D,INH
                    fcc       'TSX ',$30,INH
                    fcc       'TSY ',$30,P2INH
                    fcc       'TXS ',$35,INH
                    fcc       'TYS ',$35,P2INH
                    fcc       'WAI ',$3E,INH
                    fcc       'XGDX',$8F,INH
                    fcc       'XGDY',$8F,P2INH
                    fcc       'BRSE',$12,BTBD     ; bit direct modes for disassembler.
                    fcc       'BRCL',$13,BTBD
                    fcc       'BSET',$14,SETCLRD
                    fcc       'BCLR',$15,SETCLRD

                    fcb       EOT                 ; End of table

;*******************************************************************************

PG1                 equ       0
PG2                 equ       1
PG3                 equ       2
PG4                 equ       3

;*******************************************************************************
;*disassem() - disassemble the opcode.
;*******************************************************************************
;*(check for page prebyte)
;*baseop=pc[0];
;*pnorm=PG1;
;*if(baseop==$18) pnorm=PG2;
;*if(baseop==$1A) pnorm=PG3;
;*if(baseop==$CD) pnorm=PG4;
;*if(pnorm != PG1) dispc=pc+1;
;*else dispc=pc; (dispc points to next byte)

DISASSM             proc
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

;*If(opcode == ($00-$5F or $8D or $8F or $CF))
;*  if(pnorm == (PG3 or PG4))
;*      disillop(); return();
;*  b=disrch(opcode,NULL);
;*  if(b==0) disillop(); return();

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

;*   if(opcode==$8D) dissrch(opcode,REL);
;*   if(opcode==($8F or $CF)) disrch(opcode,INH);

DISPEC              proc
                    lda       BASEOP
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

;*   if(class==INH)           /* INH */
;*      if(pnorm==PG2)
;*         b=disrch(baseop,P2INH);
;*         if(b==0) disillop(); return();
;*      prntmne();
;*      return();

DISINH              proc
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

;*   elseif(class=REL)       /* REL */
;*      if(pnorm != PG1)
;*         disillop(); return();
;*      prntmne();
;*      disrelad();
;*      return();

DISREL              proc
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

;*   else           /* SETCLR,SETCLRD,BTB,BTBD */
;*      if(class == (SETCLRD or BTBD))
;*         if(pnorm != PG1)
;*            disillop(); return();   /* illop */
;*         prntmne();           /* direct */
;*         disdir();           /* output $byte */
;*      else (class == (SETCLR or BTB))
;*         prntmne();           /* indexed */
;*         disindx();
;*      outspac();
;*      disdir();
;*      outspac();
;*      if(class == (BTB or BTBD))
;*         disrelad();
;*   return();

DISBTD              proc
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

DISBIT              jsr       PRNTMNE
                    jsr       DISINDX             ; operand(indexed)
DISBIT1             jsr       OUTSPAC
                    jsr       DISDIR              ; mask
                    ldb       CLASS
                    cmpb      #BTB
                    beq       DISBIT2             ; jump if btb
                    cmpb      #BTBD
                    bne       :AnRTS              ; jump if not bit branch
DISBIT2             jsr       DISRELAD            ; relative address
                    rts

;*Elseif($60 <= opcode <= $7F)  /*  GRP2 */
;*   if(pnorm == (PG3 or PG4))
;*      disillop(); return();
;*   if((pnorm==PG2) and (opcode != $6x))
;*      disillop(); return();
;*   b=disrch(baseop & $6F,NULL);
;*   if(b==0) disillop(); return();
;*   prntmne();
;*   if(opcode == $6x)
;*      disindx();
;*   else
;*      disext();
;*   return();

DISGRP              proc
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

;*Else  ($80 <= opcode <= $FF)
;*   if(opcode == ($87 or $C7))
;*      disillop(); return();
;*   b=disrch(opcode&$CF,NULL);
;*   if(b==0) disillop(); return();

DISNEXT             proc
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

;*   if(opcode&$CF==$8D) disrch(baseop,NIMM; (jsr)
;*   if(opcode&$CF==$8F) disrch(baseop,NIMM; (sts)
;*   if(opcode&$CF==$CF) disrch(baseop,XNIMM; (stx)
;*   if(opcode&$CF==$83) disrch(baseop,LIMM); (subd)

DISNEW              proc
                    lda       BASEOP
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

;*   if(class == (GEN or NIMM or LIMM   ))   /* GEN,NIMM,LIMM,CPD */
;*      if(opcode&$CF==$83)
;*         if(pnorm==(PG3 or PG4)) disrch(opcode#$CF,CPD)
;*         class=LIMM;
;*      if((pnorm == (PG2 or PG4) and (opcode != ($Ax or $Ex)))
;*         disillop(); return();
;*      disgenrl();
;*      return();

DISGEN              proc
                    ldb       CLASS               ; get class
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

;*   else       /* XLIMM,XNIMM,YLIMM,YNIMM */
;*      if(pnorm==(PG2 or PG3))
;*         if(class==XLIMM) disrch(opcode&$CF,YLIMM);
;*         else disrch(opcode&$CF,YNIMM);
;*      if((pnorm == (PG3 or PG4))
;*         if(opcode != ($Ax or $Ex))
;*            disillop(); return();
;*      class=LIMM;
;*      disgen();
;*   return();

DISXLN              proc
                    ldb       PNORM
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

;*******************************************************************************
;*disrch(a=opcode,b=class)
;*return b=0 if not found
;*  else mneptr=points to mnemonic
;*        class=class of opcode
;*******************************************************************************
;*x=#MNETABL
;*while(x[0] != eot)
;*   if((opcode==x[4]) && ((class=NULL) || (class=x[5])))
;*      mneptr=x;
;*      class=x[5];
;*      return(1);
;*   x += 6;
;*return(0);      /* not found */

DISRCH              proc
                    ldx       #MNETABL            ; point to top of table
DISRCH1             cmpa      4,X                 ; test opcode
                    bne       DISRCH3             ; jump not this entry
                    tstb
                    beq       DISRCH2             ; jump if class=null
                    cmpb      5,X                 ; test class
                    bne       DISRCH3             ; jump not this entry
DISRCH2             ldb       5,X
                    stb       CLASS
                    stx       MNEPTR              ; return ptr to mnemonic
                    incb
                    rts                           ; return found

DISRCH3             pshb                          ; save class
                    ldb       #6
                    abx
                    ldb       ,X
                    cmpb      #EOT                ; test end of table
                    pulb
                    bne       DISRCH1
                    clrb
                    rts                           ; return not found

;*******************************************************************************
;*prntmne() - output the mnemonic pointed at by mneptr.
;*******************************************************************************
;*outa(mneptr[0-3]);
;*outspac;
;*return();

PRNTMNE             proc
                    ldx       MNEPTR

                    @outa     ,X                  ; output char1
                    @outa     1,X                 ; output char2
                    @outa     2,X                 ; output char3
                    @outa     3,X                 ; output char4

                    jsr       OUTSPAC
                    rts

;*******************************************************************************
;*disindx() - process indexed mode
;*******************************************************************************
;*disdir();
;*outa(',');
;*if(pnorm == (PG2 or PG4)) outa('Y');
;*else outa('X');
;*return();

DISINDX             proc
                    bsr       DISDIR              ; output $byte
                    @outa     #','                ; output ,
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

;*******************************************************************************
;*disrelad() - compute and output relative address.
;*******************************************************************************
;* braddr = dispc[0] + (dispc++);( 2's comp arith)
;*outa('$');
;*out2bsp(braddr);
;*return();

DISRELAD            proc
                    ldx       DISPC
                    ldb       ,X                  ; get relative offset
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
                    @outa     #'$'
                    ldx       #BRADDR
                    jsr       OUT2BSP             ; output address
                    rts

;*******************************************************************************
;*disgenrl() - output data for the general cases which
;*includes immediate, direct, indexed, and extended modes.
;*******************************************************************************
;*prntmne();
;*if(baseop == ($8x or $Cx))   /* immediate */
;*   outa('#');
;*   disdir();
;*   if(class == LIMM)
;*      out1byt(dispc++);
;*elseif(baseop == ($9x or $Dx))  /* direct */
;*   disdir();
;*elseif(baseop == ($Ax or $Ex)) /* indexed */
;*   disindx();
;*else  (baseop == ($Bx or $Fx)) /* extended */
;*   disext();
;*return();

DISGENRL            proc
                    bsr       PRNTMNE             ; print mnemonic
                    lda       BASEOP              ; get opcode
                    anda      #$B0                ; mask bits 6,3-0
                    cmpa      #$80
                    bne       DISGRL2             ; jump if not immed
                    @outa     #'#'                ; do immediate
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
                    bne       DISEXT              ; jump not indexed, do extended
                    bsr       DISINDX             ; do extended
                    rts

;*******************************************************************************
;*disdir() - output "$ next byte"
;*******************************************************************************

DISDIR              proc
                    @outa     #'$'
                    ldx       DISPC
                    jsr       OUT1BYT
                    stx       DISPC
                    rts

;*******************************************************************************
;*disext() - output "$ next 2 bytes"
;*******************************************************************************

DISEXT              proc
                    @outa     #'$'
                    ldx       DISPC
                    jsr       OUT2BSP
                    stx       DISPC
                    rts

;*******************************************************************************
;*disillop() - output "illegal opcode"
;*******************************************************************************

DISILLOP            proc
                    pshx
                    ldx       #Msg@@
                    jsr       OUTSTRG0            ; no cr
                    pulx
                    rts

Msg@@               fcc       'ILLOP',EOT

;*******************************************************************************
;*   help  -  List buffalo commands to terminal.
;*******************************************************************************

HELP                proc
                    ldx       #Msg@@
                    jsr       OUTSTRG             ; print help screen
                    rts

Msg@@               fcc       'ASM [<addr>]  Line asm/disasm',CR
                    fcc       '  [/,=]  Same addr,       [^,-]  Prev addr,       [+,CTLJ] Next addr',CR
                    fcc       '  [CR]  Next opcode,                              [CTLA,.]  Quit',CR
                    fcc       'BF <addr1> <addr2> [<data>]  Block fill memory',CR
                    fcc       'BR [-][<addr>] Set up bkpt table',CR
                    fcc       'BULK  Erase EEPROM,                 BULKALL  Erase EEPROM and CONFIG',CR
                    fcc       'CALL [<addr>] Call subroutine',CR
                    fcc       'GO [<addr>] Execute code at addr,        PROCEED  Continue execution',CR
                    fcc       'EEMOD [<addr> [<addr>]] Modify EEPROM range',CR
                    fcc       'LOAD, VERIFY [T] <host dwnld command>  Load or verify S-records',CR
                    fcc       'MD [<addr1> [<addr2>]]  Memory dump',CR
                    fcc       'MM [<addr>] or [<addr>]/  Memory Modify',CR
                    fcc       '  [/,=]  Same addr,  [^,-,CTLH] Prev addr,  [+,CTLJ,SPACE] Next addr',CR
                    fcc       '  <addr>O Compute offset,                   [CR]  Quit',CR
                    fcc       'MOVE <s1> <s2> [<d>]  Block move',CR
                    fcc       'OFFSET [-]<arg>  Offset for download',CR
                    fcc       'RM [P,Y,X,A,B,C,S]  Register modify',CR
                    fcc       'STOPAT <addr>  Trace until addr',CR
                    fcc       'T [<n>]  Trace n instructions',CR
                    fcc       'TM  Transparent mode (CTLA = exit, CTLB = send brk)',CR
                    fcc       '[CTLW]  Wait,          [CTLX,DEL] Abort         [CR] Repeat last cmd',CR

                    fcb       EOT

;*******************************************************************************
;* call [<addr>] - Execute a jsr to <addr> or user
;* pc value.  Return to monitor via  rts or breakpoint.
;*******************************************************************************
;*a = wskip();
;*if(a != cr)
;*     a = buffarg();
;*     a = wskip();
;*     if(a != cr) return(bad argument)
;*     pc = shftreg;

CALL                proc
                    jsr       WSKIP
                    beq       CALL3               ; jump if no arg
                    jsr       BUFFARG
                    jsr       WSKIP
                    beq       CALL2               ; jump if cr
                    ldx       #MSG9               ; "bad argument"
                    jsr       OUTSTRG
                    rts

CALL2               ldx       SHFTREG
                    stx       REGISTERS           ; pc = <addr>

;*put return address on user stack
;*setbps();
;*restack();     /* restack and go*/

CALL3               ldx       SP
                    dex                           ; user stack pointer
                    ldd       #RETURN             ; return address
                    std       ,x
                    dex
                    stx       SP                  ; new user stack pointer
                    bsr       SETBPS
                    clr       TMP2                ; 1=go, 0=call
                    jmp       RESTACK             ; go to user code

;*******************************************************************************
;* return() - Return here from rts after call command.
;*******************************************************************************

RETURN              proc
                    psha                          ; save a register
                    tpa
                    sta       REGISTERS+8         ; cc register
                    sei                           ; mask interrupts
                    pula
                    std       REGISTERS+6         ; a and b registers
                    stx       REGISTERS+4         ; x register
                    sty       REGISTERS+2         ; y register
                    sts       SP                  ; user stack pointer
                    lds       PTR2                ; monitor stack pointer
                    jsr       REMBPS              ; remove breakpoints
                    jsr       OUTCRLF
                    jsr       RPRINT              ; print user registers
                    rts

;*******************************************************************************
;* proceed - Same as go except it ignores
;* a breakpoint at the first opcode.  Calls
;* runone for the first instruction only.
;*******************************************************************************

PROCEED             proc
                    jsr       RUNONE              ; run one instruction
                    jsr       CHKABRT             ; check for abort
                    clr       TMP2                ; flag for breakpoints
                    inc       TMP2                ; 1=go 0=call
                    bsr       SETBPS
                    jmp       RESTACK             ; go execute

;*******************************************************************************
;* go [<addr>] - Execute starting at <addr> or
;* user's pc value.  Executes an rti to user code.
;* Returns to monitor via an swi through swiin.
;*******************************************************************************
;*a = wskip();
;*if(a != cr)
;*     a = buffarg();
;*     a = wskip();
;*     if(a != cr) return(bad argument)
;*     pc = shftreg;
;*setbps();
;*restack();     /* restack and go*/

GO                  proc
                    jsr       WSKIP
                    beq       GO2                 ; jump if no arg
                    jsr       BUFFARG
                    jsr       WSKIP
                    beq       CR@@                ; jump if cr
                    ldx       #MSG9               ; "bad argument"
                    jsr       OUTSTRG
                    rts

CR@@                ldx       SHFTREG
                    stx       REGISTERS           ; pc = <addr>
GO2                 clr       TMP2
                    inc       TMP2                ; 1=go, 0=call
                    bsr       SETBPS
                    jmp       RESTACK             ; go to user code

;*******************************************************************************
;** SWIIN - Breakpoints from go or call commands enter here.
;*Remove breakpoints, save user registers, return

SWIIN               proc
                    tsx                           ; user sp -> x
                    lds       PTR2                ; restore monitor sp
                    jsr       SAVSTACK            ; save user registers
                    bsr       REMBPS              ; remove breakpoints from code
                    ldx       REGISTERS
                    dex
                    stx       REGISTERS           ; save user pc value

          ; if(call command) remove call return addr from user stack;

                    tst       TMP2                ; 1=go, 0=call
                    bne       Go@@                ; jump if go command
                    ldx       SP                  ; remove return address
                    inx:2                         ; user stack pointer
                    stx       SP
Go@@                jsr       OUTCRLF             ; print register values
                    jsr       RPRINT
                    rts                           ; done

;*******************************************************************************
;* setbps - Replace user code with swi's at breakpoint addresses.
;*******************************************************************************
;*for(b=0; b=6; b =+ 2)
;*     x = brktabl[b];
;*     if(x != 0)
;*          optabl[b] = x[0];
;*          x[0] = $3F;
;*Put monitor SWI vector into jump table

SETBPS              proc
                    clrb
Loop@@              ldx       #BRKTABL
                    ldy       #PTR4
                    abx
                    aby
                    ldx       ,X                  ; breakpoint table entry
                    beq       Skip@@              ; jump if 0
                    lda       ,X                  ; save user opcode
                    sta       ,Y
                    lda       #SWI
                    jsr       WRITE               ; insert swi into code
Skip@@              addb      #2
                    cmpb      #6
                    ble       Loop@@              ; loop 4 times
                    ldx       JSWI+1
                    stx       PTR3                ; save user swi vector
                    lda       #JMP                ; jmp opcode
                    sta       JSWI
                    ldx       #SWIIN
                    stx       JSWI+1              ; monitor swi vector
                    rts

;*******************************************************************************
;*   rembps - Remove breakpoints from user code.
;*******************************************************************************
;*for(b=0; b=6; b =+ 2)
;*     x = brktabl[b];
;*     if(x != 0)
;*          x[0] = optabl[b];
;*Replace user's SWI vector

REMBPS              proc
                    clrb
Loop@@              ldx       #BRKTABL
                    ldy       #PTR4
                    abx
                    aby
                    ldx       ,X                  ; breakpoint table entry
                    beq       Skip@@              ; jump if 0
                    lda       ,Y
                    jsr       WRITE               ; restore user opcode
Skip@@              addb      #2
                    cmpb      #6
                    ble       Loop@@              ; loop 4 times
                    ldx       PTR3                ; restore user swi vector
                    stx       JSWI+1
                    rts

;*******************************************************************************
;* trace <n> - Trace n instructions starting
;* at user's pc value. n is a hex number less than $FF (defaults to 1).
;*******************************************************************************
;*a = wskip();
;*if(a != cr)
;*     a = buffarg(); a = wskip();
;*     if(a != cr) return(bad argument);
;*     countt1 = n

TRACE               proc
                    clr       TMP4
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

TRACE1              lda       SHFTREG+1           ; n
                    sta       TMP4

; Disassemble the line about to be traced

TRACE2              ldb       TMP4
                    pshb
                    ldx       REGISTERS
                    stx       PTR1                ; pc value for disass
                    jsr       DISASSM
                    pulb
                    stb       TMP4

;*run one instruction
;*rprint();
;*while(count > 0) continue trace;

                    bsr       RUNONE
                    jsr       CHKABRT             ; check for abort
                    jsr       TABTO               ; print registers for
                    jsr       RPRINT              ; result of trace
                    dec       TMP4
                    beq       :AnRTS              ; quit if count=0
TRACE3              jsr       OUTCRLF
                    bra       TRACE2

;*******************************************************************************
;* stopat <addr> - Trace instructions until <addr> is reached.
;*******************************************************************************
;*if((a=wskip) != cr)
;*     a = buffarg(); a = wskip();
;*     if(a != cr) return(bad argument);
;*else return(bad argument);

STOPAT              proc
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

;*while(!(ptrmem <= userpc < ptrmem+10)) runone();
;*rprint();

STOPGO              proc
                    ldd       REGISTERS           ; userpc
                    cpd       PTRMEM
                    blo       RunOne@@            ; if(userpc < ptrmem) runone
                    ldd       PTRMEM
                    addd      #10
                    cpd       REGISTERS
                    bhi       Done@@              ; quit if ptrmem+10 > userpc
RunOne@@            bsr       RUNONE
                    jsr       CHKABRT             ; check for abort
                    bra       STOPGO

Done@@              jsr       OUTCRLF
                    jsr       RPRINT              ; result of trace
                    rts                           ; done

;*******************************************************************************
;* runone - This routine is used by the trace and
;* execute commands to run one only one user instruction.
;*   Control is passed to the user code via an RTI.  OC5
;* is then used to trigger an XIRQ as soon as the first user
;* opcode is fetched.  Control then returns to the monitor
;* through XIRQIN.
;*  Externally, the OC5 pin must be wired to the XIRQ pin.
;*******************************************************************************
;* Disable oc5 interrupts
;* Put monitor XIRQ vector into jump table
;* Unmask x bit in user ccr
;* Setup OC5 to go low when first user instruction executed

RUNONE              proc
                    lda       #JMP                ; put "jmp xirqin" in jump table
                    sta       JTOC5
                    ldx       #XIRQIN
                    stx       JXIRQ+1
                    lda       REGISTERS+8         ; x bit will be cleared when
                    anda      #$BF                ; rti is executed below
                    sta       REGISTERS+8
                    ldb       #87                 ; cycles to end of rti
                    ldx       TCNT
                    abx                           ; 3~ /
                    stx       TOC5                ; oc5 match register 5~ /
                    lda       TCTL1               ; 4~ /
                    anda      #$FE                ; set up oc5 low on match 2~ /
                    sta       TCTL1               ; enable oc5 interrupt 4~ / 86~

;** RESTACK - Restore user stack and RTI to user code. 68~
;* This code is the pathway to execution of user code.
;*(Force extended addressing to maintain cycle count)
;*Restore user stack and rti to user code

RESTACK             proc
                    sts       >PTR2               ; save monitor sp
                    lds       >SP                 ; user stack pointer
                    ldx       >REGISTERS
                    pshx                          ; pc
                    ldx       >REGISTERS+2
                    pshx                          ; y
                    ldx       >REGISTERS+4
                    pshx                          ; x
                    ldd       >REGISTERS+6
                    psha                          ; a
                    pshb                          ; b
                    lda       >REGISTERS+8
                    psha                          ; ccr
                    rti

;** Return here from run one line of user code.

XIRQIN              proc
                    tsx                           ; user sp -> x
                    lds       PTR2                ; restore monitor sp

;** SAVSTACK - Save user's registers.
;* On entry - x points to top of user stack.

SAVSTACK            proc
                    lda       ,X
                    sta       REGISTERS+8         ; user ccr
                    ldd       1,X
                    sta       REGISTERS+7         ; b
                    stb       REGISTERS+6         ; a
                    ldd       3,X
                    std       REGISTERS+4         ; x
                    ldd       5,X
                    std       REGISTERS+2         ; y
                    ldd       7,X
                    std       REGISTERS           ; pc
                    ldb       #8
                    abx
                    stx       SP                  ; user stack pointer
                    lda       TCTL1               ; force oc5 pin high which
                    ora       #$03                ; is tied to xirq line
                    sta       TCTL1
                    lda       #$08
                    sta       CFORC
                    rts

;*******************************************************************************
;*   HOST() - Estb lishes transparent link between
;*       terminal and host.  Port used for host is
;*       determined in the reset initialization routine
;*       and stored in HOSTDEV.
;*          To exit type control A.
;*          To send break to host type control B.
;*if(no external device) return;
;*initialize host port;
;*While( !(control A))
;*     input(terminal); output(host);
;*     input(host); output(terminal);

HOST                proc
                    lda       EXTDEV
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

;*******************************************************************************
;* txbreak() - transmit break to host port.
;* The duration of the transmitted break is
;* approximately 200,000 E-clock cycles, or
;* 100ms at 2.0 MHz.
;*******************************************************************************

TXBREAK             proc
                    lda       HOSTDEV
                    cmpa      #$03
                    beq       TXBDU               ; jump if duartb is host

                    ldx       #SCCR2              ; sci is host
                    bset      ,X,#01              ; set send break bit
                    bsr       TXBWAIT
                    bclr      ,X,#01              ; clear send break bit
                    bra       CRLF@@

TXBDU               ldx       #DPORTB             ; duart host port
                    lda       #$60                ; start break cmd
                    sta       2,X                 ; port b command register
                    bsr       TXBWAIT
                    lda       #$70                ; stop break cmd
                    sta       2,X                 ; port b command register

CRLF@@              lda       #CR
                    bsr       HOSTOUT             ; send carriage return
                    lda       #LF
                    bsr       HOSTOUT             ; send linefeed
                    rts

TXBWAIT             proc
                    ldy       #28571              ; loop count = 28571
Loop@@              dey                           ; 7 cycle loop
                    bne       Loop@@
                    rts

;*******************************************************************************
;* hostinit(), hostin(), hostout() - host i/o
;* routines.  Restores original terminal device.
;*******************************************************************************

HOSTINIT            proc
                    ldb       IODEV               ; save terminal
                    pshb
                    ldb       HOSTDEV
                    stb       IODEV               ; point to host
                    jsr       INIT                ; initialize host
                    bra       TERMRES             ; restore terminal

HOSTIN              proc
                    ldb       IODEV               ; save terminal
                    pshb
                    ldb       HOSTDEV
                    stb       IODEV               ; point to host
                    jsr       INPUT               ; read host
                    bra       TERMRES             ; restore terminal

HOSTOUT             proc
                    ldb       IODEV               ; save terminal
                    pshb
                    ldb       HOSTDEV
                    stb       IODEV               ; point to host
                    jsr       OUTPUT              ; write to host
TERMRES             pulb                          ; restore terminal device
                    stb       IODEV
                    rts

;*******************************************************************************
;*   load(ptrbuff[]) - Load s1/s9 records from
;* host to memory.  Ptrbuff[] points to string in
;* input buffer which is a command to output s1/s9
;* records from the host ("cat filename" for unix).
;*    Returns error and address if it can't write
;* to a particular location.
;*******************************************************************************
;*   verify(ptrbuff[]) - Verify memory from load
;*command.  Ptrbuff[] is same as for load.
;* tmp3 is used as an error indication, 0=no errors,
;* 1=receiver, 2=rom error, 3=checksum error.
;*******************************************************************************

VERIFY              proc
                    clr       TMP2
                    inc       TMP2                ; TMP2=1=verify
                    bra       LOAD1

LOAD                proc
                    clr       TMP2                ; 0=load

;*a=wskip();
;*if(a = cr) goto transparent mode;
;*if(t option) hostdev = iodev;

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
                    cmpa      #CR
                    bne       LOAD3               ; jump if not t option
                    clr       AUTOLF
                    lda       IODEV
                    sta       HOSTDEV             ; set host port = terminal
                    bra       LOAD10              ; go wait for s1 records

;*else while(not cr)
;*     read character from input buffer;
;*     send character to host;

LOAD3               clr       AUTOLF
                    jsr       HOSTCO              ; connect sci (evb board)
                    bsr       HOSTINIT            ; initialize host port
LOAD4               jsr       READBUFF            ; get next char
                    jsr       INCBUFF
                    psha                          ; save char
                    bsr       HOSTOUT             ; output to host
                    jsr       OUTPUT              ; echo to terminal
                    pula
                    cmpa      #CR
                    bne       LOAD4               ; jump if not cr

;*repeat:                           /* look for s records */
;*      if(hostdev != iodev) check abort;
;*      a = hostin();
;*      if(a = 'S')
;*          a = hostin;
;*          if(a = '1')
;*              checksum = 0;
;*              get byte count in b;
;*              get base address in x;
;*              while(byte count > 0)
;*                  byte();
;*                  x++; b--;
;*                  if(tmp3=0)           /* no error */
;*                      if(load) x[0] = shftreg+1;
;*                      if(x[0] != shftreg+1)
;*                          tmp3 = 2;    /* rom error */
;*                          ptr3 = x;    /* save address */
;*              if(tmp3 = 0) do checksum;
;*              if(checksum err) tmp3 = 3; /* checksum error */

          ; Look for s-record header

LOAD10              lda       HOSTDEV
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

          ; Get Byte Count and Starting Address

                    jsr       BYTE
                    ldb       SHFTREG+1
                    subb      #$2                 ; b = byte count
                    bsr:2     BYTE
                    pshb                          ; save byte count
                    ldd       SHFTREG
                    addd      LDOFFST             ; add offset
                    xgdx                          ; x = address+offset
                    pulb                          ; restore byte count
                    dex                           ; condition for loop

          ; Get and Store Incoming Data Byte

LOAD20              bsr       BYTE                ; get next byte
                    inx
                    decb                          ; check byte count
                    beq       LOAD30              ; if b=0, go do checksum
                    tst       TMP3
                    bne       LOAD10              ; jump if error flagged
                    tst       TMP2
                    bne       LOAD21              ; jump if verify
                    lda       SHFTREG+1
                    jsr       WRITE               ; load only
LOAD21              cmpa      ,x                  ; verify ram location
                    beq       LOAD20              ; jump if ram ok
                    lda       #$02
                    sta       TMP3                ; indicate rom error
                    stx       PTR3                ; save error address
                    bra       LOAD20              ; finish download

          ; Get and Test Checksum

LOAD30              tst       TMP3
                    bne       LOAD10              ; jump if error already
                    lda       TMP4
                    inca                          ; do checksum
                    beq       LOAD10              ; jump if s1 record okay
                    lda       #$03
                    sta       TMP3                ; indicate checksum error
                    bra       LOAD10

;*          if(a = '9')
;*              read rest of record;
;*              if(tmp3=2) return("[ptr3]");
;*              if(tmp3=1) return("rcv error");
;*              if(tmp3=3) return("checksum err");
;*              else return("done");

LOAD90              bsr       BYTE
                    ldb       SHFTREG+1           ; b = byte count
LOAD91              bsr       BYTE
                    decb
                    bne       LOAD91              ; loop until end of record
                    ldb       #100
LOAD91A             jsr       DLY10MS             ; delay 1 sec -let host finish
                    decb
                    bne       LOAD91A
                    jsr       INPUT               ; clear comm device
                    ldd       #$7E0D              ; put dummy command in inbuff
                    std       INBUFF
                    inc       AUTOLF              ; turn on autolf
                    jsr       TARGCO              ; disconnect sci (evb)
                    ldx       #MSG11              ; "done" default msg
                    lda       TMP3
                    cmpa      #2
                    bne       LOAD92              ; jump not rom error
                    ldx       #PTR3
                    jsr       OUT2BSP             ; address of rom error
                    rts

LOAD92              cmpa      #1
                    bne       LOAD93              ; jump not rcv error
                    ldx       #MSG14              ; "rcv error"
                    bra       LOAD94

LOAD93              cmpa      #3
                    bne       LOAD94              ; jump not checksum error
                    ldx       #MSG12              ; "checksum error"
LOAD94              jsr       OUTSTRG
                    rts

;*******************************************************************************
;* byte() -  Read 2 ascii bytes from host and
;* convert to one hex byte.  Returns byte
;* shifted into shftreg and added to tmp4.
;*******************************************************************************

BYTE                proc
                    pshb
                    pshx
Loop@@              jsr       HOSTIN              ; read host (1st byte)
                    tsta
                    beq       Loop@@              ; loop until input
                    jsr       HEXBIN
Loop2@@             jsr       HOSTIN              ; read host (2nd byte)
                    tsta
                    beq       Loop2@@             ; loop until input
                    jsr       HEXBIN
                    lda       SHFTREG+1
                    adda      TMP4
                    sta       TMP4                ; add to checksum
                    pulx
                    pulb
                    rts

;*******************************************************************************
;*   offset [<addr>]
;* Specify offset to be added to s-record address when
;* downloading from the host.
;*  OFFSET                -show the current offset
;*  OFFSET <data>         -current offset = data
;*  OFFSET -<data>        -current offset = 0 - data
;*******************************************************************************
;*if(<data>) then offset = data;
;*print(offset);

OFFSET              proc
                    clr       TMP4                ; minus indicator
                    jsr       WSKIP
                    beq       CR@@                ; jump if cr (no argument)
                    cmpa      #'-'
                    bne       NotMinus@@          ; jump not -
                    inc       TMP4                ; set minus sign flag
                    jsr       INCBUFF             ; move buffer pointer
                    jsr       WSKIP
NotMinus@@          jsr       BUFFARG             ; read argument
                    tst       COUNT
                    beq       Fail@@              ; jump if bad argument
                    jsr       WSKIP
                    bne       Fail@@              ; jump if not cr
                    ldd       SHFTREG             ; get offset value
                    tst       TMP4
                    beq       Positive@@          ; jump if positive
                    clrd                          ; negative - sub from 0
                    subd      SHFTREG
Positive@@          std       LDOFFST
CR@@                jsr       OUTCRLF             ; display current offset
                    ldx       #LDOFFST
                    jsr       OUT2BSP
                    rts

Fail@@              ldx       #MSG9               ; "bad argument"
                    jsr       OUTSTRG
                    rts

;*******************************************************************************
;*   register [<name>]  - prints the user registers
;*and opens them for modification.  <name> is
;*the first register opened (default = P).
;*   Subcommands:
;* [<nn>]<space>  Opens the next register.
;* [<nn>]<cr>     Return.
;*    The register value is only changed if
;*    <nn> is entered before the subcommand.
;*******************************************************************************
;*x[] = reglist
;*a = wskip(); a = upcase(a);
;*if(a != cr)
;*     while( a != x[0] )
;*          if( x[0] = "s") return(bad argument);
;*          x[]++;
;*     incbuff(); a = wskip();
;*     if(a != cr) return(bad argument);

REGISTER            proc
                    ldx       #REGLIST
                    jsr       WSKIP               ; a = first char of arg
                    jsr       UPCASE              ; convert to upper case
                    cmpa      #$D
                    beq       REG4                ; jump if no argument
Loop@@              cmpa      ,x
                    beq       REG3
                    ldb       ,x
                    inx
                    cmpb      #'S'
                    bne       Loop@@              ; jump if not "s"
Fail@@              ldx       #MSG9               ; "bad argument"
                    jsr       OUTSTRG
                    rts

REG3                pshx
                    jsr       INCBUFF
                    jsr       WSKIP               ; next char after arg
                    pulx
                    bne       Fail@@              ; jump if not cr

;*rprint();
;*     while(x[0] != "s")
;*          rprnt1(x);
;*          a = termarg();    /* read from terminal */
;*          if( ! dchek(a) ) return(bad argument);
;*          if(countu1 != 0)
;*               if(x[14] = 1)
;*                    regs[x[7]++ = shftreg;
;*               regs[x[7]] = shftreg+1;
;*          if(a = cr) break;
;*return;

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
                    ldb       7,X                 ; get reg offset
                    lda       14,X                ; byte size
                    ldx       #REGISTERS          ; user registers
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
                    beq       :AnRTS              ; jump if "s"
                    inx                           ; point to next register
                    cmpa      #$D
                    bne       REG5                ; jump if not cr
                    rts

;*******************************************************************************
;* xboot [<addr1> [<addr2>]] - Use SCI to talk to an 'hc11 in
;* boot mode.  Downloads bytes from addr1 thru addr2.
;* Default addr1 = $C000 and addr2 = $C0ff.
;*
;* IMPORTANT:
;* if talking to an 'A8 or 'A2: use either default addresses or ONLY
;*    addr1 - this sends 256 bytes
;* if talking to an 'E9: include BOTH addr1 and addr2 for variable
;*    length
;*******************************************************************************

          ; Get arguments
          ; If no args, default $C000

BOOT                proc
                    jsr       WSKIP
                    bne       Args@@              ; jump if arguments
                    ldx       #$C0FF              ; addr2 default
                    stx       PTR5
                    ldy       #$C000              ; addr1 default
                    bra       Go@@                ; go - use default address

          ; Else get arguments

Args@@              jsr       BUFFARG
                    tst       COUNT
                    beq       Fail@@              ; jump if no address
                    ldy       SHFTREG             ; start address (addr1)
                    jsr       WSKIP
                    bne       Addr2@@             ; go get addr2
                    sty       PTR5                ; default addr2...
                    ldd       PTR5                ; ...by taking addr1...
                    addd      #$FF                ; ...and adding 255 to it...
                    std       PTR5                ; ...for a total download of 256
                    bra       Go@@                ; continue

Addr2@@             jsr       BUFFARG
                    tst       COUNT
                    beq       Fail@@              ; jump if no address
                    ldx       SHFTREG             ; end address (addr2)
                    stx       PTR5
                    jsr       WSKIP
                    beq       Go@@

Fail@@              ldx       #MSG9               ; "bad argument"
                    jsr       OUTSTRG
                    rts

          ; Boot routine

Go@@                ldb       #$FF                ; control character ($ff -> download)
                    bsr       BTSUB               ; set up SCI and send control char
                                                  ; initializes X as register pointer
          ; Download block

BLOP                lda       ,y
                    sta       SCDR                ; write to transmitter
                    bsr       WaitForSCI
                    cpy       PTR5                ; if last...
                    beq       :AnRTS              ; ...quit
                    iny                           ; else...
                    bra       BLOP                ; ...send next

WaitForSCI          proc
                    tst       SCSR
                    bpl       WaitForSCI          ; wait for TDRE

;*******************************************************************************
;* TILDE - This command is put into the combuff by the
;* load command so that extraneous carriage returns after
;* the load will not hang up.

TILDE               proc
                    rts

;*******************************************************************************
;* Subroutine
;* btsub   - sets up SCI and outputs control character
;* On entry, B = control character
;* On exit,  A = $0C
;*******************************************************************************

BTSUB               proc
                    lda       #$02
                    sta       PORTD               ; drive transmitter line
                    sta       DDRD                ; high
                    clr       SCCR2               ; turn off XMTR and RCVR
                    lda       #$22                ; BAUD = /16
                    sta       BAUD
                    lda       #$0C                ; TURN ON XMTR & RCVR
                    sta       SCCR2
                    stb       SCDR
                    bra       WaitForSCI

;*******************************************************************************
;* EVBTEST - This routine makes it a little easier on us to test this board.
;*******************************************************************************

EVBTEST             proc
                    lda       #$FF
                    sta       PORTA               ; Write ones to port A
                    clr       AUTOLF              ; Turn off auto lf
                    jsr       HOSTCO              ; Connect host
                    jsr       HOSTINIT            ; Initialize host
                    lda       #$7f
                    jsr       HOSTOUT             ; Send Delete to Altos
                    lda       #CR
                    jsr       HOSTOUT             ; Send <CR>
                    inc       AUTOLF              ; Turn on Auto LF
                    ldx       #INBUFF+5           ; Point at Load message
                    stx       PTR0                ; Set pointer for load command
                    ldy       #Msg@@              ; Point at cat line
Loop@@              lda       ,Y                  ; Loop to xfer command line
                    cmpa      #04                 ; Into buffalo line buffer
                    beq       Done@@              ; Quit on $04
                    sta       ,X
                    inx                           ; next character
                    iny
                    bra       Loop@@

Done@@              clr       TMP2                ; Set load vs. verify
                    jsr       LOAD3               ; Jmp into middle of load
                    lds       #STACK              ; Reset Stack
                    jmp       $C0B3               ; Jump to Downloaded code

Msg@@               fcc       'cat evbtest.out',CR,EOT

;*******************************************************************************
; Jump table
;*******************************************************************************

                    org       ROM+$1F7C

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

                    #VECTORS
                    org       ROM+$1FD6

VSCI                dw        JSCI
VSPI                dw        JSPI
VPAIE               dw        JPAIE
VPAO                dw        JPAO
VTOF                dw        JTOF
VTOC5               dw        JTOC5
VTOC4               dw        JTOC4
VTOC3               dw        JTOC3
VTOC2               dw        JTOC2
VTOC1               dw        JTOC1
VTIC3               dw        JTIC3
VTIC2               dw        JTIC2
VTIC1               dw        JTIC1
VRTI                dw        JRTI
VIRQ                dw        JIRQ
VXIRQ               dw        JXIRQ
VSWI                dw        JSWI
VILLOP              dw        JILLOP
VCOP                dw        JCOP
VCLM                dw        JCLM
VRST                dw        BUFFALO

                    end       :s19crc

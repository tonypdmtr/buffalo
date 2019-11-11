;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
;*                  BUFFALO
;* "Bit User's Fast Friendly Aid to Logical Operation"
;* Version modified for experimentation by Tony Papadimitriou <tonyp@acm.org>
;* (Some features may not yet work as expected.)
;*
;* ASM11 can be found here: http://www.aspisys.com/asm11.htm
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
;* Rev 3.41 98.08.01 - Modified for ASM11 by Tony G. Papadimitriou
;*          98.10.11 - Optimized JSR/RTS to JMP and BSR/RTS to BRA
;*          98.12.30 - More optimizations
;*          99.01.02 - Changed cmd table to ASCIIZ for easier command name mods
;*          99.02.11 - Formatted at columns 1, 21, 31, 51
;*          99.02.19 - Additional optimizations
;*          99.03.14 - More optimizations
;*          99.05.22 - More optimizations
;*          99.06.07 - Adapted to ASM11 v1.68
;*          99.07.08 - Adapted to ASM11 v1.74
;*          99.07.26 - Further optimizations
;*          99.07.31 - Major Revision (Removed all but SCI)
;*          99.09.22 - A lot of optimization
;*          99.09.23 - Added ASM11 default directives
;*          99.09.29 - Added segments
;*          99.11.22 - Added conditional for ASPiSYS F1 board
;*          99.12.11 - Added exp for exporting symbols
;*          00.04.21 - Added MHZ conditional for 4MHz bus
;*          00.07.14 - Added conditional for WSI
;*          00.09.07 - Reformatted HELP screen
;*          00.09.11 - Added CLS command to clear the screen
;*                     All code related to AUTOLF removed
;*          02.05.16 - Added EXP in subroutine vectors
;*          05.04.05 - Retouched
;* Rev 3.42 09.09.04 - OUTSTRG terminates on NULL (not EOT), and LF -> CR/LF
;*                     Added GetAsmCRC for CRC calculation
;
;         ****************************************************
;         *    Although the information contained herein,    *
;         *    as well as any information provided relative  *
;         *    thereto, has been carefully reviewed and is   *
;         *    believed accurate, Motorola or ASPiSYS assume *
;         *    no liability arising out of its application or*
;         *    use, neither does it convey any license under *
;         *    its patent rights nor the rights of others.   *
;         ****************************************************
                    #ListOff
                    #Uses     macros.inc
                    #ListOn
                    #Macro                        ;macros called as mnemonics

VERSION             equ       342                 ;version as x.xx

#ifdef ?
  #Hint +===================================================
  #Hint | Available conditionals (for use with -Dx option)
  #Hint +===================================================
  #Hint | DEBUG: for SIM11x runs (faster bps, etc.)
  #Hint | E9 : for 711E9 MCUs
  #Hint | F1 : for ASPiSYS F1 board (default)
  #Hint | WSI: for WSI DK68HC11 Development Kit
  #Hint | NOICE: for NOICE debugger
  #Hint | MHZ<n>: for specifying crystal frequency (n=8,16)
  #Hint +===================================================
  #Fatal Run ASM11 -Dx (where x is any of the above)
#endif

          #ifndef F1¦E9¦WSI¦NOICE
F1                  def       *
ROM                 def       $D000
          #endif
                    #CaseOff                      ;Case insensitive symbols
                    #ExtraOn                      ;Allow extra mnemonics

#ifdef NOICE
                    #Uses     noice.inc
#else
   #ifdef F1
                    #Uses     exp-f1.inc
   #else
          #ifdef WSI
                    #Uses     wsi.inc
          #else
                    #Uses     711e9.inc
          #endif
   #endif
                    #MEMORY   $FFC0 $FFD5
#endif
                    #OptRelOff
                    #OptRtsOn

          #ifdef DEBUG
                    #Message  DEBUG Mode (do NOT burn device)
          #endif

;**********
; EQUATES *
;**********
;Author             equ       Tony Fourcroy

RAMBS               def       RAM                 ; start of ram
ROMBS               def       ROM                 ; start of rom
DSTREE              def       EEPROM              ; start of eeprom
DENDEE              def       EEPROM_END          ; end of eeprom

PROMPT              equ       '>'
BUFFLNG             equ       35

CTL                 macro     letter,comment
                    mreq      1:letter,comment
~0~_~1~             equ       '~1~'-'A'+1         ;~2~
                    endm

                    @ctl      A,exit              ; host or assembler
                    @ctl      B,send              ; break to host
                    @ctl      W,wait
                    @ctl      X,abort

DEL                 equ       $7F                 ; abort
SWI_OPCODE          equ       $3F                 ; Opcode for SWI instruction

ADDR1               equ       $C000
ADDR2               equ       ADDR1|$00FF

          #ifdef DEBUG
BAUDRATE            def       $00                 ; 115200 bps rate for SIM11E to go faster
          #endif
          #if MHZ = 8
BAUDRATE            def       $30                 ; 9600 bps rate
          #else if MHZ = 16
BAUDRATE            def       $30                 ; 19200 bps rate
          #endif
          #ifdef __WSI__
BAUDRATE            def       $04                 ; 9600 bps rate
          #endif

;*******************************************************************************
                    #DATA
;*******************************************************************************
                    org       ROM_END&$F000+$0300  ; strings and other constants go here

;*******************************************************************************
                    #RAM
;*******************************************************************************
                    org       RAM+$2D

;*** Buffalo RAM space ***

                    rmb       20                  ; user stack area
USTACK              rmb       30                  ; monitor stack area
STACK               rmb       1
REGISTERS           rmb       9                   ; user's pc,y,x,a,b,c
UserSP              rmb       2                   ; user's sp
INBUFF              rmb       BUFFLNG             ; input buffer
ENDBUFF
COMBUFF             rmb       8                   ; command buffer
SHFTREG             rmb       2                   ; input shift register
STREE               rmb       2                   ; eeprom start address
ENDEE               rmb       2                   ; eeprom end address
BRKTABL             rmb       8                   ; breakpoint table
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
                    #ROM
;*******************************************************************************

;*******************************************************************************
; BUFFALO - This is where Buffalo starts out of reset.
;           All initialization is done here.
;*******************************************************************************

Start               proc
                    lda       #%10010011
                    sta       OPTION              ; adpu, dly, irqe, cop
                    clr       TMSK2               ; timer pre = %1 for trace

          #ifndef __WSI__
                    clr       BPROT               ; clear 'E9 eeprom block protect
          #endif
                    lds       #STACK              ; monitor stack pointer
          #ifdef __ASPISYS__
                    @SetChipSelects
          #endif
          #ifdef __WSI__
                    @setupwsi
          #endif
                    ldx       #DSTREE             ; set up default eeprom address range
                    stx       STREE
                    ldx       #DENDEE
                    stx       ENDEE
                    clrx                          ; set up default download offset
                    stx       LDOFFST
                    jsr       VECINIT
                    ldx       #USTACK
                    stx       UserSP              ; default user stack
                    lda       TCTL1
                    ora       #3
                    sta       TCTL1               ; force oc5 pin high for trace
                    lda       #$D0
                    sta       REGISTERS+8         ; default user CCR
                    ldd       #'?'<8|CR           ; initial command is ?
                    std       INBUFF
                    jsr       BPCLR               ; clear breakpoints
                    jsr       ONSCI               ; initialize device
                    ldx       #MSG1               ; buffalo message
                    jsr       OUTSTRG
;                   bra       MAIN

;*******************************************************************************
; MAIN - This module reads the user's input into
; a buffer called INBUFF.  The first field (assumed
; to be the command field) is then parsed into a
; second buffer called COMBUFF.  The command table
; is then searched for the contents of COMBUFF and
; if found, the address of the corresponding task
; routine is fetched from the command table.  The
; task is then called as a subroutine so that
; control returns back to here upon completion of
; the task.  Buffalo expects the following format
; for commands:
; <cmd>[<wsp><arg><wsp><arg>...]<cr>
; [] implies contents optional.
; <wsp> means whitespace character (space,comma,tab).
; <cmd> = command string of 1-8 characters.
; <arg> = Argument particular to the command.
; <cr> = Carriage return signifying end of input string.
;****************
; Prompt user
; do
; a=input();
; if(a==(cntlx or del)) continue;
; elseif(a==backspace)
; b--;
; if(b<0) b=0;
; else
; if(a==cr && buffer empty)
; repeat last command;
; else put a into buffer;
; check if buffer full;
; while(a != (cr or /)

MAIN                proc
                    sei                           ; block interrupts
                    lds       #STACK              ; initialize sp every time
                    jsr       OUTCRLF
                    lda       #PROMPT             ; prompt user
                    jsr       OUTPUT
                    clrb
Loop@@              jsr       INCHAR              ; read terminal
                    ldx       #INBUFF
                    abx                           ; pointer into buffer

                    cbeqa     #CTL_X,MAIN         ; jump if cntl X
                    cbeqa     #DEL,MAIN           ; jump if del
                    cbnea     #8,_1@@             ; jump if not bckspc

                    decb
                    blt       MAIN                ; jump if buffer empty
                    bra       Loop@@

_1@@                cbnea     #CR,_2@@            ; jump if not cr

                    tstb
                    beq       COMM0               ; jump if buffer empty

                    sta       ,x                  ; put A in buffer
                    bra       COMM0

_2@@                sta       ,x                  ; put A in buffer
                    incb
                    cmpb      #BUFFLNG
                    ble       _3@@                ; jump if not long
                    ldx       #MSG3               ; "long"
                    jsr       OUTSTRG
                    bra       MAIN

_3@@                cbnea     #'/',Loop@@         ; jump if not "/"
;                   bra       COMM0

;*******************************************************************************
; Parse out and evaluate the command field.
;*******************************************************************************
; Initialize

COMM0               proc
                    clrb
                    stb       TMP1                ; Enable "/" command
                    stb       SHFTREG
                    stb       SHFTREG+1
                    ldx       #INBUFF             ; ptrbuff[] = inbuff[]
                    stx       PTR0
                    jsr       WSKIP               ; find first char

; while((a=readbuff) != (cr or wspace))
; upcase(a);
; buffptr[b] = a
; b++
; if (b > 8) error(too long);
; if(a == "/")
; if(enabled) mslash();
; else error(command?);
; else hexbin(a);

Loop@@              jsr       READBUFF            ; read from buffer
                    ldx       #COMBUFF
                    abx
                    jsr       UPCASE              ; convert to upper case
                    sta       ,x                  ; put in command buffer
                    cbnea     #CR,_1@@
                    jsr       OUTPUT
                    lda       #LF
                    jsr       OUTPUT
                    bra       SRCH

_1@@                jsr       WCHEK
                    beq       SRCH                ; jump if wspac
                    jsr       INCBUFF             ; move buffer pointer
                    incb
                    cmpb      #8
                    ble       _2@@
                    ldx       #MSG3               ; "long"
                    jsr       OUTSTRG
                    jmp       MAIN

_2@@                cbnea     #'/',_4@@           ; jump if not "/"

                    tst       TMP1
                    bne       _3@@                ; jump if not enabled

                    decb
                    stb       COUNT
                    ldx       #MSLASH
                    bra       EXEC                ; execute "/"

_3@@                ldx       #MSG7               ; "command?"
                    jsr       OUTSTRG
                    jmp       MAIN

_4@@                jsr       HEXBIN
                    bra       Loop@@

;*******************************************************************************
; Search tables for command.  At this point,
; COMBUFF holds the command field to be executed,
; and B = # of characters in the command field.
; The command table holds the whole command name
; but only the first n characters of the command
; must match what is in COMBUFF where n is the
; number of characters entered by the user.
;*****************
; count = b;
; ptr1 = comtabl;
; while(ptr1[0] != end of table)
; ptr1 = next entry
; for(b=1; b=count; b++)
; if(ptr1[b] == combuff[b]) continue;
; else error(not found);
; execute task;
; return();
; return(command not found);

SRCH                proc
                    stb       COUNT               ; size of command entered
                    ldx       #COMTABL            ; pointer to table
                    stx       PTR1                ; pointer to next entry
Loop@@              ldx       PTR1
                    ldy       #COMBUFF            ; pointer to command buffer
                    ldb       ,x
                    cmpb      #$FF
                    bne       _2@@

                    ldx       #MSG2               ; "command not found"
                    jsr       OUTSTRG
                    jmp       MAIN

_2@@                pshx                          ; compute next table entry
_3@@                tst       ,x                  ; ..by searching for zero
                    beq       _4@@
                    inx
                    bra       _3@@

_4@@                inx:3                         ; skip zero, and command 16-bit address
                    stx       PTR1
                    pulx
                    clrb

Help@@              incb                          ; match characters loop
                    lda       ,x                  ; read table
                    cmpa      ,y                  ; compare to combuff
                    bne       Loop@@              ; try next entry
                    inx                           ; move pointers
                    iny
                    cmpb      COUNT
                    blt       Help@@              ; loop countu1 times

                    ldx       PTR1
                    dex:2
                    ldx       ,x                  ; jump address from table
;                   bra       EXEC

;*******************************************************************************

EXEC                proc
                    jsr       ,x                  ; call task as subroutine
                    jmp       MAIN

;*******************************************************************************
; UTILITY SUBROUTINES - These routines are called by any of the task routines.
;*******************************************************************************
; BPCLR() - Clear all entries in the table of breakpoints
;*******************************************************************************

BPCLR               proc
                    ldx       #BRKTABL
                    ldb       #8                  ; loop 8 times
Loop@@              clr       ,x
                    inx
                    decb
                    bne       Loop@@
                    rts

;*******************************************************************************
; UPCASE(A) - If the contents of A is alpha, returns A converted to uppercase.
;*******************************************************************************

UPCASE              proc
                    cmpa      #'a'
                    blo       Done@@              ; jump if < a

                    cmpa      #'z'
                    bhi       Done@@              ; jump if > z

                    adda      #'A'-'a'            ; convert
Done@@              rts

;*******************************************************************************
                    #DATA
;*******************************************************************************

;*******************************************************************************
; RPRNT1(x) - Prints name and contents of a single user register. On entry X
; points to name of register in reglist.  On exit, a=register name.
;*******************************************************************************

REGLIST             fcc       'PYXABCS'           ; names
                    fcb       0,2,4,6,7,8,9       ; offset
                    fcb       1,1,1,0,0,0,1       ; size

;*******************************************************************************
                    #ROM
;*******************************************************************************

RPRNT1              proc
                    lda       ,x
                    psha
                    pshx
                    jsr       OUTPUT              ; name
                    lda       #'-'
                    jsr       OUTPUT              ; dash
                    ldb       7,x                 ; contents offset
                    lda       14,x                ; bytesize
                    ldx       #REGISTERS          ; address
                    abx
                    tsta
                    beq       _1@@                ; jump if 1 byte
                    jsr       OUT1BYT             ; 2 bytes
_1@@                jsr       OUT1BSP
                    pulx
                    pula
                    rts

;*******************************************************************************
; RPRINT() - Print the name and contents of all the user registers
;*******************************************************************************

RPRINT              proc
                    pshx
                    ldx       #REGLIST
Loop@@              bsr       RPRNT1              ; print name
                    inx
                    cbnea     #'S',Loop@@         ; s is last register, jump if not done
                    pulx
                    rts

;*******************************************************************************
; HEXBIN(a) - Convert the ASCII character in a to binary and shift into shftreg.
; Returns value in tmp1 incremented if a is not hex
;*******************************************************************************

HEXBIN              proc
                    pshd
                    pshx

                    bsr       UPCASE              ; convert to upper case

                    cmpa      #'0'
                    blo       Fail@@              ; jump if a < $30

                    cmpa      #'9'
                    bls       Num@@               ; jump if 0-9

                    cmpa      #'A'
                    blo       Fail@@              ; jump if $39> a <$41

                    cmpa      #'F'
                    bhi       Fail@@              ; jump if a > $46

                    adda      #10-'A'+'0'         ; convert $A-$F

Num@@               suba      #'0'                ; convert to binary
                    ldx       #SHFTREG
                    ldb       #4
_1@@                asl       1,x                 ; 2 byte shift through
                    rol       ,x                  ; carry bit
                    decb
                    bgt       _1@@                ; shift 4 times
                    ora       1,x
                    sta       1,x
                    bra       Done@@

Fail@@              inc       TMP1                ; indicate not hex
Done@@              pulx
                    puld
                    rts

;*******************************************************************************
; BUFFARG() - Build a hex argument from the contents of the input buffer.
; Characters are converted to binary and shifted into shftreg until a non-hex
; character is found.  On exit shftreg holds the last four digits read, count
; holds the number of digits read, ptrbuff points to the first non-hex character
; read, and A holds that first non-hex character.
;*****************
; Initialize
; while((a=readbuff()) not hex)
; hexbin(a);
; return();

BUFFARG             proc
                    clr       TMP1                ; not hex indicator
                    clr       COUNT               ; # or digits
                    clr       SHFTREG
                    clr       SHFTREG+1
                    jsr       WSKIP
Loop@@              !jsr      READBUFF            ; read char
                    bsr       HEXBIN
                    tst       TMP1
                    bne       Done@@              ; jump if not hex
                    inc       COUNT
                    !jsr      INCBUFF             ; move buffer pointer
                    bra       Loop@@
Done@@              equ       :AnRTS

;*******************************************************************************
; TERMARG() - Build a hex argument from the
; terminal.  Characters are converted to binary
; and shifted into shftreg until a non-hex character
; is found.  On exit shftreg holds the last four
; digits read, count holds the number of digits
; read, and A holds the first non-hex character.
;*****************
; initialize
; while((a=inchar()) == hex)
; if(a = cntlx or del)
; abort;
; else
; hexbin(a); countu1++;
; return();

TERMARG             proc
                    clr       COUNT
                    clr       SHFTREG
                    clr       SHFTREG+1
Loop@@              jsr       INCHAR
                    cmpa      #CTL_X
                    beq       _1@@                ; jump if controlx
                    cmpa      #DEL
                    bne       _2@@                ; jump if not delete
_1@@                jmp       MAIN                ; abort

_2@@                clr       TMP1                ; hex indicator
                    bsr       HEXBIN
                    tst       TMP1
                    bne       Done@@              ; jump if not hex
                    inc       COUNT
                    bra       Loop@@
Done@@              equ       :AnRTS

;*******************************************************************************
; CHGBYT() - If shftreg is not empty, put
; contents of shftreg at address in X.  If X
; is an address in EEPROM then program it.
;*****************
; if(count != 0)
; (x) = a;

CHGBYT              proc
                    tst       COUNT
                    beq       Done@@              ; quit if shftreg empty
                    lda       SHFTREG+1           ; get data into A
;                   bra       WRITE
Done@@              equ       :AnRTS

;*******************************************************************************
; WRITE() - This routine is used to write the
; contents of A to the address of X.  If the
; address is in EEPROM, it will be programmed
; and if it is already programmed, it will be
; byte erased first.
;******************
; if(X == config) then
; byte erase config;
; if(X is eeprom)then
; if(not erased) then erase;
; program (x) = A;
; write (x) = A;
; if((x) != A) error(rom);

WRITE               proc
                    cmpx      #CONFIG
                    beq       _2@@                ; jump if config

                    cmpx      STREE               ; start of EE
                    blo       _3@@                ; jump if not EE

                    cmpx      ENDEE               ; end of EE
                    bhi       _3@@                ; jump if not EE

                    pshb                          ; check if byte erased
                    ldb       ,x
                    cmpb      #$FF
                    pulb

          #ifndef __WSI__
                    beq       _1@@                ; jump if erased
          #endif
_2@@
          #ifndef __WSI__
                    bsr       EEBYTE              ; byte erase
_1@@                bsr       EEWRIT              ; byte program
          #endif
_3@@                sta       ,x                  ; write for non EE
                    cmpa      ,x
                    beq       Done@@              ; jump if write ok
                    pshx
                    ldx       #MSG6               ; "rom"
                    jsr       OUTSTRG
                    pulx
Done@@              rts

          #ifndef __WSI__

;*******************************************************************************
; EEWRIT(), EEBYTE(), EEBULK() -
; These routines are used to program and eeprom
; locations.  eewrite programs the address in X with
; the value in A, eebyte does a byte address at X,
; and eebulk does a bulk of eeprom.  Whether eebulk
; erases the config or not depends on the address it
; receives in X.
;*******************************************************************************

EEWRIT              proc
                    pshb                          ; program one byte at x
                    ldb       #2
                    bra       EEPROG

EEBYTE              proc
                    pshb                          ; byte erase address x
                    ldb       #$16
                    bra       EEPROG

EEBULK              proc
                    pshb                          ; bulk erase eeprom
                    ldb       #6
;                   bra       EEPROG

EEPROG              stb       PPROG
                    sta       ,x                  ; erase config or not ...
                    inc       PPROG               ; ... depends on X addr
                    pulb
          #endif

;*******************************************************************************
                              #Cycles
DLY10MS             proc
                    pshx                          ; delay 10ms
                    ldx       #DELAY@@            ; auto-adjusts w/ bus
                              #Cycles             ; reset cycle counter
Loop@@              dex
                    bne       Loop@@
                              #temp :cycles       ; grab cycles for loop section
                    pulx
          #ifndef __WSI__
                    clr       PPROG
          #endif
                    rts

DELAY@@             equ       10*BUS_KHZ-:cycles-:ocycles/:temp

;*******************************************************************************
; READBUFF() -  Read the character in INBUFF
; pointed at by ptrbuff into A.  Returns ptrbuff
; unchanged.
;*******************************************************************************

READBUFF            proc
                    pshx
                    ldx       PTR0
                    lda       ,x
                    pulx
                    rts

;*******************************************************************************
; INCBUFF(), DECBUFF() - Increment or decrement
; ptrbuff.
;*******************************************************************************

INCBUFF.TWICE       bsr       INCBUFF

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

;*******************************************************************************
; WSKIP() - Read from the INBUFF until a
; non whitespace (space, comma, tab) character
; is found.  Returns ptrbuff pointing to the
; first non-whitespace character and a holds
; that character.  WSKIP also compares a to
; (CR) and cond codes indicating the
; results of that compare.
;*******************************************************************************

WSKIP               proc
                    bsr       READBUFF            ; read character
                    bsr       WCHEK
                    bne       CR@@                ; jump if not wspc
                    bsr       INCBUFF             ; move pointer
                    bra       WSKIP               ; loop

CR@@                cmpa      #CR
                    rts

;*******************************************************************************
; WCHEK(a) - Returns z=1 if A holds a whitespace character, else z=0.
;*******************************************************************************

WCHEK               proc
                    cbeqa,    #',' Done@@         ; comma
                    cbeqa     #' ',Done@@         ; space
                    cmpa      #TAB
Done@@              rts

;*******************************************************************************
; DCHEK(a) - Returns Z=1 if A = whitespace or carriage return. Else returns z=0
;*******************************************************************************

DCHEK               proc
                    bsr       WCHEK
                    beq       Done@@              ; jump if whitespace
                    cmpa      #CR
Done@@              rts

;*******************************************************************************
; CHKABRT() - Checks for a control x or delete
; from the terminal.  If found, the stack is
; reset and the control is transferred to main.
; Note that this is an abnormal termination.
; If the input from the terminal is a control W
; then this routine keeps waiting until any other
; character is read.
;*****************
; a=input();
; if(a=cntl w) wait until any other key;
; if(a = cntl x or del) abort;

CHKABRT             proc
                    bsr       INPUT
                    beq       Done@@              ; jump if no input

                    cbnea     #CTL_W,_2@@         ; jump in not cntlw

Loop@@              bsr       INPUT
                    beq       Loop@@              ; jump if no input

_2@@                cbeqa     #DEL,_3@@           ; jump if delete
                    cbeqa     #CTL_X,_3@@         ; jump if control x
                    cbnea     #CTL_A,Done@@       ; jump not control a

_3@@                jmp       MAIN                ; abort
Done@@              equ       :AnRTS

;*******************************************************************************
; VECINIT - This routine checks for
; vectors in the RAM table.  All
; uninitialized vectors are programmed
; to JMP STOPIT
;*******************************************************************************

VECINIT             proc
                    ldx       #JSCI               ; Point to First RAM Vector
                    ldy       #STOPIT             ; Pointer to STOPIT routine
                    ldd       #JMP_OPCODE<8|3     ; A=JMP opcode; B=offset
Loop@@              cbeqa,    ,x Skip@@           ; If vector already in
                    sta       ,x                  ; install JMP
                    sty       1,x                 ; to STOPIT routine
Skip@@              abx                           ; Add 3 to point at next vector
                    cmpx      #JCLM+3             ; Done?
                    bne       Loop@@              ; If not, continue loop
                    rts                           ; return

;*******************************************************************************

STOPIT              proc
                    sei
                    cls
Loop@@              stop                          ; You are lost! Shut down until Reset
                    bra       Loop@@              ; In case of XIRQ go back to STOP

;*******************************************************************************
; INPUT() - Read device. Returns a=char or 0.
; This routine also disarms the COP.
;*******************************************************************************

INPUT               proc
                    @cop
;                   bra       INSCI

;*******************************************************************************
; INSCI() - Read from SCI.  Return A=char or 0.
;*******************************************************************************

INSCI               proc
                    pshx
                    clra
                    ldx       #REGS
                    brclr     [SCSR,x,#$20,Done@@
                    lda       SCDR                ; read data
                    anda      #$7F                ; mask parity
Done@@              pulx
                    rts

;*******************************************************************************
; OUTPUT() - Output character in A
; chrcnt indicates the current column on the
; output display.  It is incremented every time
; a character is outputted, and cleared whenever
; the subroutine outcrlf is called.
;*******************************************************************************

OUTPUT              proc
                    cbnea     #LF,PutChar@@
                    lda       #CR
                    bsr       PutChar@@
                    lda       #LF
PutChar@@           psha                          ; save registers
                    bsr       OUTSCI              ; write sci
                    pula
                    inc       CHRCNT              ; increment column count
                    rts

;*******************************************************************************
; ONSCI() - Initialize the SCI for 9600 bps
;*******************************************************************************

ONSCI               proc
                    psha
                    lda       #BAUDRATE
                    sta       BAUD                ; baud register
                    clr       SCCR1
                    lda       #%1100              ; enable TX/RX in polled mode
                    sta       SCCR2
                    pula
                    rts

;*******************************************************************************
; OUTSCI() - Output A to sci.
;*******************************************************************************

OUTSCI              proc
Loop@@              tst       SCSR                ; read status
                    bpl       Loop@@              ; loop until tdre=1
                    anda      #$7F                ; mask parity
                    sta       SCDR                ; send character
                    rts

;*******************************************************************************
; I/O UTILITY SUBROUTINES
; These subroutines perform the neccesary data I/O operations

; OUTLHLF-Convert left 4 bits of A from binary to ASCII and output.
; OUTRHLF-Convert right 4 bits of A from binary to ASCII and output.
; OUT1BYT-Convert byte at X from binary to ASCII and output.
; OUT1BSP-Convert byte at X from bin to ASCII and output followed by a space.
; OUT2BSP-Convert 2 bytes at X from bin to ASCII and output followed by a space.
; OUTSPAC-Output a space.

; OUTCRLF-Output a line feed and carriage return.

; OUTSTRG-Output the string of ASCII bytes addressed by X until $00.
; OUTA-Output the ASCII character in A.

; TABTO-Output spaces until column 20 is reached.

; INCHAR-Input to A and echo one character.  Loops until character read.

;*******************************************************************************
; OUTRHLF(), OUTLHLF(), OUTA()
; Convert A from binary to ASCII and output.
; Contents of A are destroyed..
;*******************************************************************************

OUTLHLF             proc
                    lsra:4                        ; shift data to right
;                   bra       OUTRHLF

;*******************************************************************************

OUTRHLF             proc
                    anda      #$0F                ; mask top half
                    adda      #$90
                    daa
                    adca      #$40
                    daa
OUTA                bra       OUTPUT              ; output character

;*******************************************************************************
; OUT1BYT(x) - Convert the byte at X to two
; ASCII characters and output. Return X pointing
; to next byte.
;*******************************************************************************

OUT1BYT             proc
                    psha
                    lda       ,x                  ; get data in A
                    psha                          ; save copy
                    bsr       OUTLHLF             ; output left half
                    pula                          ; retrieve copy
                    bsr       OUTRHLF             ; output right half
                    pula
                    inx
                    rts

;*******************************************************************************
; OUT1BSP(x), OUT2BSP(x) - Output 1 or 2 bytes at X
; followed by a space.  Returns x pointing to next byte.
;*******************************************************************************

OUT2BSP             proc
                    bsr       OUT1BYT             ; do first byte
;                   bra       OUT1BSP

;*******************************************************************************

OUT1BSP             proc
                    bsr       OUT1BYT             ; do next byte
;                   bra       OUTSPAC

;*******************************************************************************

OUTSPAC             proc
                    lda       #' '                ; output a space
                    bra       OUTPUT

;*******************************************************************************
; OUTCRLF() - Output a Carriage return and
; a line feed.  Returns A = CR.
;*******************************************************************************

PutChar             macro     char
                    mreq      1:Character         ; to print
                    lda       #~1~
                    bsr       OUTPUT
                    endm

;*******************************************************************************

OUTCRLF             proc
                    @putchar  CR                  ; output A
                    @putchar  LF                  ; output padding
                    lda       #CR
                    clr       CHRCNT              ; zero the column counter
                    rts

;*******************************************************************************
; OUTSTRG(x) - Output string of ASCII bytes
; starting at X until end of text ($04).  Can
; be paused by control w (any char restarts).
;*******************************************************************************

OUTSTRG             proc
                    bsr       OUTCRLF
;                   bra       OUTSTRG0

;*******************************************************************************

OUTSTRG0            proc
                    psha
Loop@@              lda       ,x                  ; read char into a
                    beq       ?PulaRts            ; jump if eot
                    bsr       OUTPUT              ; output character
                    inx
                    jsr       INPUT
                    beq       Loop@@              ; jump if no input
                    cbnea     #CTL_W,Loop@@       ; jump if not cntlw
Read@@              jsr       INPUT
                    beq       Read@@              ; jump if any input
                    bra       Loop@@

;*******************************************************************************
; TABTO() - move cursor over to column 20.
; while(chrcnt < 20) outspac.

TABTO               proc
                    psha
Loop@@              bsr       OUTSPAC
                    lda       CHRCNT
                    cmpa      #20
                    ble       Loop@@
?PulaRts            pula
                    rts

;*******************************************************************************
; INCHAR() - Reads input until character sent.
; Echoes char and returns with A = char.

INCHAR              proc
Loop@@              jsr       INPUT
                    tsta
                    beq       Loop@@              ; jump if no input
                    jmp       OUTPUT              ; echo

;*******************************************************************************
                    #DATA
;*******************************************************************************

;*******************************************************************************
; COMMAND TABLE
; Format is ASCIIZ Command, Command Address (allows easier modification)

Command             macro     name[,address]      ; if address is missing, same as name
                    mreq      1:name[,address]
                    mdef      2,~1~
                    mstr      1
                    fcs       ~1~
                    dw        ~2~
                    endm

COMTABL             @command  ASSEM
                    @command  BREAK
          #ifndef __WSI__
                    @command  BULK
                    @command  BULKALL
          #endif
                    @command  CALL
                    @command  CLS
                    @command  DUMP
                    @command  EEMOD
                    @command  FILL
                    @command  GO
                    @command  HELP
                    @command  LOAD
                    @command  MEMORY
                    @command  MOVE
                    @command  OFFSET
                    @command  PROCEED
                    @command  REGISTER
                    @command  STOPAT
                    @command  TRACE
                    @command  VERIFY

                    @command  ?,HELP              ; initial command
                    @command  XBOOT,BOOT
                    @command  ~,TILDE             ; dummy command for load

; Command names for EVM compatibility

                    @command  ASM,ASSEM
                    @command  BF,FILL
                    @command  COPY,MOVE
          #ifndef __WSI__
                    @command  ERASE,BULK
          #endif
                    @command  MD,DUMP
                    @command  MM,MEMORY
                    @command  RD,REGISTER
                    @command  RM,REGISTER
                    @command  READ,MOVE

                    fcb       $FF                 ; End-of-table marker

;*******************************************************************************
; TEXT TABLES

Msg                 macro     String
                    mset      1,~@~
                    mreq      1:String
          #if :mindex = 1
MSG{:mindex}        fcc       ASCII_FF,CR,'TBUF34 v{VERSION(2)} (Buffalo-based Monitor) by Tony Papadimitriou <tonyp@acm.org>',LF
                    fcc       '(Original BUFFALO by Tony Fourcroy)',LF
                    fcs       '[Target: ',~1~,' - {KHZ(3)}MHz xtal/{BUS_KHZ(3)}MHz bus]',LF
                    mexit
          #endif
MSG{:mindex}        fcs       ~1~,LF
                    endm

; -------------------------------------------------------------------------------

          #ifdef __ASPISYS__
                    @msg      'ASPiSYS F1 Board'
                    @msg      'MC68HC11F1'
          #else ifdef __WSI__
                    @msg      'WSI DK68HC11 Development Kit'
                    @msg      'MC68HC11E0'
          #endif
                    @msg      'What?'
                    @msg      'Too Long'
                    @msg      'Full'
                    @msg      'Op- '
                    @msg      'rom-'
                    @msg      'Command?'
                    @msg      'Bad argument'
                    @msg      'done'
                    @msg      'checksum error'
                    @msg      'error addr '
                    @msg      'receiver error'

;*******************************************************************************
; break [-][<addr>] . . .
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
; a = wskip();
; switch(a)
; case(cr):
; bprint(); return;

;*******************************************************************************
                    #ROM
;*******************************************************************************

BREAK               proc
                    jsr       WSKIP
                    jeq       BPRINT              ; jump if CR print table

; case("-"):
; incbuff(); readbuff();
; if(dchek(a))          /* look for wspac or cr */
; bpclr();
; breaksw;
; a = buffarg();
; if( !dchek(a) ) return(bad argument);
; b = bpsrch();
; if(b >= 0)
; brktabl[b] = 0;
; breaksw;

                    cbnea     #'-',BRKDEF         ; jump if not -
                    jsr       INCBUFF
                    jsr       READBUFF
                    jsr       DCHEK
                    bne       _1@@                ; jump if not delimeter
                    jsr       BPCLR               ; clear table
                    bra       BREAK               ; do next argument

_1@@                jsr       BUFFARG             ; get address to delete
                    jsr       DCHEK
                    beq       _2@@                ; jump if delimeter
                    ldx       #MSG8               ; "bad argument"
                    bra       OUTSTRG

_2@@                bsr       BPSRCH              ; look for addr in table
                    tstb
                    bmi       _3@@                ; jump if not found
                    ldx       #BRKTABL
                    abx
                    clr       ,x                  ; clear entry
                    clr       1,x
_3@@                bra       BREAK               ; do next argument

;*******************************************************************************
; default:
; a = buffarg();
; if( !dchek(a) ) return(bad argument);
; b = bpsrch();
; if(b < 0)            /* not already in table */
; x = shftreg;
; shftreg = 0;
; a = x[0]; x[0] = $3F
; b = x[0]; x[0] = a;
; if(b != $3F) return(rom);
; b = bpsrch();   /* look for hole */
; if(b >= 0) return(table full);
; brktabl[b] = x;
; breaksw;

BRKDEF              proc
                    jsr       BUFFARG             ; get argument
                    jsr       DCHEK
                    beq       _1@@                ; jump if delimiter
                    ldx       #MSG8               ; "bad argument"
                    bra       OUTSTRG

_1@@                bsr       BPSRCH              ; look for entry in table
                    tstb
                    bge       BREAK               ; jump if already in table

                    ldx       SHFTREG             ; x = new entry addr
                    lda       ,x                  ; save original contents
                    psha
                    lda       #SWI_OPCODE
                    jsr       WRITE               ; write to entry addr
                    ldb       ,x                  ; read back
                    pula
                    jsr       WRITE               ; restore original
                    cmpb      #SWI_OPCODE
                    beq       _2@@                ; jump if writes ok
                    stx       PTR1                ; save address
                    ldx       #PTR1
                    jsr       OUT2BSP             ; print address
                    bra       BPRINT

_2@@                clr       SHFTREG
                    clr       SHFTREG+1
                    pshx
                    bsr       BPSRCH              ; look for 0 entry
                    pulx
                    tstb
                    bpl       _3@@                ; jump if table not full
                    ldx       #MSG4               ; "full"
                    jsr       OUTSTRG
                    bra       BPRINT

_3@@                ldy       #BRKTABL
                    aby
                    stx       ,y                  ; put new entry in
                    jmp       BREAK               ; do next argument

;*******************************************************************************
; bprint() - print the contents of the table.
;*******************************************************************************

BPRINT              proc
                    jsr       OUTCRLF
                    ldx       #BRKTABL
                    ldb       #4
Loop@@              jsr       OUT2BSP
                    decb
                    bne       Loop@@              ; loop 4 times
                    rts

;*******************************************************************************
; bpsrch() - search table for address in
; shftreg. Returns b = index to entry or
; b = -1 if not found.
;**********
; for(b=0; b=6; b=+2)
; x[] = brktabl + b;
; if(x[0] = shftreg)
; return(b);
; return(-1);

BPSRCH              proc
                    clrb
Loop@@              ldx       #BRKTABL
                    abx
                    ldx       ,x                  ; get table entry
                    cmpx      SHFTREG
                    beq       Done@@              ; exit if match
                    incb:2
                    cmpb      #6
                    ble       Loop@@              ; loop 4 times
                    ldb       #-1
Done@@              rts

          #ifndef __WSI__
;*******************************************************************************
; bulk  - Bulk erase the eeprom not config.
; bulkall - Bulk erase eeprom and config.
;*******************************************************************************

BULK                proc
                    ldx       STREE
                    bra       ?BULK

;*******************************************************************************

BULKALL             proc
                    ldx       #CONFIG
?BULK               lda       #-1
                    jmp       EEBULK
          #endif
DUMPERR             ldx       #MSG8               ; "bad argument"
                    jmp       OUTSTRG

;*******************************************************************************
; dump [<addr1> [<addr2>]]  - Dump memory
; in 16 byte lines from <addr1> to <addr2>.
; Default starting address is "current
; location" and default number of lines is 8.
;**********
; ptr1 = ptrmem;        /* default start address */
; ptr2 = ptr1 + $80;    /* default end address */
; a = wskip();
; if(a != cr)
; a = buffarg();
; if(countu1 = 0) return(bad argument);
; if( !dchek(a) ) return(bad argument);
; ptr1 = shftreg;
; ptr2 = ptr1 + $80;  /* default end address */
; a = wskip();
; if(a != cr)
; a = buffarg();
; if(countu1 = 0) return(bad argument);
; a = wskip();
; if(a != cr) return(bad argument);
; ptr2 = shftreg;

DUMP                proc
                    ldx       PTRMEM              ; current location
                    stx       PTR1                ; default start
                    ldb       #$80
                    abx
                    stx       PTR2                ; default end
                    jsr       WSKIP
                    beq       _1@@                ; jump - no arguments
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
                    beq       _1@@                ; jump - 1 argument
                    jsr       BUFFARG             ; read argument
                    tst       COUNT
                    beq       DUMPERR             ; jump if no argument
                    jsr       WSKIP
                    bne       DUMPERR             ; jump if not cr
                    ldx       SHFTREG
                    stx       PTR2

; ptrmem = ptr1;
; ptr1 = ptr1 & $fff0;

_1@@                ldd       PTR1
                    std       PTRMEM              ; new current location
                    andb      #$F0
                    std       PTR1                ; start dump at 16 byte boundary

;*** dump loop starts here ***
; *do:
; output address of first byte;

DumpLoop@@          jsr       OUTCRLF
                    ldx       #PTR1
                    jsr       OUT2BSP             ; first address

; x = ptr1;
; for(b=0; b=16; b++)
; output contents;

                    ldx       PTR1                ; base address
                    clrb                          ; loop counter
DumpDat@@           jsr       OUT1BSP             ; hex value loop
                    incb
                    cmpb      #16
                    blo       DumpDat@@           ; loop 16 times

; x = ptr1;
; for(b=0; b=16; b++)
; a = x[b];
; if($7A < a < $20)  a = $20;
; output ascii contents;

                    clrb                          ; loop counter
Loop@@              ldx       PTR1                ; base address
                    abx
                    lda       ,x                  ; ascii value loop
                    cmpa      #' '
                    blo       _2@@                ; jump if non printable
                    cmpa      #'z'
                    bls       _3@@                ; jump if printable
_2@@                lda       #' '                ; space for non printables
_3@@                jsr       OUTPUT              ; output ascii value
                    incb
                    cmpb      #16
                    blo       Loop@@              ; loop 16 times

; chkabrt();
; ptr1 = ptr1 + $10;
; while(ptr1 <= ptr2);
; return;

                    jsr       CHKABRT             ; check abort or wait
                    ldd       PTR1
                    addd      #16                 ; point to next 16 byte bound
                    std       PTR1                ; update ptr1
                    cmpd      PTR2
                    bhi       Done@@              ; quit if ptr1 > ptr2
                    cmpd      #0                  ; check wraparound at $ffff
                    bne       DumpLoop@@          ; jump - no wraparound
                    ldd       PTR2
                    cmpd      #$FFF0
                    blo       DumpLoop@@          ; upper bound not at top
Done@@              rts                           ; quit

;*******************************************************************************
; eemod [<addr1> [<addr2>]]
; Modifies the eeprom address range.
; EEMOD                 -show ee address range
; EEMOD <addr1>         -set range to addr1 -> addr1+2k
; EEMOD <addr1> <addr2> -set range to addr1 -> addr2
;**********
; if(<addr1>)
; stree = addr1;
; endee = addr1 + 2k bytes;
; if(<addr2>)
; endee = addr2;
; print(stree,endee);

EEMOD               proc
                    jsr       WSKIP
                    beq       _2@@                ; jump - no arguments
                    jsr       BUFFARG             ; read argument
                    tst       COUNT
                    beq       Fail@@              ; jump if no argument
                    jsr       DCHEK
                    bne       Fail@@              ; jump if no delimeter
                    ldd       SHFTREG
                    std       PTR1
                    addd      #2048-1             ; add 2k bytes to stree
                    std       PTR2                ; default endee address
                    jsr       WSKIP
                    beq       _1@@                ; jump - 1 argument
                    jsr       BUFFARG             ; read argument
                    tst       COUNT
                    beq       Fail@@              ; jump if no argument
                    jsr       WSKIP
                    bne       Fail@@              ; jump if not CR
                    ldx       SHFTREG
                    stx       PTR2
_1@@                ldx       PTR1
                    stx       STREE               ; new stree address
                    ldx       PTR2
                    stx       ENDEE               ; new endee address
_2@@                jsr       OUTCRLF             ; display ee range
                    ldx       #STREE
                    jsr       OUT2BSP
                    ldx       #ENDEE
                    jmp       OUT2BSP

Fail@@              ldx       #MSG8               ; "bad argument"
                    jmp       OUTSTRG

;*******************************************************************************
; fill <addr1> <addr2> [<data>]  - Block fill
; memory from addr1 to addr2 with data.  Data
; defaults to $FF.
;*******************************************************************************

; get addr1 and addr2

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

; Get data if it exists

                    lda       #$FF
                    sta       TMP2                ; default data
                    jsr       WSKIP
                    beq       Loop@@              ; jump if default data
                    jsr       BUFFARG
                    tst       COUNT
                    beq       Fail@@              ; jump if no argument
                    jsr       WSKIP
                    bne       Fail@@              ; jump if bad argument
                    lda       SHFTREG+1
                    sta       TMP2

; while(ptr1 <= ptr2)
; *ptr1 = data
; if(*ptr1 != data) abort

Loop@@              jsr       CHKABRT             ; check for abort
                    ldx       PTR1                ; starting address
                    lda       TMP2                ; data
                    jsr       WRITE               ; write the data to x
                    cmpa      ,x
                    bne       BadAddr@@           ; jump if no write
                    cmpx      PTR2
                    beq       Done@@              ; quit yet?
                    inx
                    stx       PTR1
                    bra       Loop@@              ; loop

Done@@              rts

Fail@@              ldx       #MSG8               ; "bad argument"
                    jmp       OUTSTRG

BadAddr@@           ldx       #PTR1               ; output bad address
                    jmp       OUT2BSP

;*******************************************************************************
; MEMORY [<addr>]
; [<addr>]/
; Opens memory and allows user to modify the
; contents at <addr> or the last opened location.
; Subcommands:
; [<data>]<cr>       - Close current location and exit.
; [<data>]<lf><+>    - Close current and open next.
; [<data>]<^><-><bs> - Close current and open previous.
; [<data>]<sp>       - Close current and open next.
; [<data>]</><=>     - Reopen current location.
; The contents of the current location is only
; changed if valid data is entered before each
; subcommand.
; [<addr>]O - Compute relative offset from current
; location to <addr>.  The current location must
; be the address of the offset byte.
;**********
; a = wskip();
; if(a != cr)
; a = buffarg();
; if(a != cr) return(bad argument);
; if(countu1 != 0) ptrmem[] = shftreg;

MEMORY              proc
                    ldx       #Msg@@
                    jsr       OUTSTRG
                    jsr       WSKIP
                    beq       MEM1                ; jump if CR
                    jsr       BUFFARG
                    jsr       WSKIP
                    beq       MSLASH              ; jump if CR
                    ldx       #MSG8               ; "bad argument"
                    jmp       OUTSTRG
;-------------------------------------------------------------------------------
                    #push
                    #DATA
Msg@@               fcs       '[/,=] Same  [^,-,^H] Previous  [+,^J,SPACE] Next  <addr>Offset  [CR] Quit',LF
                    #pull

;*******************************************************************************

MSLASH              proc
                    tst       COUNT
                    beq       MEM1                ; jump if no argument
                    ldx       SHFTREG
                    stx       PTRMEM              ; update "current location"
;                   bra       MEM1

;*******************************************************************************
; Subcommands
;*******************************************************************************
; outcrlf();
; out2bsp(ptrmem[]);
; out1bsp(ptrmem[0]);

MEM1                proc
                    jsr       OUTCRLF
;                   bra       MEM2

;*******************************************************************************

MEM2                proc
                    ldx       #PTRMEM
                    jsr       OUT2BSP             ; output address
;                   bra       MEM3

;*******************************************************************************

MEM3                proc
                    ldx       PTRMEM
                    jsr       OUT1BSP             ; output contents
                    clr       SHFTREG
                    clr       SHFTREG+1
;                   bra       MEM4

;*******************************************************************************
; while 1
; a = termarg();
; switch(a)
; case(space):
; chgbyt();
; ptrmem[]++;
; if(ptrmem%16 == 0) start new line;
; case(linefeed | +):
; chgbyt();
; ptrmem[]++;
; case(up arrow | backspace | -):
; chgbyt();
; ptrmem[]--;
; case('/' | '='):
; chgbyt();
; outcrlf();
; case(O):
; d = ptrmem[0] - (shftreg);
; if($80 < d < $ff81)
; print(out of range);
; countt1 = d-1;
; out1bsp(countt1);
; case(carriage return):
; chgbyt();
; return;
; default: return(command?)

MEM4                proc
                    jsr       TERMARG
                    jsr       UPCASE
                    ldx       PTRMEM

                    cbeqa     #' ',MEMSP          ; jump if space
                    cbeqa     #LF,MEMLF           ; jump if linefeed
                    cbeqa     #'+',MEMPLUS        ; jump if +
                    cbeqa     #'^',MEMUA          ; jump if up arrow (caret)
                    cbeqa     #'-',MEMUA          ; jump if -
                    cbeqa     #8,MEMUA            ; jump if backspace
                    cbeqa     #'/',MEMSL          ; jump if /
                    cbeqa     #'=',MEMSL          ; jump if =
                    cbeqa     #'O',MEMOFF         ; jump if O
                    cbeqa     #CR,MEMCR           ; jump if carriage ret
                    cbeqa     #'.',Done@@         ; jump if .

                    ldx       #MSG7               ; "command?"
                    jsr       OUTSTRG
                    bra       MEM1
Done@@              equ       :AnRTS

;*******************************************************************************

MEMSP               proc
                    jsr       CHGBYT
                    inx
                    stx       PTRMEM
                    xgdx
                    andb      #$0F
                    bne       MEM3                ; continue same line
                    bra       MEM1                ; ... else start new line

;*******************************************************************************

MEMLF               proc
                    jsr       CHGBYT
                    inx
                    stx       PTRMEM
                    bra       MEM2                ; output next address

;*******************************************************************************

MEMPLUS             proc
                    jsr       CHGBYT
                    inx
                    stx       PTRMEM
                    bra       MEM1                ; output CR, next address

;*******************************************************************************

MEMUA               proc
                    jsr       CHGBYT
                    dex
                    stx       PTRMEM
                    bra       MEM1                ; output CR, previous address

;*******************************************************************************

MEMSL               proc
                    jsr       CHGBYT
                    bra       MEM1                ; output CR, same address

;*******************************************************************************

MEMOFF              proc
                    ldd       SHFTREG             ; destination addr
                    subd      PTRMEM
                    tsta
                    bne       _1@@                ; jump if not 0
                    cmpb      #$80
                    bls       _3@@                ; jump if in range
                    bra       _2@@                ; out of range

_1@@                cbnea     #$FF,_2@@           ; out of range
                    cmpb      #$81
                    bhs       _3@@                ; in range

_2@@                ldx       #MSG3               ; "Too long"
                    jsr       OUTSTRG
                    jmp       MEM1                ; output CR, addr, contents

_3@@                decd                          ; B now has offset
                    stb       TMP4
                    jsr       OUTSPAC
                    ldx       #TMP4
                    jsr       OUT1BSP             ; output offset
                    jmp       MEM1                ; output CR, addr, contents

;*******************************************************************************

MEMCR               proc
                    jmp       CHGBYT

;*******************************************************************************
; move <src1> <src2> [<dest>]  - move
; block at <src1> to <src2> to <dest>.
; Moves block 1 byte up if no <dest>.
;**********
; a = buffarg();
; if(countu1 = 0) return(bad argument);
; if( !wchek(a) ) return(bad argument);
; ptr1 = shftreg;         /* src1 */

MOVE                proc
                    jsr       BUFFARG
                    tst       COUNT
                    beq       Fail@@              ; jump if no arg
                    jsr       WCHEK
                    bne       Fail@@              ; jump if no delim
                    ldx       SHFTREG             ; src1
                    stx       PTR1

; a = buffarg();
; if(countu1 = 0) return(bad argument);
; if( !dchek(a) ) return(bad argument);
; ptr2 = shftreg;         /* src2 */

                    jsr       BUFFARG
                    tst       COUNT
                    beq       Fail@@              ; jump if no arg
                    jsr       DCHEK
                    bne       Fail@@              ; jump if no delim
                    ldx       SHFTREG             ; src2
                    stx       PTR2

; a = buffarg();
; a = wskip();
; if(a != cr) return(bad argument);
; if(countu1 != 0) tmp2 = shftreg;  /* dest */
; else tmp2 = ptr1 + 1;

                    jsr       BUFFARG
                    jsr       WSKIP
                    bne       Fail@@              ; jump if not cr
                    tst       COUNT
                    beq       _1@@                ; jump if no arg
                    ldx       SHFTREG             ; dest
                    bra       _2@@

Fail@@              ldx       #MSG8               ; "bad argument"
                    jmp       OUTSTRG

_1@@                ldx       PTR1
                    inx                           ; default dest
_2@@                stx       PTR3

; if(src1 < dest <= src2)
; dest = dest+(src2-src1);
; for(x = src2; x = src1; x--)
; dest[0]-- = x[0]--;

                    ldx       PTR3                ; dest
                    cmpx      PTR1                ; src1
                    bls       MOVE3               ; jump if dest =< src1
                    cmpx      PTR2                ; src2
                    bhi       MOVE3               ; jump if dest > src2
                    ldd       PTR2
                    subd      PTR1
                    addd      PTR3
                    std       PTR3                ; dest = dest+(src2-src1)
                    ldx       PTR2
Loop@@              jsr       CHKABRT             ; check for abort
                    lda       ,x                  ; char at src2
                    pshx
                    ldx       PTR3
                    jsr       WRITE               ; write a to x
                    cmpa      ,x
                    bne       MOVEBAD             ; jump if no write
                    dex
                    stx       PTR3
                    pulx
                    cmpx      PTR1
                    beq       Done@@
                    dex
                    bra       Loop@@              ; Loop SRC2 - SRC1 times
Done@@              rts

;*******************************************************************************
; else
; for(x=src1; x=src2; x++)
; dest[0]++ = x[0]++;

MOVE3               proc
                    ldx       PTR1                ; srce1
Loop@@              jsr       CHKABRT             ; check for abort
                    lda       ,x
                    pshx
                    ldx       PTR3                ; dest
                    jsr       WRITE               ; write A to X
                    cmpa      ,x
                    bne       MOVEBAD             ; jump if no write
                    inx
                    stx       PTR3
                    pulx
                    cmpx      PTR2
                    beq       Done@@
                    inx
                    bra       Loop@@              ; Loop SRC2-SRC1 times
Done@@              equ       :AnRTS

;*******************************************************************************

MOVEBAD             proc
                    pulx                          ; restore stack
                    ldx       #PTR3
                    jmp       OUT2BSP             ; output bad address

;*******************************************************************************
; assem(addr) -68HC11 line assembler/disassembler.
; This routine will disassemble the opcode at
; <addr> and then allow the user to enter a line for
; assembly. Rules for assembly are as follows:
; -A '#' sign indicates immediate addressing.
; -A ',' (comma) indicates indexed addressing
; and the next character must be X or Y.
; -All arguments are assumed to be hex and the
; '$' sign shouldn't be used.
; -Arguments should be separated by 1 or more
; spaces or tabs.
; -Any input after the required number of
; arguments is ignored.
; -Upper or lower case makes no difference.
;
; To signify end of input line, the following
; commands are available and have the indicated action:
; <cr>      - Finds the next opcode for
; assembly.  If there was no assembly input,
; the next opcode disassembled is retrieved
; from the disassembler.
; <lf><+>   - Works the same as carriage return
; except if there was no assembly input, the
; <addr> is incremented and the next <addr> is
; disassembled.
; <^><->   - Decrements <addr> and the previous
; address is then disassembled.
; </><=>   - Redisassembles the current address.
;
; To exit the assembler use CONTROL A or . (period).
; Of course control X and DEL will also allow you to abort.

; Equates for assembler

PAGE1               equ       0                   ; values for page opcodes
PAGE2               equ       $18
PAGE3               equ       $1A
PAGE4               equ       $CD

IMMED               equ       0                   ; addressing modes
INDX                equ       1
INDY                equ       2
LIMMED              equ       3                   ; (long immediate)
OTHER               equ       4

; Rename variables for assem/disassem

AMODE               equ       TMP2                ; addressing mode
YFLAG               equ       TMP3
PNORM               equ       TMP4                ; page for normal opcode
OLDPC               equ       PTR8
PC                  equ       PTR1                ; program counter
PX                  equ       PTR2                ; page for X indexed
PY                  equ       PTR2+1              ; page for Y indexed
BASEOP              equ       PTR3                ; base opcode
CLASS               equ       PTR3+1              ; class
DISPC               equ       PTR4                ; PC for disassembler
BRADDR              equ       PTR5                ; relative branch offset
MNEPTR              equ       PTR6                ; pointer to table for dis
ASSCOMM             equ       PTR7                ; subcommand for assembler

;*******************************************************************************
                    #DATA
;*******************************************************************************

; Error messages for assembler

MSGDIR              dw        MSGA1               ; message table index
                    dw        MSGA2
                    dw        MSGA3
                    dw        MSGA4
                    dw        MSGA5
                    dw        MSGA6
                    dw        MSGA7
                    dw        MSGA8
                    dw        MSGA9

MSGA1               fcs       'Immediate mode illegal',LF
MSGA2               fcs       'Error in Mnemonics table',LF
MSGA3               fcs       'Illegal bit op',LF
MSGA4               fcs       'Bad argument',LF
MSGA5               fcs       'Mnemonic not found',LF
MSGA6               fcs       'Unknown addressing mode',LF
MSGA7               fcs       'Indexed addressing assumed',LF
MSGA8               fcs       'Syntax error',LF
MSGA9               fcs       'Branch out of range',LF

ASSEM.MSG           fcs       LF,'[/,=] Same  [^,-] Previous  [+,^J] Next  [CR] Next opcode  [^A,.] Quit',LF

;*******************************************************************************
                    #ROM
;*******************************************************************************

;*******************************************************************************
; oldpc = rambase;
; a = wskip();
; if (a != cr)
; buffarg()
; a = wskip();
; if ( a != cr ) return(error);
; oldpc = a;

ASSEM               proc
                    ldx       #ASSEM.MSG
                    jsr       OUTSTRG
                    ldx       #RAMBS
                    stx       OLDPC
                    jsr       WSKIP
                    beq       Loop@@              ; jump if no argument
                    jsr       BUFFARG
                    jsr       WSKIP
                    beq       Go@@                ; jump if argument ok
                    ldx       #MSGA4              ; "bad argument"
                    jmp       OUTSTRG

Go@@                ldx       SHFTREG
                    stx       OLDPC

; repeat
; pc = oldpc;
; out2bsp(pc);
; disassem();
; a=readln();
; asscomm = a;  /* save command */
; if(a == [^,+,-,/,=]) outcrlf;
; if(a == 0) return(error);

Loop@@              ldx       OLDPC
                    stx       PC
                    jsr       OUTCRLF
                    ldx       #PC
                    jsr       OUT2BSP             ; output the address
                    jsr       DISASSM             ; disassemble opcode
                    jsr       TABTO
                    lda       #PROMPT             ; prompt user
                    jsr       OUTA                ; output prompt character
                    jsr       READLN              ; read input for assembly
                    sta       ASSCOMM

                    cbeqa     #'^',Operator@@     ; jump if '^'
                    cbeqa     #'+',Operator@@     ; jump if '+'
                    cbeqa     #'-',Operator@@     ; jump if '-'
                    cbeqa     #'/',Operator@@     ; jump if '/'
                    cbeqa     #'=',Operator@@     ; jump if '='

                    tsta
                    bne       Cont@@              ; jump if none of above
                    rts                           ; return if bad input

Operator@@          jsr       OUTCRLF
Cont@@              pshx
                    ldx       #PC
                    jsr       OUT2BSP             ; output the address
                    pulx
                    jsr:5     OUTSPAC             ; come here for CR or LF

; b = parse(input); /* get mnemonic */
; if(b > 5) print("not found"); asscomm='/';
; elseif(b >= 1)
; msrch();
; if(class==$FF)
; print("not found"); asscomm='/';
; else
; a = doop(opcode,class);
; if(a == 0) dispc=0;
; else process error; asscomm='/';

                    jsr       PARSE
                    cmpb      #5
                    ble       _1@@                ; jump if mnemonic <= 5 chars
                    ldx       #MSGA5              ; "mnemonic not found"
                    jsr       OUTSTRG
                    bra       _4@@

_1@@                tstb
                    beq       _5@@                ; jump if no input
                    jsr       MSRCH
                    lda       CLASS
                    cbnea     #$FF,_2@@
                    ldx       #MSGA5              ; "mnemonic not found"
                    jsr       OUTSTRG
                    bra       _4@@

_2@@                jsr       DOOP
                    tsta
                    bne       _3@@                ; jump if doop error
                    clrx
                    stx       DISPC               ; indicate good assembly
                    bra       _5@@

_3@@                deca                          ; A = error message index
                    tab
                    ldx       #MSGDIR
                    abx:2
                    ldx       ,x
                    jsr       OUTSTRG             ; output error message
_4@@                clr       ASSCOMM             ; error command

; /* compute next address - asscomm holds subcommand
; and dispc indicates if valid assembly occured. */
; if(asscomm== ^ or -) oldpc--;
; if(asscomm==(lf or + or cr)
; if(dispc==0) oldpc=pc;   /* good assembly */
; else
; if(asscomm==lf or +) dispc= ++oldpc;
; oldpc=dispc;
; until(eot)

_5@@                lda       ASSCOMM

                    cbeqa     #'^',_6@@           ; jump if '^'
                    cbnea     #'-',_7@@           ; jump not '-'

_6@@                ldx       OLDPC               ; back up for '^' or '-'
                    dex
                    stx       OLDPC
                    bra       _12@@

_7@@                cbeqa     #LF,_8@@            ; jump if linefeed
                    cbeqa     #'+',_8@@           ; jump if '+'
                    cbnea     #CR,_12@@           ; jump if not cr

_8@@                ldx       DISPC
                    bne       _9@@                ; jump if dispc != 0
                    ldx       PC
                    stx       OLDPC
                    bra       _12@@

_9@@                cbeqa     #LF,_10@@           ; jump not lf
                    cbnea     #'+',_11@@          ; jump not lf or '+'

_10@@               ldx       OLDPC
                    inx
                    stx       DISPC
_11@@               ldx       DISPC
                    stx       OLDPC
_12@@               jmp       Loop@@

;*******************************************************************************
; readln() --- Read input from terminal into buffer
; until a command character is read (cr,lf,/,^).
; If more chars are typed than the buffer will hold,
; the extra characters are overwritten on the end.
; On exit: b=number of chars read, a=0 if quit,
; else a=next command.
;*******************************************************************************
; for(b==0;b<=bufflng;b++) inbuff[b] = cr;

READLN              proc
                    clrb
                    lda       #CR                 ; carriage ret
InitLoop@@          ldx       #INBUFF
                    abx
                    sta       ,x                  ; initialize input buffer
                    incb
                    cmpb      #BUFFLNG
                    blt       InitLoop@@

; b=0;
; repeat
; if(a == (ctla, cntlc, cntld, cntlx, del))
; return(a=0);
; if(a == backspace)
; if(b > 0) b--;
; else b=0;
; else  inbuff[b] = upcase(a);
; if(b < bufflng) b++;
; until (a == [cr,lf,+,^,-,/,=])
; return(a);

                    clrb
Loop@@              jsr       INCHAR

                    cbeqa     #DEL,RLNQUIT        ; Delete
                    cbeqa     #CTL_X,RLNQUIT      ; Control X
                    cbeqa     #CTL_A,RLNQUIT      ; Control A
                    cbeqa     #'.',RLNQUIT        ; Period
                    cbeqa     #3,RLNQUIT          ; Control C
                    cbeqa     #4,RLNQUIT          ; Control D
                    cbnea     #8,BS@@             ; backspace

                    decb
                    bgt       Loop@@
                    bra       READLN              ; start over

BS@@                ldx       #INBUFF
                    abx
                    jsr       UPCASE
                    sta       ,x                  ; put char in buffer
                    cmpb      #BUFFLNG            ; max buffer length
                    bge       Skip@@              ; jump if buffer full
                    incb                          ; move buffer pointer
Skip@@              bsr       ASSCHEK             ; check for subcommand
                    bne       Loop@@
                    rts

;*******************************************************************************

RLNQUIT             proc
                    clra                          ; quit
                    rts

;*******************************************************************************
; parse() -parse out the mnemonic from INBUFF
; to COMBUFF. on exit: b=number of chars parsed.
;*******************************************************************************
; combuff[3] = <space>;   initialize 4th character to space.
; ptrbuff[] = inbuff[];
; a=wskip();
; for (b = 0; b = 5; b++)
; a=readbuff(); incbuff();
; if (a = (cr,lf,^,/,wspace)) return(b);
; combuff[b] = upcase(a);
; return(b);

PARSE               proc
                    lda       #' '
                    sta       COMBUFF+3
                    ldx       #INBUFF             ; initialize buffer ptr
                    stx       PTR0
                    jsr       WSKIP               ; find first character
                    clrb
Loop@@              jsr       READBUFF            ; read character
                    jsr       INCBUFF
                    jsr       WCHEK
                    beq       Done@@              ; jump if whitespace
                    bsr       ASSCHEK
                    beq       Done@@              ; jump if end of line
                    jsr       UPCASE              ; convert to upper case
                    ldx       #COMBUFF
                    abx
                    sta       ,x                  ; store in combuff
                    incb
                    cmpb      #5
                    ble       Loop@@              ; loop 6 times
Done@@              rts

;*******************************************************************************
; asschek() -perform compares for lf, cr, ^, /, +, -, =
;*******************************************************************************

ASSCHEK             proc
                    cbeqa     #LF,Done@@          ; linefeed
                    cbeqa     #CR,Done@@          ; carriage ret
                    cbeqa     #'^',Done@@         ; up arrow
                    cbeqa     #'/',Done@@         ; slash
                    cbeqa     #'+',Done@@         ; plus
                    cbeqa     #'-',Done@@         ; minus
                    cmpa      #'='                ; equals
Done@@              rts

;*******************************************************************************
; msrch() --- Search MNETABL for mnemonic in COMBUFF.
; stores base opcode at baseop and class at class.
; Class = $FF if not found.
;*******************************************************************************
; while ( != EOF )
; if (COMBUFF[0-3] = MNETABL[0-3])
; return(MNETABL[4],MNETABL[5]);
; else *MNETABL =+ 6

MSRCH               proc
                    ldx       #MNETABL            ; pointer to mnemonic table
                    ldy       #COMBUFF            ; pointer to string
                    bra       Go@@

Loop@@              ldb       #6
                    abx                           ; point to next table entry
Go@@                lda       ,x                  ; read table
                    bne       Skip@@              ; jump if not end of table
                    lda       #$FF
                    sta       CLASS               ; FF = not in table
                    rts

Skip@@              cmpa      ,y                  ; op[0] = tabl[0] ?
                    bne       Loop@@

                    lda       1,x
                    cmpa      1,y                 ; op[1] = tabl[1] ?
                    bne       Loop@@

                    lda       2,x
                    cmpa      2,y                 ; op[2] = tabl[2] ?
                    bne       Loop@@

                    lda       3,x
                    cmpa      3,y                 ; op[2] = tabl[2] ?
                    bne       Loop@@

                    ldd       4,x                 ; opcode, class
                    sta       BASEOP
                    stb       CLASS

                    rts

;*******************************************************************************
; doop(baseop,class) --- process mnemonic.
; on exit: a=error code corresponding to error messages.
;*******************************************************************************
; amode = OTHER; /* addressing mode */
; yflag = 0;     /* ynoimm, nlimm, and cpd flag */
; x[] = ptrbuff[]

DOOP                proc
                    lda       #OTHER
                    sta       AMODE               ; mode
                    clr       YFLAG
                    ldx       PTR0

; while (*x != end of buffer)
; if (x[0]++ == ',')
; if (x[0] == 'y') amode = INDY;
; else amod = INDX;
; break;
; a = wskip()
; if( a == '#' ) amode = IMMED;

Loop@@              cmpx      #ENDBUFF            ; (end of buffer)
                    beq       Skip@@              ; jump if end of buffer
                    ldd       ,x                  ; read 2 chars from buffer
                    inx                           ; move pointer
                    cmpa      #','
                    bne       Loop@@
                    cmpb      #'Y'                ; look for ",y"
                    bne       NotY@@
                    lda       #INDY
                    sta       AMODE
                    bra       Skip@@

NotY@@              cmpb      #'X'                ; look for ",x"
                    bne       Skip@@              ; jump if not x
                    lda       #INDX
                    sta       AMODE
Skip@@              jsr       WSKIP
                    cbnea     #'#',Imm@@          ; look for immediate mode
                    jsr       INCBUFF             ; point at argument
                    lda       #IMMED
                    sta       AMODE
Imm@@               ldb       CLASS               ; switch(class)

                    cmpb      #P2INH
                    beq       DOP2I

                    cmpb      #INH
                    beq       DOINH

                    cmpb      #REL
                    beq       DOREL

                    cmpb      #LIMM
                    beq       DOLIM

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
                    jeq       DOBTB

                    cmpb      #SETCLR
                    jeq       DOSET

; default: return("error in mnemonic table");

                    lda       #2
                    rts

;*******************************************************************************
; case P2INH: emit(PAGE2)

DOP2I               proc
                    lda       #PAGE2
                    jsr       EMIT

;*******************************************************************************
; case INH: emit(baseop);
; return(0);

DOINH               proc
                    lda       BASEOP
                    jmp       DOGOTH2

;*******************************************************************************
; case REL: a = assarg();
; if(a=4) return(a);
; d = address - pc + 2;
; if ($7f >= d >= $ff82)
; return (out of range);
; emit(opcode);
; emit(offset);
; return(0);

DOREL               proc
                    jsr       ASSARG
                    cbeqa     #4,Done@@           ; jump if arg ok
                    ldd       SHFTREG             ; get branch address
                    ldx       PC                  ; get program counter
                    inx:2                         ; point to end of opcode
                    stx       BRADDR
                    subd      BRADDR              ; calculate offset
                    std       BRADDR              ; save result
                    cmpd      #$7F                ; in range ?
                    bls       _1@@                ; jump if in range
                    cmpd      #$FF80
                    bhs       _1@@                ; jump if in range
                    lda       #9                  ; 'Out of range'
Done@@              rts

_1@@                lda       BASEOP
                    jsr       EMIT                ; emit opcode
                    lda       BRADDR+1
                    jmp       DOGOTH2

;*******************************************************************************
; case LIMM: if (amode == IMMED) amode = LIMMED;

DOLIM               proc
                    lda       AMODE
                    cbnea     #IMMED,DONOI
                    lda       #LIMMED
                    sta       AMODE

;*******************************************************************************
; case NIMM: if (amode == IMMED)
; return("Immediate mode illegal");

DONOI               proc
                    lda       AMODE
                    cbnea     #IMMED,DOGENE       ; jump if not immediate
                    lda       #1                  ; "immediate mode illegal"
                    rts

;*******************************************************************************
; case GEN: dogen(baseop,amode,PAGE1,PAGE1,PAGE2);
; return;

DOGENE              proc
                    lda       #PAGE1
                    sta       PNORM
                    sta       PX
                    lda       #PAGE2
                    sta       PY
                    jmp       DOGEN

;*******************************************************************************
; case GRP2: if (amode == INDY)
; emit(PAGE2);
; amode = INDX;
; if( amode == INDX )
; doindx(baseop);
; else a = assarg();
; if(a=4) return(a);
; emit(opcode+0x10);
; emit(extended address);
; return;

DOGRP               proc
                    lda       AMODE
                    cbnea     #INDY,Skip@@
                    lda       #PAGE2
                    jsr       EMIT
                    lda       #INDX
                    sta       AMODE
Skip@@              lda       AMODE
                    cmpa      #INDX
                    jeq       DOINDEX
                    lda       BASEOP
                    adda      #16
                    jsr       EMIT
                    jsr       ASSARG
                    cbeqa     #4,Done@@           ; jump if bad arg
                    ldd       SHFTREG             ; extended address
                    jmp       DOGOTH20
Done@@              equ       :AnRTS

;*******************************************************************************
; case CPD: if (amode == IMMED)
; amode = LIMMED; /* cpd */
; if( amode == INDY ) yflag = 1;
; dogen(baseop,amode,PAGE3,PAGE3,PAGE4);
; return;

DOCPD               proc
                    lda       AMODE
                    cbnea     #IMMED,_1@@
                    lda       #LIMMED
                    sta       AMODE
_1@@                lda       AMODE
                    cbnea     #INDY,_2@@
                    inc       YFLAG
_2@@                lda       #PAGE3
                    sta       PNORM
                    sta       PX
                    lda       #PAGE4
                    sta       PY
                    jmp       DOGEN

;*******************************************************************************
; case XNIMM: if (amode == IMMED)      /* stx */
; return("Immediate mode illegal");

DOXNOI              proc
                    lda       AMODE
                    cbnea     #IMMED,DOXLI
                    lda       #1                  ; "immediate mode illegal"
                    rts

;*******************************************************************************
; case XLIMM: if (amode == IMMED)  /* cpx, ldx */
; amode = LIMMED;
; dogen(baseop,amode,PAGE1,PAGE1,PAGE4);
; return;

DOXLI               proc
                    lda       AMODE
                    cbnea     #IMMED,_1@@
                    lda       #LIMMED
                    sta       AMODE
_1@@                lda       #PAGE1
                    sta       PNORM
                    sta       PX
                    lda       #PAGE4
                    sta       PY
                    jmp       DOGEN

;*******************************************************************************
; case YNIMM: if (amode == IMMED)      /* sty */
; return("Immediate mode illegal");

DOYNOI              proc
                    lda       AMODE
                    cbnea     #IMMED,DOYLI
                    lda       #1                  ; "immediate mode illegal"
                    rts

;*******************************************************************************
; case YLIMM: if (amode == INDY) yflag = 1;/* cpy, ldy */
; if(amode == IMMED) amode = LIMMED;
; dogen(opcode,amode,PAGE2,PAGE3,PAGE2);
; return;

DOYLI               proc
                    lda       AMODE
                    cbnea     #INDY,_1@@
                    inc       YFLAG
_1@@                cbnea     #IMMED,_2@@
                    lda       #LIMMED
                    sta       AMODE
_2@@                lda       #PAGE2
                    sta       PNORM
                    sta       PY
                    lda       #PAGE3
                    sta       PX
                    jmp       DOGEN

;*******************************************************************************
; case BTB:        /* bset, bclr */
; case SETCLR: a = bitop(baseop,amode,class);
; if(a=0) return(a = 3);
; if( amode == INDY )
; emit(PAGE2);
; amode = INDX;

DOBTB               proc
DOSET               bsr       BITOP
                    tsta
                    bne       _1@@
                    lda       #3                  ; "illegal bit op"
                    rts

_1@@                lda       AMODE
                    cbnea     #INDY,_2@@
                    lda       #PAGE2
                    jsr       EMIT
                    lda       #INDX
                    sta       AMODE
_2@@
; emit(baseop);
; a = assarg();
; if(a = 4) return(a);
; emit(index offset);
; if( amode == INDX )
; buffptr += 2;      /* skip ,x or ,y */

                    lda       BASEOP
                    jsr       EMIT
                    jsr       ASSARG
                    cbnea     #4,_3@@             ; jump if arg ok
                    rts

_3@@                lda       SHFTREG+1           ; index offset
                    jsr       EMIT
                    lda       AMODE
                    cbnea     #INDX,_4@@
                    jsr       INCBUFF.TWICE
_4@@
; a = assarg();
; if(a = 4) return(a);
; emit(mask);   /* mask */
; if( class == SETCLR )
; return;

                    jsr       ASSARG
                    cbeqa     #4,Done@@           ; jump if arg ok
                    lda       SHFTREG+1           ; mask
                    jsr       EMIT
                    lda       CLASS
                    cbnea     #SETCLR,_5@@
                    clra
Done@@              rts

_5@@
; a = assarg();
; if(a = 4) return(a);
; d = (pc+1) - shftreg;
; if ($7f >= d >= $ff82)
; return (out of range);
; emit(branch offset);
; return(0);

                    jsr       ASSARG
                    cbeqa     #4,Done@@           ; jump if arg ok
                    ldx       PC                  ; program counter
                    inx                           ; point to next inst
                    stx       BRADDR              ; save pc value
                    ldd       SHFTREG             ; get branch address
                    subd      BRADDR              ; calculate offset
                    cmpd      #$7F
                    bls       _6@@                ; jump if in range
                    cmpd      #$FF80
                    bhs       _6@@                ; jump if in range
                    clra
                    jsr       EMIT
                    lda       #9                  ; 'out of range'
                    rts

_6@@                tba                           ; offset
                    jmp       DOGOTH2

;*******************************************************************************
;**   bitop(baseop,amode,class) --- adjust opcode on bit
;**       manipulation instructions.  Returns opcode in a
;**       or a = 0 if error
;*******************************************************************************
; if( amode == INDX || amode == INDY ) return(op);
; if( class == SETCLR ) return(op-8);
; else if(class==BTB) return(op-12);
; else fatal("bitop");

BITOP               proc
                    lda       AMODE
                    ldb       CLASS
                    cbeqa     #INDX,Done@@
                    cbeqa     #INDY,Done@@        ; jump not indexed
                    cmpb      #SETCLR
                    bne       NotBsetBclr@@       ; jump not bset,bclr
                    lda       BASEOP              ; get opcode
                    suba      #8
                    sta       BASEOP
Done@@              rts

NotBsetBclr@@       cmpb      #BTB
                    bne       NotBranch@@         ; jump not bit branch
                    lda       BASEOP              ; get opcode
                    suba      #12
                    sta       BASEOP
                    rts

NotBranch@@         clra                          ; 0 = fatal bitop
                    rts

;*******************************************************************************
;** dogen(baseop,mode,pnorm,px,py) - process
;** general addressing modes. Returns A = error #.
;*******************************************************************************
; pnorm = page for normal addressing modes: IMM,DIR,EXT
; px = page for INDX addressing
; py = page for INDY addressing
; switch(amode)

DOGEN               proc
                    lda       AMODE

                    cbeqa     #LIMMED,DOGLIM
                    cbeqa     #IMMED,DOGIMM
                    cbeqa     #INDY,DOGINDY
                    cbeqa     #INDX,DOGINDX
                    cbeqa     #OTHER,DOGOTH

; *default: error("Unknown Addressing Mode");

                    lda       #6                  ; unknown addre...
                    rts

;*******************************************************************************
; case LIMMED: epage(pnorm);
; emit(baseop);
; a = assarg();
; if(a = 4) return(a);
; emit(2 bytes);
; return(0);

DOGLIM              proc
                    lda       PNORM
                    jsr       EPAGE
                    lda       BASEOP
                    jsr       EMIT
                    jsr       ASSARG              ; get next argument
                    cbeqa     #4,Done@@           ; jump if arg ok
                    ldd       SHFTREG
                    bra       DOGOTH20
Done@@              equ       :AnRTS

;*******************************************************************************
; case IMMED: epage(pnorm);
; emit(baseop);
; a = assarg();
; if(a = 4) return(a);
; emit(lobyte);
; return(0);

DOGIMM              proc
                    lda       PNORM
                    jsr       EPAGE
                    lda       BASEOP
                    jsr       EMIT
                    bsr       ASSARG
                    cbeqa     #4,Done@@           ; jump if arg ok
                    lda       SHFTREG+1
                    bra       DOGOTH2
Done@@              equ       :AnRTS

;*******************************************************************************
; case INDY: epage(py);
; a=doindex(op+0x20);
; return(a);

DOGINDY             proc
                    lda       PY
                    bsr       EPAGE
                    lda       BASEOP
                    adda      #$20
                    sta       BASEOP
                    bra       DOINDEX

;*******************************************************************************
; case INDX: epage(px);
; a=doindex(op+0x20);
; return(a);

DOGINDX             proc
                    lda       PX
                    bsr       EPAGE
                    lda       BASEOP
                    adda      #$20
                    sta       BASEOP
                    bra       DOINDEX

;*******************************************************************************
; case OTHER: a = assarg();
; if(a = 4) return(a);
; epage(pnorm);
; if(countu1 <= 2 digits)   /* direct */
; emit(op+0x10);
; emit(lobyte(Result));
; return(0);
; else    emit(op+0x30);    /* extended */
; eword(Result);
; return(0)

DOGOTH              proc
                    bsr       ASSARG
                    cbeqa     #4,Done@@           ; jump if arg ok
                    lda       PNORM
                    bsr       EPAGE
                    lda       COUNT
                    cmpa      #2
                    bgt       Skip@@
                    lda       BASEOP
                    adda      #$10                ; direct mode opcode
                    bsr       EMIT
                    lda       SHFTREG+1
                    bra       DOGOTH2

Skip@@              lda       BASEOP
                    adda      #$30                ; extended mode opcode
                    bsr       EMIT
                    ldd       SHFTREG
DOGOTH20            bsr       EMIT
                    tba
DOGOTH2             bsr       EMIT
                    clra
Done@@              rts

;*******************************************************************************
; doindex(op) --- handle all wierd stuff for
; indexed addressing. Returns a = error number.
;*******************************************************************************
; emit(baseop);
; a=assarg();
; if(a = 4) return(a);
; if( a != ',' ) return("Syntax");
; buffptr++
; a=readbuff()
; if( a != 'x' &&  != 'y') warn("Ind Addr Assumed");
; emit(lobyte);
; return(0);

DOINDEX             proc
                    lda       BASEOP
                    bsr       EMIT
                    bsr       ASSARG
                    cbeqa     #4,Done@@           ; jump if arg ok
                    cmpa      #','
                    beq       Comma@@
                    lda       #8                  ; "syntax error"
Done@@              rts

Comma@@             jsr       INCBUFF
                    jsr       READBUFF

                    cbeqa     #'Y',Index@@
                    cbeqa     #'X',Index@@

                    ldx       MSGA7               ; "index addr assumed"
                    jsr       OUTSTRG
Index@@             lda       SHFTREG+1
                    bra       DOGOTH2

;*******************************************************************************
; assarg(); - get argument.  Returns a = 4 if bad
; argument, else a = first non hex char.
;*******************************************************************************
; a = buffarg()
; if(asschk(aa) && countu1 != 0) return(a);
; return(bad argument);

ASSARG              proc
                    jsr       BUFFARG
                    jsr       ASSCHEK             ; check for command
                    beq       Done?@@             ; jump if ok
                    jsr       WCHEK               ; check for whitespace
                    bne       Fail@@              ; jump if not ok
Done?@@             tst       COUNT
                    bne       Done@@              ; jump if argument
Fail@@              lda       #4                  ; bad argument
Done@@              rts

;*******************************************************************************
; epage(a) --- emit page prebyte
;*******************************************************************************
; if( a != PAGE1 ) emit(a);

EPAGE               proc
                    cbeqa     #PAGE1,Done@@
;                   bra       EMIT
Done@@              equ       :AnRTS

;*******************************************************************************
; emit(a) --- emit contents of a
;*******************************************************************************

EMIT                proc
                    ldx       PC
                    jsr       WRITE               ; write a to x
                    jsr       OUT1BSP
                    stx       PC
                    rts

;-------------------------------------------------------------------------------
; Mnemonic table for HC11 line assembler
;-------------------------------------------------------------------------------

INH                 equ       $01                 ; inherent
P2INH               equ       $02                 ; page 2 inherent
GEN                 equ       $03                 ; general addressing
GRP2                equ       $04                 ; group 2
REL                 equ       $05                 ; relative
IMM                 equ       $06                 ; immediate
NIMM                equ       $07                 ; general except for immediate
LIMM                equ       $08                 ; 2 byte immediate
XLIMM               equ       $09                 ; longimm for x
XNIMM               equ       $10                 ; no immediate for x
YLIMM               equ       $11                 ; longimm for y
YNIMM               equ       $12                 ; no immediate for y
BTB                 equ       $13                 ; bit test and branch
SETCLR              equ       $14                 ; bit set or clear
CPD                 equ       $15                 ; compare d
BTBD                equ       $16                 ; bit test and branch direct
SETCLRD             equ       $17                 ; bit set or clear direct

;*******************************************************************************
; mnetabl - includes all '11 mnemonics, base opcodes,
; and type of instruction.  The assembler search routine
; depends on 4 characters for each mnemonic so that 3 char
; mnemonics are extended with a space and 5 char mnemonics
; are truncated.
;*******************************************************************************

;*******************************************************************************
                    #DATA
;*******************************************************************************

; Format is: Mnemonic, Base Opcode, Class

MNETABL             fcc       'ABA ',$1B,INH
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
                    fcb       0                   ; End of table

;*******************************************************************************

PG1                 equ       0
PG2                 equ       1
PG3                 equ       2
PG4                 equ       3

;*******************************************************************************
                    #ROM
;*******************************************************************************

;*******************************************************************************
; disassem() - disassemble the opcode.
;*******************************************************************************
; (check for page prebyte)
; baseop=pc[0];
; pnorm=PG1;
; if(baseop==$18) pnorm=PG2;
; if(baseop==$1A) pnorm=PG3;
; if(baseop==$CD) pnorm=PG4;
; if(pnorm != PG1) dispc=pc+1;
; else dispc=pc; (dispc points to next byte)

DISASSM             proc
                    ldx       PC                  ; address
                    lda       ,x                  ; opcode
                    ldb       #PG1

                    cbeqa     #$18,Page2@@        ; jump if page2
                    cbeqa     #$1A,Page3@@        ; jump if page3
                    cbnea     #$CD,Page1@@        ; jump if not page4

                    incb                          ; set up page value (Page4)
Page3@@             incb
Page2@@             incb
                    inx
Page1@@             stx       DISPC               ; point to opcode
                    stb       PNORM               ; save page

; If(opcode == ($00-$5F or $8D or $8F or $CF))
; if(pnorm == (PG3 or PG4))
; disillop(); return();
; b=disrch(opcode,NULL);
; if(b==0) disillop(); return();

                    lda       ,x                  ; get current opcode
                    sta       BASEOP
                    inx
                    stx       DISPC               ; point to next byte

                    cmpa      #$5F
                    bls       _1@@                ; jump if in range

                    cbeqa     #$8D,_1@@           ; jump if bsr
                    cbeqa     #$8F,_1@@           ; jump if xgdx

                    cmpa      #$CF
                    jne       DISGRP              ; try next part of map

_1@@                ldb       PNORM
                    cmpb      #PG3
                    jhs       DISILLOP            ; "illegal opcode"
                    ldb       BASEOP              ; opcode
                    clrb                          ; class=null
                    jsr       DISRCH
                    tstb
                    jeq       DISILLOP            ; "illegal opcode"

;*******************************************************************************
; if(opcode==$8D) dissrch(opcode,REL);
; if(opcode==($8F or $CF)) disrch(opcode,INH);

DISPEC              proc
                    lda       BASEOP
                    cbnea     #$8D,_1@@
                    ldb       #REL
                    bra       _3@@                ; look for BSR opcode

_1@@                cbeqa     #$8F,_2@@           ; jump if XGDX opcode
                    cbnea     #$CF,DISINH         ; jump not STOP opcode

_2@@                ldb       #INH
_3@@                jsr       DISRCH              ; find other entry in table
;                   bra       DISINH

;*******************************************************************************
; if(class==INH)           /* INH */
; if(pnorm==PG2)
; b=disrch(baseop,P2INH);
; if(b==0) disillop(); return();
; prntmne();
; return();

DISINH              proc
                    ldb       CLASS
                    cmpb      #INH
                    bne       DISREL              ; jump if not inherent
                    ldb       PNORM
                    cmpb      #PG1
                    beq       _1@@                ; jump if page1
                    lda       BASEOP              ; get opcode
                    ldb       #P2INH              ; class=p2inh
                    jsr       DISRCH
                    tstb
                    jeq       DISILLOP            ; "illegal opcode"
_1@@                jmp       PRNTMNE

;*******************************************************************************
; elseif(class=REL)       /* REL */
; if(pnorm != PG1)
; disillop(); return();
; prntmne();
; disrelad();
; return();

DISREL              proc
                    ldb       CLASS
                    cmpb      #REL
                    bne       DISBTD
                    tst       PNORM
                    jne       DISILLOP            ; "illegal opcode"
                    jsr       PRNTMNE             ; output mnemonic
                    jmp       DISRELAD            ; compute relative address

;*******************************************************************************
; else           /* SETCLR,SETCLRD,BTB,BTBD */
; if(class == (SETCLRD or BTBD))
; if(pnorm != PG1)
; disillop(); return();   /* illop */
; prntmne();           /* direct */
; disdir();           /* output $byte */
; else (class == (SETCLR or BTB))
; prntmne();           /* indexed */
; disindx();
; outspac();
; disdir();
; outspac();
; if(class == (BTB or BTBD))
; disrelad();
; return();

DISBTD              proc
                    ldb       CLASS
                    cmpb      #SETCLRD
                    beq       _1@@
                    cmpb      #BTBD
                    bne       _2@@                ; jump not direct bitop
_1@@                tst       PNORM
                    jne       DISILLOP
                    jsr       PRNTMNE
                    jsr       DISDIR              ; operand(direct)
                    bra       _3@@
_2@@                jsr       PRNTMNE
                    jsr       DISINDX             ; operand(indexed)
_3@@                jsr       OUTSPAC
                    jsr       DISDIR              ; mask
                    ldb       CLASS
                    cmpb      #BTB
                    beq       _4@@                ; jump if btb
                    cmpb      #BTBD
                    bne       Done@@              ; jump if not bit branch
_4@@                jmp       DISRELAD            ; relative address
Done@@              rts

;*******************************************************************************
; Elseif($60 <= opcode <= $7F)  /*  GRP2 */
; if(pnorm == (PG3 or PG4))
; disillop(); return();
; if((pnorm==PG2) and (opcode != $6x))
; disillop(); return();
; b=disrch(baseop & $6F,NULL);
; if(b==0) disillop(); return();
; prntmne();
; if(opcode == $6x)
; disindx();
; else
; disext();
; return();

DISGRP              proc
                    cmpa      #$7F                ; A=opcode
                    bhi       DISNEXT             ; try next part of map
                    ldb       PNORM
                    cmpb      #PG3
                    jhs       DISILLOP            ; "illegal opcode"
                    anda      #$6F                ; mask bit 4
                    clrb                          ; class=null
                    jsr       DISRCH
                    tstb
                    jeq       DISILLOP            ; "illegal opcode"
                    jsr       PRNTMNE
                    lda       BASEOP              ; get opcode
                    anda      #$F0
                    cmpa      #$60
                    jeq       DISINDX             ; operand(indexed)
                    jmp       DISEXT              ; operand(extended)

;*******************************************************************************
; Else  ($80 <= opcode <= $FF)
; if(opcode == ($87 or $C7))
; disillop(); return();
; b=disrch(opcode&$CF,NULL);
; if(b==0) disillop(); return();

DISNEXT             proc
                    cmpa      #$87                ; A=opcode
                    beq       _1@@
                    cmpa      #$C7
                    bne       _2@@
_1@@                jmp       DISILLOP            ; "illegal opcode"
_2@@                anda      #$CF
                    clrb                          ; class=null
                    jsr       DISRCH
                    tstb
                    jeq       DISILLOP            ; "illegal opcode"

;*******************************************************************************
; if(opcode&$CF==$8D) disrch(baseop,NIMM; (jsr)
; if(opcode&$CF==$8F) disrch(baseop,NIMM; (sts)
; if(opcode&$CF==$CF) disrch(baseop,XNIMM; (stx)
; if(opcode&$CF==$83) disrch(baseop,LIMM); (subd)

DISNEW              proc
                    lda       BASEOP
                    anda      #$CF
                    cmpa      #$8D
                    bne       _1@@                ; jump not jsr
                    ldb       #NIMM
                    bra       _4@@

_1@@                cmpa      #$8F
                    bne       _2@@                ; jump not sts
                    ldb       #NIMM
                    bra       _4@@

_2@@                cmpa      #$CF
                    bne       _3@@                ; jump not stx
                    ldb       #XNIMM
                    bra       _4@@

_3@@                cmpa      #$83
                    bne       DISGEN              ; jump not subd
                    ldb       #LIMM
_4@@                bsr       DISRCH
                    tstb
                    jeq       DISILLOP            ; "illegal opcode"
;                   bra       DISGEN

;*******************************************************************************
; if(class == (GEN or NIMM or LIMM   ))   /* GEN,NIMM,LIMM,CPD */
; if(opcode&$CF==$83)
; if(pnorm==(PG3 or PG4)) disrch(opcode#$CF,CPD)
; class=LIMM;
; if((pnorm == (PG2 or PG4) and (opcode != ($Ax or $Ex)))
; disillop(); return();
; disgenrl();
; return();

DISGEN              proc
                    ldb       CLASS               ; get class
                    cmpb      #GEN
                    beq       _1@@
                    cmpb      #NIMM
                    beq       _1@@
                    cmpb      #LIMM
                    bne       DISXLN              ; jump if other class
_1@@                lda       BASEOP
                    anda      #$CF
                    cmpa      #$83
                    bne       _2@@                ; jump if not #$83
                    ldb       PNORM
                    cmpb      #PG3
                    blo       _2@@                ; jump not pg3 or 4
                    ldb       #CPD
                    bsr       DISRCH              ; look for cpd mne
                    ldb       #LIMM
                    stb       CLASS               ; set class to limm
_2@@                ldb       PNORM
                    cmpb      #PG2
                    beq       _3@@                ; jump if page 2
                    cmpb      #PG4
                    bne       _4@@                ; jump not page 2 or 4
_3@@                lda       BASEOP
                    anda      #$B0                ; mask bits 6,3-0
                    cmpa      #$A0
                    jne       DISILLOP            ; "illegal opcode"
_4@@                jmp       DISGENRL            ; process general class

;*******************************************************************************
; else       /* XLIMM,XNIMM,YLIMM,YNIMM */
; if(pnorm==(PG2 or PG3))
; if(class==XLIMM) disrch(opcode&$CF,YLIMM);
; else disrch(opcode&$CF,YNIMM);
; if((pnorm == (PG3 or PG4))
; if(opcode != ($Ax or $Ex))
; disillop(); return();
; class=LIMM;
; disgen();
; return();

DISXLN              proc
                    ldb       PNORM
                    cmpb      #PG2
                    beq       _1@@                ; jump if page2
                    cmpb      #PG3
                    bne       _4@@                ; jump not page3
_1@@                lda       BASEOP
                    anda      #$CF
                    ldb       CLASS
                    cmpb      #XLIMM
                    bne       _2@@
                    ldb       #YLIMM
                    bra       _3@@                ; look for ylimm

_2@@                ldb       #YNIMM              ; look for ynimm
_3@@                bsr       DISRCH
_4@@                ldb       PNORM
                    cmpb      #PG3
                    blo       _5@@                ; jump if page 1 or 2
                    lda       BASEOP              ; get opcode
                    anda      #$B0                ; mask bits 6,3-0
                    cmpa      #$A0
                    jne       DISILLOP            ; "illegal opcode"
_5@@                ldb       #LIMM
                    stb       CLASS
                    bra       DISGENRL            ; process general class

;*******************************************************************************
; disrch(a=opcode,b=class)
; return b=0 if not found
; else mneptr=points to mnemonic
; class=class of opcode
;*******************************************************************************
; x=#MNETABL
; while(x[0] != eot)
; if((opcode==x[4]) && ((class=NULL) || (class=x[5])))
; mneptr=x;
; class=x[5];
; return(1);
; x += 6;
; return(0);      /* not found */

DISRCH              proc
                    ldx       #MNETABL            ; point to top of table
Loop@@              cmpa      4,x                 ; test opcode
                    bne       Skip@@              ; jump not this entry
                    tstb
                    beq       Null@@              ; jump if class=null
                    cmpb      5,x                 ; test class
                    bne       Skip@@              ; jump not this entry
Null@@              ldb       5,x
                    stb       CLASS
                    stx       MNEPTR              ; return ptr to mnemonic
                    incb
                    rts                           ; return found

Skip@@              aix       #6
                    tst       ,x
                    bne       Loop@@
                    clrb
                    rts                           ; return not found

;*******************************************************************************
; prntmne() - output the mnemonic pointed
; at by mneptr.
;*******************************************************************************
; outa(mneptr[0-3]);
; outspac;
; return();

PRNTMNE             proc
                    ldx       MNEPTR
                    lda       ,x
                    jsr       OUTA                ; output char1
                    lda       1,x
                    jsr       OUTA                ; output char2
                    lda       2,x
                    jsr       OUTA                ; output char3
                    lda       3,x
                    jsr       OUTA                ; output char4
                    jmp       OUTSPAC

;*******************************************************************************
; disindx() - process indexed mode
;*******************************************************************************
; disdir();
; outa(',');
; if(pnorm == (PG2 or PG4)) outa('Y');
; else outa('X');
; return();

DISINDX             proc
                    bsr       DISDIR              ; output $byte
                    lda       #','
                    jsr       OUTA                ; output ,
                    ldb       PNORM
                    cmpb      #PG2
                    beq       _1@@                ; jump if page2
                    cmpb      #PG4
                    bne       _2@@                ; jump if not page4
_1@@                lda       #'Y'
                    bra       _3@@

_2@@                lda       #'X'
_3@@                jmp       OUTA                ; output X or Y

;*******************************************************************************
; disrelad() - compute and output relative address.
;*******************************************************************************
; braddr = dispc[0] + (dispc++);( 2's comp arith)
; outa('$');
; out2bsp(braddr);
; return();

DISRELAD            proc
                    ldx       DISPC
                    ldb       ,x                  ; get relative offset
                    inx
                    stx       DISPC
                    tstb
                    bmi       Loop@@              ; jump if negative
                    abx
                    bra       Skip@@

Loop@@              dex
                    incb
                    bne       Loop@@              ; subtract
Skip@@              stx       BRADDR              ; save address
                    jsr       OUTSPAC
                    lda       #'$'
                    jsr       OUTA
                    ldx       #BRADDR
                    jmp       OUT2BSP             ; output address

;*******************************************************************************
; disgenrl() - output data for the general cases which
; includes immediate, direct, indexed, and extended modes.
;*******************************************************************************
; prntmne();
; if(baseop == ($8x or $Cx))   /* immediate */
; outa('#');
; disdir();
; if(class == LIMM)
; out1byt(dispc++);
; elseif(baseop == ($9x or $Dx))  /* direct */
; disdir();
; elseif(baseop == ($Ax or $Ex)) /* indexed */
; disindx();
; else  (baseop == ($Bx or $Fx)) /* extended */
; disext();
; return();

DISGENRL            proc
                    bsr       PRNTMNE             ; print mnemonic
                    lda       BASEOP              ; get opcode
                    anda      #$B0                ; mask bits 6,3-0
                    cmpa      #$80
                    bne       Direct@@            ; jump if not immed
                    lda       #'#'                ; do immediate
                    jsr       OUTA
                    bsr       DISDIR
                    ldb       CLASS
                    cmpb      #LIMM
                    bne       Done@@              ; jump class = limm

                    ldx       DISPC
                    jsr       OUT1BYT
                    stx       DISPC
Done@@              rts

Direct@@            cmpa      #$90
                    beq       DISDIR              ; do direct

                    cmpa      #$A0
                    beq       DISINDX             ; do extended

                    bra       DISEXT              ; do extended

;*******************************************************************************
; disdir() - output "$ next byte"
;*******************************************************************************

DISDIR              proc
                    lda       #'$'
                    jsr       OUTA
                    ldx       DISPC
                    jsr       OUT1BYT
                    stx       DISPC
                    rts

;*******************************************************************************
; disext() - output "$ next 2 bytes"
;*******************************************************************************

DISEXT              proc
                    bsr       DISDIR
                    jsr       OUT1BSP
                    stx       DISPC
                    rts

;*******************************************************************************
; disillop() - output "illegal opcode"
;*******************************************************************************

DISILLOP            proc
                    pshx
                    ldx       #Msg@@
                    jsr       OUTSTRG0            ; no CR
                    pulx
                    rts
;-------------------------------------------------------------------------------
                    #push
                    #DATA
Msg@@               fcs       'ILLOP'
                    #pull

;*******************************************************************************
; Help  -  List buffalo commands to terminal.
;*******************************************************************************

HELP                proc
                    ldx       #Msg@@
                    jmp       OUTSTRG             ; print help screen

;*******************************************************************************
                    #DATA
;*******************************************************************************

Msg@@               fcc       CR
                    fcc       'List of available commands follows',LF,LF
                    fcc       'ASM [<addr>].....................Line asm/disasm',LF
                    fcc       'BF <addr1> <addr2> [<data>]......Block fill memory',LF
                    fcc       'BR [-][<addr>]...................Set up breakpoint table',LF
                    fcc       'BULK.............................Erase int. EEPROM',LF
                    fcc       'BULKALL..........................Erase int. EEPROM and CONFIG',LF
                    fcc       'CALL [<addr>]....................Call subroutine',LF
                    fcc       'CLS..............................Clear the screen',LF
                    fcc       'GO [<addr>]......................Execute code at address',LF
                    fcc       'PROCEED..........................Continue execution',LF
                    fcc       'EEMOD [<addr> [<addr>]]..........Modify int. EEPROM range',LF
                    fcc       'LOAD, VERIFY [T] <dwnld command> Load or verify S-records',LF
                    fcc       'MD [<addr1> [<addr2>]]...........Memory Dump',LF
                    fcc       'MM [<addr>] or [<addr>]/.........Memory Modify',LF
                    fcc       'MOVE <s1> <s2> [<d>].............Block move (copy)',LF
                    fcc       'OFFSET [-]<arg>..................Offset for download',LF
                    fcc       'RM [P,Y,X,A,B,C,S]...............Register modify',LF
                    fcc       'STOPAT <addr>....................Trace until addr',LF
                    fcc       'T [<n>]..........................Trace n instructions',LF
                    fcc       '[CTLW]...........................Wait (screen output)',LF
                    fcc       '[CTLX,DEL].......................Abort',LF
                    fcs       '[CR].............................Repeat last cmd',LF

;*******************************************************************************
                    #ROM
;*******************************************************************************

;*******************************************************************************
; call [<addr>] - Execute a jsr to <addr> or user
; PC value.  Return to monitor via  rts or breakpoint.
;*******************************************************************************
; a = wskip();
; if(a != cr)
; a = buffarg();
; a = wskip();
; if(a != cr) return(bad argument)
; pc = shftreg;

CALL                proc
                    jsr       WSKIP
                    beq       NoArg@@             ; jump if no arg
                    jsr       BUFFARG
                    jsr       WSKIP
                    beq       Skip@@              ; jump if CR
                    ldx       #MSG8               ; "bad argument"
                    jmp       OUTSTRG

Skip@@              ldx       SHFTREG
                    stx       REGISTERS           ; PC = <addr>

; put return address on user stack
; setbps();
; restack();     /* restack and go*/

NoArg@@             ldx       UserSP
                    dex                           ; user stack pointer
                    ldd       #RETURN             ; return address
                    std       ,x
                    dex
                    stx       UserSP              ; new user stack pointer
                    #ifdef    __WSI__
                    jsr       SETBPS
                    #else
                    bsr       SETBPS
                    #endif
                    clr       TMP2                ; 1=go, 0=call
                    jmp       RESTACK             ; go to user code

;*******************************************************************************
; return() - Return here from RTS after call command
;*******************************************************************************

RETURN              proc
                    psha                          ; save a register
                    tpa
                    sta       REGISTERS+8         ; cc register
                    sei                           ; mask interrupts
                    pula
                    std       REGISTERS+6         ; A and B registers
                    stx       REGISTERS+4         ; X register
                    sty       REGISTERS+2         ; Y register
                    sts       UserSP              ; user stack pointer
                    lds       PTR2                ; monitor stack pointer
                    jsr       REMBPS              ; remove breakpoints
                    jsr       OUTCRLF
                    jmp       RPRINT              ; print user registers

;*******************************************************************************
; cls - Clear the screen with a form feed (ASCII 12) character
;*******************************************************************************

CLS                 proc
                    lda       #ASCII_FF
                    jsr       OUTPUT
                    lda       #CR
                    jmp       OUTPUT

;*******************************************************************************
; proceed - Same as go except it ignores
; a breakpoint at the first opcode.  Calls
; runone for the first instruction only.
;*******************************************************************************

PROCEED             proc
                    jsr       RUNONE              ; run one instruction
                    jsr       CHKABRT             ; check for abort
                    clr       TMP2                ; flag for breakpoints
                    inc       TMP2                ; 1=go 0=call
                    bsr       SETBPS
                    jmp       RESTACK             ; go execute

;*******************************************************************************
; go [<addr>] - Execute starting at <addr> or
; user's pc value.  Executes an rti to user code.
; Returns to monitor via an swi through swiin.
;*******************************************************************************
; a = wskip();
; if(a != cr)
; a = buffarg();
; a = wskip();
; if(a != cr) return(bad argument)
; pc = shftreg;
; setbps();
; restack();     /* restack and go*/

GO                  proc
                    jsr       WSKIP
                    beq       NoArg@@             ; jump if no arg
                    jsr       BUFFARG
                    jsr       WSKIP
                    beq       CR@@                ; jump if CR
                    ldx       #MSG8               ; "bad argument"
                    jmp       OUTSTRG

CR@@                ldx       SHFTREG
                    stx       REGISTERS           ; PC = <addr>
NoArg@@             clr       TMP2
                    inc       TMP2                ; 1=go, 0=call
                    bsr       SETBPS
                    jmp       RESTACK             ; go to user code

;*******************************************************************************
; SWIIN - Breakpoints from "go" or "call" commands enter here.
; Remove breakpoints, save user registers, return

SWIIN               proc                          ; SWI entry point
                    tsx                           ; user SP -> X
                    lds       PTR2                ; restore monitor SP
                    jsr       SAVSTACK            ; save user regs
                    bsr       REMBPS              ; remove breakpoints from code
                    ldx       REGISTERS
                    dex
                    stx       REGISTERS           ; save user PC value

; if(call command) remove call return addr from user stack;

                    tst       TMP2                ; 1=go, 0=call
                    bne       Go@@                ; jump if go command
                    ldx       UserSP              ; remove return address
                    inx:2                         ; user stack pointer
                    stx       UserSP
Go@@                jsr       OUTCRLF             ; print register values
                    jmp       RPRINT

;*******************************************************************************
; setbps - Replace user code with swi's at breakpoint addresses
;*******************************************************************************
; for(b=0; b=6; b =+ 2)
; x = brktabl[b];
; if(x != 0)
; optabl[b] = x[0];
; x[0] = $3F;
; Put monitor SWI vector into jump table

SETBPS              proc
                    clrb
Loop@@              ldx       #BRKTABL
                    ldy       #PTR4
                    abx
                    aby
                    ldx       ,x                  ; breakpoint table entry
                    beq       Skip@@              ; jump if 0
                    lda       ,x                  ; save user opcode
                    sta       ,y
                    lda       #SWI_OPCODE
                    jsr       WRITE               ; insert SWI into code
Skip@@              addb      #2
                    cmpb      #6
                    ble       Loop@@              ; loop 4 times
                    ldx       JSWI+1
                    stx       PTR3                ; save user SWI vector
                    lda       #JMP_OPCODE
                    sta       JSWI
                    ldx       #SWIIN
                    stx       JSWI+1              ; monitor SWI vector
                    rts

;*******************************************************************************
; rembps - Remove breakpoints from user code.
;*******************************************************************************
; for(b=0; b=6; b =+ 2)
; x = brktabl[b];
; if(x != 0)
; x[0] = optabl[b];
; Replace user's SWI vector

REMBPS              proc
                    clrb
Loop@@              ldx       #BRKTABL
                    ldy       #PTR4
                    abx
                    aby
                    ldx       ,x                  ; breakpoint table entry
                    beq       Skip@@              ; jump if 0
                    lda       ,y
                    jsr       WRITE               ; restore user opcode
Skip@@              addb      #2
                    cmpb      #6
                    ble       Loop@@              ; loop 4 times
                    ldx       PTR3                ; restore user SWI vector
                    stx       JSWI+1
                    rts

;*******************************************************************************
; trace <n> - Trace n instructions starting
; at user's pc value. n is a hex number less than
; $FF (defaults to 1).
;*******************************************************************************
; a = wskip();
; if(a != cr)
; a = buffarg(); a = wskip();
; if(a != cr) return(bad argument);
; countt1 = n

TRACE               proc
                    clr       TMP4
                    inc       TMP4                ; default count=1
                    clr       CHRCNT              ; set up for display
                    jsr       WSKIP
                    beq       Loop@@              ; jump if CR
                    jsr       BUFFARG
                    jsr       WSKIP
                    beq       CR@@                ; jump if CR
                    ldx       #MSG8               ; "bad argument"
                    jmp       OUTSTRG

CR@@                lda       SHFTREG+1           ; n
                    sta       TMP4

; Disassemble the line about to be traced

Loop@@              ldb       TMP4
                    pshb
                    ldx       REGISTERS
                    stx       PTR1                ; PC value for disasm
                    jsr       DISASSM
                    pulb
                    stb       TMP4

; run one instruction
; rprint();
; while(count > 0) continue trace;

                    bsr       RUNONE
                    jsr       CHKABRT             ; check for abort
                    jsr       TABTO               ; print registers for
                    jsr       RPRINT              ; result of trace
                    dec       TMP4
                    beq       Done@@              ; quit if count=0
                    jsr       OUTCRLF
                    bra       Loop@@
Done@@              equ       :AnRTS

;*******************************************************************************
; stopat <addr> - Trace instructions until <addr> is reached.
;*******************************************************************************
; if((a=wskip) != cr)
; a = buffarg(); a = wskip();
; if(a != cr) return(bad argument);
; else return(bad argument);

STOPAT              proc
                    jsr       WSKIP
                    beq       STOPGO              ; jump if CR - no argument
                    jsr       BUFFARG
                    jsr       WSKIP
                    beq       Skip@@              ; jump if CR
                    ldx       #MSG8               ; "bad argument"
                    jmp       OUTSTRG

Skip@@              tst       COUNT
                    beq       STOPGO              ; jump if no argument
                    ldx       SHFTREG
                    stx       PTRMEM              ; update "current location"
;                   bra       STOPGO

;*******************************************************************************
; while(!(ptrmem <= userpc < ptrmem+10)) runone();
; rprint();

STOPGO              proc
                    ldd       REGISTERS           ; userpc
                    cmpd      PTRMEM
                    blo       Skip@@              ; if(userpc < ptrmem) runone
                    ldd       PTRMEM
                    addd      #10
                    cmpd      REGISTERS
                    bhi       Done@@              ; quit if ptrmem+10 > userpc
Skip@@              bsr       RUNONE
                    jsr       CHKABRT             ; check for abort
                    bra       STOPGO

Done@@              jsr       OUTCRLF
                    jmp       RPRINT              ; result of trace

;*******************************************************************************
; runone - This routine is used by the trace and
; execute commands to run only one user instruction.
; Control is passed to the user code via an RTI.  OC5
; is then used to trigger an XIRQ as soon as the first user
; opcode is fetched.  Control then returns to the monitor
; through XIRQIN.
; Externally, the OC5 pin must be wired to the XIRQ pin.
; For WSI, the PA0 pin must be wired to the XIRQ pin.
;*******************************************************************************
; Disable oc5 interrupts
; Put monitor XIRQ vector into jump table
; Unmask x bit in user ccr
; Setup OC5 to go low when first user instruction executed

RUNONE              proc
                    lda       #JMP_OPCODE         ; put "jmp xirqin" in jump table
                    sta       JTOC5

                    ldx       #XIRQIN
                    stx       JXIRQ+1

                    lda       REGISTERS+8         ; x bit will be cleared when
                    anda      #$BF                ; RTI is executed below
                    sta       REGISTERS+8

                    ldb       #87                 ; cycles to end of rti
                    ldx       TCNT                ; [ 5]
                    abx                           ; [ 3] 3~ |
                    stx       TOC5                ; [ 5] oc5 match register 5~ |

                    lda       TCTL1               ; [ 4] 4~ |
                    anda      #Bit0.^NOT          ; [ 2] set up oc5 low on match 2~ |
                    sta       TCTL1               ; [ 4] enable oc5 interrupt 4~ | 86~
;                   bra       RESTACK

;*******************************************************************************
; RESTACK - Restore user stack and RTI to user code.
; This code is the pathway to execution of user code.
; (Force extended addressing to maintain cycle count)
; Restore user stack and RTI to user code
                              #Cycles
RESTACK             proc                          ; Total: 67 cycles
                    sts       >PTR2               ; save monitor sp
                    lds       >UserSP             ; user stack pointer

                    ldx       >REGISTERS
                    pshx                          ; PC

                    ldx       >REGISTERS+2
                    pshx                          ; Y

                    ldx       >REGISTERS+4
                    pshx                          ; X

                    ldd       >REGISTERS+6
                    pshd                          ; D

                    lda       >REGISTERS+8
                    psha                          ; CCR

                    RTI

                    #temp     :cycles
          #if :temp <> 67
                    #Warning  Cycles ({:temp}) should be 67
          #endif

;*******************************************************************************
; Return here from run one line of user code.

XIRQIN              proc
                    tsx                           ; user SP -> X
                    lds       PTR2                ; restore monitor SP

;*******************************************************************************
; SAVSTACK - Save user's registers.
; On entry - x points to top of user stack.

SAVSTACK            proc
                    lda       ,x
                    sta       REGISTERS+8         ; user CCR

                    ldd       1,x
                    sta       REGISTERS+7         ; B
                    stb       REGISTERS+6         ; A

                    ldd       3,x
                    std       REGISTERS+4         ; X

                    ldd       5,x
                    std       REGISTERS+2         ; Y

                    ldd       7,x
                    std       REGISTERS           ; PC

                    ldb       #8
                    abx
                    stx       UserSP              ; user stack pointer

                    lda       TCTL1               ; force oc5 pin high which
                    ora       #3                  ; is tied to xirq line
                    sta       TCTL1

                    lda       #8
                    sta       CFORC

                    rts

;*******************************************************************************
; txbreak() - transmit break to host port.
; The duration of the transmitted break is
; approximately 200,000 E-clock cycles, or
; 100ms at 2.0 MHz.
;*******************************************************************************

TXBREAK             proc
                    ldx       #SCCR2              ; sci is host
                    bset      ,x,1                ; set send break bit
                    bsr       TXBWAIT
                    bclr      ,x,1                ; clear send break bit
;                   bra       TXBWAIT

;*******************************************************************************
                              #Cycles
TXBWAIT             proc
                    ldy       #DELAY@@            ; loop count
                              #Cycles
Loop@@              dey                           ; 7 cycle loop
                    bne       Loop@@
                              #temp :cycles
                    rts

DELAY@@             equ       100*BUS_KHZ-:cycles-:ocycles/:temp

;*******************************************************************************
; load(ptrbuff[]) - Load S1/S9 records from
; host to memory.  Ptrbuff[] points to string in
; input buffer which is a command to output s1/s9
; records from the host ("cat filename" for unix).
; Returns error and address if it can't write
; to a particular location.
;*******************************************************************************
; verify(ptrbuff[]) - Verify memory from load
; command.  Ptrbuff[] is same as for load.
; tmp3 is used as an error indication, 0=no errors,
; 1=receiver, 2=rom error, 3=checksum error.
;*******************************************************************************

VERIFY              proc
                    clr       TMP2
                    inc       TMP2                ; TMP2=1=verify
                    bra       ?LOAD

;*******************************************************************************

LOAD                proc
                    clr       TMP2                ; 0=load

; a=wskip();
; if(t option) hostdev = iodev;

?LOAD               clr       TMP3                ; clear error flag
                    jsr       WSKIP
                    jsr       UPCASE
                    jsr       INCBUFF
                    jsr       READBUFF            ; get next character
                    jsr       DECBUFF

                    cmpa      #CR
                    beq       S1@@                ; go wait for S1 records

; else while(not cr)
; read character from input buffer;
; send character to host;

Loop@@              jsr       READBUFF            ; get next char
                    jsr       INCBUFF
                    psha                          ; save char
                    jsr       OUTPUT              ; echo to terminal
                    pula
                    cmpa      #CR
                    bne       Loop@@              ; jump if not CR

; repeat:                           /* look for s records */
; if(hostdev != iodev) check abort;
; a = hostin();
; if(a = 'S')
; a = hostin;
; if(a = '1')
; checksum = 0;
; get byte count in b;
; get base address in x;
; while(byte count > 0)
; byte();
; x++; b--;
; if(tmp3=0)           /* no error */
; if(load) x[0] = shftreg+1;
; if(x[0] != shftreg+1)
; tmp3 = 2;    /* rom error */
; ptr3 = x;    /* save address */
; if(tmp3 = 0) do checksum;
; if(checksum err) tmp3 = 3; /* checksum error */

; Look for S-record header

S1@@                jsr       INPUT               ; read host

                    tsta
                    beq       S1@@                ; jump if no input

                    cmpa      #'S'
                    bne       S1@@                ; jump if not S

GetRecType@@        jsr       INPUT               ; read host

                    tsta
                    beq       GetRecType@@        ; jump if no input

                    cmpa      #'9'
                    beq       S9@@                ; jump if S9 record

                    cmpa      #'1'
                    bne       S1@@                ; jump if not S1

                    clr       TMP4                ; clear checksum

; Get Byte Count and Starting Address

                    bsr       GETBYTE
                    ldb       SHFTREG+1
                    subb      #2                  ; B = byte count
                    bsr:2     GETBYTE
                    pshb                          ; save byte count
                    ldd       SHFTREG
                    addd      LDOFFST             ; add offset
                    xgdx                          ; X = address+offset
                    pulb                          ; restore byte count
                    dex                           ; condition for loop

; Get and Store Incoming Data Byte

Data@@              bsr       GETBYTE             ; get next byte
                    inx
                    decb                          ; check byte count
                    beq       CRC@@               ; if b=0, go do checksum
                    tst       TMP3
                    bne       S1@@                ; jump if error flagged
                    tst       TMP2
                    bne       Skip@@              ; jump if verify
                    lda       SHFTREG+1
                    jsr       WRITE               ; load only

Skip@@              cmpa      ,x                  ; verify RAM location
                    beq       Data@@              ; jump if ram ok

                    lda       #2
                    sta       TMP3                ; indicate rom error
                    stx       PTR3                ; save error address
                    bra       Data@@              ; finish download

; Get and Test Checksum

CRC@@               tst       TMP3
                    bne       S1@@                ; jump if error already

                    lda       TMP4
                    inca                          ; do checksum
                    beq       S1@@                ; jump if S1 record okay

                    lda       #3
                    sta       TMP3                ; indicate checksum error
                    bra       S1@@

; if(a = '9')
; read rest of record;
; if(tmp3=2) return("[ptr3]");
; if(tmp3=1) return("rcv error");
; if(tmp3=3) return("checksum err");
; else return("done");

S9@@                bsr       GETBYTE
                    ldb       SHFTREG+1           ; b = byte count
EOL@@               bsr       GETBYTE
                    decb
                    bne       EOL@@               ; loop until end of record
                    ldb       #100
Delay@@             jsr       DLY10MS             ; delay 1 sec -let host finish
                    decb
                    bne       Delay@@
                    jsr       INPUT               ; clear comm device
                    ldd       #$7E0D              ; put dummy command in inbuff
                    std       INBUFF
                    ldx       #MSG9               ; "done" default msg
                    lda       TMP3
                    cmpa      #2
                    bne       RcvErr@@            ; jump not ROM error
                    ldx       #PTR3
                    jmp       OUT2BSP             ; address of ROM error

RcvErr@@            cmpa      #1
                    bne       CksErr@@            ; jump not rcv error
                    ldx       #MSG12              ; "rcv error"
                    bra       Done@@

CksErr@@            cmpa      #3
                    bne       Done@@              ; jump not checksum error
                    ldx       #MSG10              ; "checksum error"
Done@@              jmp       OUTSTRG

;*******************************************************************************
; getbyte() -  Read 2 ascii bytes from host and convert to one hex byte.
; Returns byte shifted into shftreg and added to tmp4.
;*******************************************************************************

GETBYTE             proc
                    pshb
                    pshx
_1@@                jsr       INPUT               ; read host (1st byte)
                    tsta
                    beq       _1@@                ; loop until input
                    jsr       HEXBIN
_2@@                jsr       INPUT               ; read host (2nd byte)
                    tsta
                    beq       _2@@                ; loop until input
                    jsr       HEXBIN
                    lda       SHFTREG+1
                    adda      TMP4
                    sta       TMP4                ; add to checksum
                    pulx
                    pulb
                    rts

;*******************************************************************************
; offset [<addr>]
; Specify offset to be added to s-record address when
; downloading from the host.
; OFFSET                -show the current offset
; OFFSET <data>         -current offset = data
; OFFSET -<data>        -current offset = 0 - data
;*******************************************************************************
; if(<data>) then offset = data;
; print(offset);

OFFSET              proc
                    clr       TMP4                ; minus indicator
                    jsr       WSKIP
                    beq       NoArg@@             ; jump if cr (no argument)
                    cmpa      #'-'
                    bne       Skip@@              ; jump not -
                    inc       TMP4                ; set minus sign flag
                    jsr       INCBUFF             ; move buffer pointer
                    jsr       WSKIP
Skip@@              jsr       BUFFARG             ; read argument
                    tst       COUNT
                    beq       Fail@@              ; jump if bad argument
                    jsr       WSKIP
                    bne       Fail@@              ; jump if not cr
                    ldd       SHFTREG             ; get offset value
                    tst       TMP4
                    beq       Pos@@               ; jump if positive
                    clrd                          ; negative - sub from 0
                    subd      SHFTREG
Pos@@               std       LDOFFST
NoArg@@             jsr       OUTCRLF             ; display current offset
                    ldx       #LDOFFST
                    jmp       OUT2BSP

Fail@@              ldx       #MSG8               ; "bad argument"
                    jmp       OUTSTRG

;*******************************************************************************
; register [<name>]  - prints the user regs
; and opens them for modification.  <name> is
; the first register opened (default = P).
; Subcommands:
; [<nn>]<space>  Opens the next register.
; [<nn>]<cr>     Return.
; The register value is only changed if
; <nn> is entered before the subcommand.
;*******************************************************************************
; x[] = reglist
; a = wskip(); a = upcase(a);
; if(a != cr)
; while( a != x[0] )
; if( x[0] = "s") return(bad argument);
; x[]++;
; incbuff(); a = wskip();
; if(a != cr) return(bad argument);

REGISTER            proc
                    ldx       #REGLIST
                    jsr       WSKIP               ; A = first char of arg
                    jsr       UPCASE              ; convert to upper case

                    cmpa      #CR
                    beq       _4@@                ; jump if no argument

Loop@@              cmpa      ,x
                    beq       Done@@

                    ldb       ,x
                    inx
                    cmpb      #'S'
                    bne       Loop@@              ; jump if not "S"

Fail@@              ldx       #MSG8               ; "bad argument"
                    jmp       OUTSTRG

Done@@              pshx
                    jsr       INCBUFF
                    jsr       WSKIP               ; next char after arg
                    pulx
                    bne       Fail@@              ; jump if not CR

; rprint();
; while(x[0] != "s")
; rprnt1(x);
; a = termarg();    /* read from terminal */
; if( ! dchek(a) ) return(bad argument);
; if(countu1 != 0)
; if(x[14] = 1)
; regs[x[7]++ = shftreg;
; regs[x[7]] = shftreg+1;
; if(a = cr) break;
; return;

_4@@                jsr       RPRINT              ; print all registers
_5@@                jsr       OUTCRLF
                    jsr       RPRNT1              ; print reg name
                    clr       SHFTREG
                    clr       SHFTREG+1
                    jsr       TERMARG             ; read subcommand
                    jsr       DCHEK
                    beq       _6@@                ; jump if delimeter
                    ldx       #MSG8               ; "bad argument"
                    jmp       OUTSTRG

_6@@                psha
                    pshx
                    tst       COUNT
                    beq       Skip@@              ; jump if no input
                    ldb       7,x                 ; get reg offset
                    lda       14,x                ; byte size
                    ldx       #REGISTERS          ; user registers
                    abx
                    tsta
                    beq       OneByte@@           ; jump if 1 byte reg
                    lda       SHFTREG
                    sta       ,x                  ; put in top byte
                    inx
OneByte@@           lda       SHFTREG+1
                    sta       ,x                  ; put in bottom byte
Skip@@              pulx
                    pula
                    ldb       ,x                  ; CHECK FOR REGISTER S
                    cmpb      #'S'
                    beq       Return@@            ; jump if "S"
                    inx                           ; point to next register
                    cmpa      #CR
                    bne       _5@@                ; jump if not CR
Return@@            rts

;*******************************************************************************
; xboot [<addr1> [<addr2>]] - Use SCI to talk to an 'hc11 in
; boot mode.  Downloads bytes from addr1 thru addr2.
; Default addr1 = $C000 and addr2 = $C0ff.
;
; IMPORTANT:
; if talking to an 'A8 or 'A2: use either default addresses or ONLY
; addr1 - this sends 256 bytes
; if talking to an 'E9: include BOTH addr1 and addr2 for variable
; length
;*******************************************************************************

; Get arguments
; If no args, default $C000

BOOT                proc
                    jsr       WSKIP
                    bne       NoArg@@             ; jump if arguments
                    ldx       #ADDR2              ; addr2 default
                    stx       PTR5
                    ldy       #ADDR1              ; addr1 default
                    bra       Default@@           ; go - use default address

; Else get arguments

NoArg@@             jsr       BUFFARG
                    tst       COUNT
                    beq       Fail@@              ; jump if no address

                    ldy       SHFTREG             ; start address (addr1)
                    jsr       WSKIP
                    bne       Addr2@@             ; go get addr2
                    sty       PTR5                ; default addr2...

                    ldd       PTR5                ; ...by taking addr1...
                    addd      #255                ; ...and adding 255 to it...
                    std       PTR5                ; ...for a total download of 256
                    bra       Default@@           ; continue

Addr2@@             jsr       BUFFARG
                    tst       COUNT
                    beq       Fail@@              ; jump if no address
                    ldx       SHFTREG             ; end address (addr2)
                    stx       PTR5
                    jsr       WSKIP
                    beq       Default@@

Fail@@              ldx       #MSG8               ; "bad argument"
                    jmp       OUTSTRG

; Boot routine

Default@@           ldb       #$FF                ; control character ($ff -> download)
                    bsr       BTSUB               ; set up SCI and send control char

; Initializes X as register pointer
; Download block

Loop@@              lda       ,y
                    sta       [SCDR,x             ; write to transmitter
                    brclr     [SCSR,x,Bit7.,*     ; wait for TDRE (was #$80)
                    cmpy      PTR5                ; if last...
                    beq       Done@@              ; ...quit
                    iny                           ; else...
                    bra       Loop@@              ; ...send next
Done@@              equ       :AnRTS

;*******************************************************************************
; Subroutine
; btsub   - sets up SCI and outputs control character
; On entry, B = control character
; On exit,  X = $1000
; A = $0C
;*******************************************************************************

BTSUB               proc
                    ldx       #REGS               ; to use indexed addressing

                    lda       #2
                    sta       [PORTD,x            ; drive transmitter line
                    sta       [DDRD,x             ; high

                    clr       [SCCR2,x            ; turn off XMTR and RCVR

                    lda       #$22                ; BAUD = /16
                    sta       [BAUD,x

                    lda       #$0C                ; TURN ON XMTR & RCVR
                    sta       [SCCR2,x

                    stb       [SCDR,x
                    brclr     [SCSR,x,Bit7.,*     ; wait for TDRE
;                   bra       TILDE

;*******************************************************************************
; TILDE - This command is put into the combuff by the load command so that
; extraneous carriage returns after the load will not hang up.

TILDE               proc
                    rts

;*******************************************************************************
; Purpose: Calculate the same CRC as that used by ASM11
; Input  : X -> First byte of block
;        : Y -> Last byte of block
;        : D = CRC_Seed
; Output : D = updated CRC_Seed
; Call   :          ldd       #CRC_SEED
;        :          ldx       #StartAddress
;        :          ldy       #EndAddress
;        :          jsr       GetAsmCRC

                    #spauto

GetAsmCRC           proc
                    pshx
                    pshy

                    pshy      .last@@
                    pshx      .first@@
                    pshd      my_crc@@
                    tsy

Loop@@              cmpx      .last@@,spy
                    bhi       Done@@

                    @cop                          ; in case of many iterations

                    lda       ,x
                    beq       Skip@@
                    cmpa      #$FF
                    beq       Skip@@

                    ldb       .first@@+1,spy
                    mul                           ; low address with data byte
                    addd      my_crc@@,spy
                    std       my_crc@@,spy

                    lda       ,x
                    ldb       .first@@,spy
                    mul                           ; high address with data byte
                    addb      my_crc@@,spy
                    stb       my_crc@@,spy

Skip@@              inx
                    stx       .first@@,spy
                    bra       Loop@@

Done@@              pull
                    puly
                    pulx
                    rts

;*******************************************************************************

                    #Push
                    #OptRelOff
                    #SEG0
                    org       ROM_END&$F000+$0F7C

SoftVec             macro
                    mreq      1
                    mdef      2,~1~
.~1~                exp       *
                    jmp       ~2~
                    endm

                    @softvec  WARMST,MAIN         ; warm start
                    @softvec  BPCLR               ; clear breakpoint table
                    @softvec  RPRINT              ; display user registers
                    @softvec  HEXBIN              ; convert ascii hex char to binary
                    @softvec  BUFFAR,BUFFARG      ; build hex argument from buffer
                    @softvec  TERMAR,TERMARG      ; read hex argument from terminal
                    @softvec  CHGBYT              ; modify memory at address in x
                    @softvec  READBU,READBUFF     ; read character from buffer
                    @softvec  INCBUF,INCBUFF      ; increment buffer pointer
                    @softvec  DECBUF,DECBUFF      ; decrement buffer pointer
                    @softvec  WSKIP               ; find non-whitespace char in buffer
                    @softvec  CHKABR,CHKABRT      ; check for abort from terminal
                    @softvec  UPCASE              ; convert to upper case
                    @softvec  WCHEK               ; check for white space
                    @softvec  DCHEK               ; check for delimeter
                    @softvec  INIT,ONSCI          ; initialize i/o device
                    @softvec  INPUT               ; low level input routine
                    @softvec  OUTPUT              ; low level output routine
                    @softvec  OUTLHL,OUTLHLF      ; display top 4 bits as hex digit
                    @softvec  OUTRHL,OUTRHLF      ; display bottom 4 bits as hex digit
                    @softvec  OUTA                ; output ascii character in A
                    @softvec  OUT1BY,OUT1BYT      ; display the hex value of byte at X
                    @softvec  OUT1BS,OUT1BSP      ; out1byt followed by space
                    @softvec  OUT2BS,OUT2BSP      ; display 2 hex bytes at x and a space
                    @softvec  OUTCRL,OUTCRLF      ; carriage return, line feed to terminal
                    @softvec  OUTSTR,OUTSTRG      ; display string at X (term with $04)
                    @softvec  OUTST0,OUTSTRG0     ; outstrg with no initial carr ret
                    @softvec  INCHAR              ; wait for and input a char from term
                    @softvec  VECINT,VECINIT      ; initialize RAM vector table

                    #Pull

;*******************************************************************************
                    #Export   BPCLR,UPCASE,RPRINT,HEXBIN,WRITE,EEWRIT,EEBYTE
                    #Export   EEBULK,DLY10MS,OUTPUT,OUTSCI,OUT1BYT,OUTCRLF
                    #Export   OUTSTRG,TABTO,INCHAR,BULK,BULKALL,Start,INSCI
;*******************************************************************************

?                   macro
                    mreq      1
                    mswap     1,:loop
          #ifdef    V~1~
                    org       V~1~
                    dw        J~1~
          #else
V~1~                dw        J~1~
          #endif
                    mtop      :loop+:{:loop+1}
                    endm

;*******************************************************************************
                    #VECTORS
;*******************************************************************************

                    @?        SCI,SPI
                    @?        PAIE,PAO
                    @?        TOF,TOC5,TOC4,TOC3,TOC2,TOC1,TIC3,TIC2,TIC1
                    @?        RTI
                    @?        IRQ,XIRQ
                    @?        SWI
                    @?        ILLOP,COP,CLM
                    dw        Start               ; Ignores the EEPROM boot option

                    end       :s19crc

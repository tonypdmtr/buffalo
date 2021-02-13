;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
;                     MONITOR PROGRAM FOR THE M68HC11
;                               version 1.0
;*******************************************************************************
; Description:    This program provides a controlled testing environment
;               for software development on a target 68HC11 microprocessor
;               It eliminates the need to reprogram the program under
;               testing into the eprom after minor changes.
;               It allows the user to alter the contents of a memory
;               location or the contents of the microprocessor's
;               accumulators and registers.
;                 The monitor adds no extra cost to the target system other
;               than the memory it occupies.  The main portion of the
;               monitor resides in the memory location specified by ROMS.
;               The variables used by the monitor is located in the memory
;               location specified by RAMS.  The values of RAMS and ROMS
;               can be alter to suit the user perference.
;                 The monitor communicates through the target system's
;               serial port to an external host computer (via RS-232).
;
; Features Available:   1) Load - Loads user's test program into specified
;                                 memory location.
;                       2) Run  - Run test program in memory.
;                       3) Call - Execute a JSR.
;                       4) Help - Display list of commands available.
;                       5) Break- Add/delete Breakpoints
;                       6) MD   - Display a block of memory.
;                       7) SS   - Single Step test program using interrupt.
;                       8) SS2  - Single Step test program using breakpoints
;                       9) Reg  - Display/modify CPU's registers.
;                      10) Fill - Alter the contents of a memory location.
;
; Memory Requirements:  4 kilobytes
;
; Other Requirements:     This monitor program does not require any external
;                       host program.  However, it requires a communication
;                       software program that could interface with the
;                       serial port of the host computer.  The monitor
;                       program configures the SCI port of the target
;                       system to a 9800 baud at a 8 MHz crystal (4800 baud
;                       for 4 MHz).  Also, the SCI character length is
;                       configure to:
;                               Start Bit:       1
;                               Data Bit :       8
;                               Parity   :       None
;                               Stop bit :       1
;
;
; PROGRAMMED BY:  KIN-HUNG LEUNG
;*******************************************************************************

;*******************************************************************************
;                            PROGRAM EQUATES
;*******************************************************************************
; RAM and ROM specifications
;*******************************************************************************
;RAMS               equ       $7000               ; Start of RAM
;IntRamEnd          equ       RAMS+$00FF          ; End Of internal RAM of M68HC11
;ROMS               equ       $7100               ; Start of ROM
RAMS                equ       $0000               ; Start of RAM
IntRamEnd           equ       RAMS+$00FF          ; End Of internal RAM of M68HC11
ROMS                equ       $E100               ; Start of ROM

;*******************************************************************************
; Port Addresses, Peripheral Devices
;*******************************************************************************

REGS                equ       $1000               ; Base Address of CPU Registers
Baud                equ       REGS+$2B            ; Baud register for SCI
SCCR1               equ       REGS+$2C            ; Control register 1 for SCI
SCCR2               equ       REGS+$2D            ; Control register 2 for SCI
SCSR                equ       REGS+$2E            ; Status register for SCI
SCDR                equ       REGS+$2F            ; Data register for SCI
TCNT                equ       REGS+$0E            ; Free running counter
TOC5                equ       REGS+$1E            ; Output compare register 5
TMSK1               equ       REGS+$22            ; Timer mask 1
TFLG1               equ       REGS+$23            ; Timer flag 1

;*******************************************************************************
; Stack Size of User and Monitor
;*******************************************************************************

UserStackSize       equ       50                  ; User stack size
MonStackSize        equ       30                  ; Monitor stack size

;*******************************************************************************
; Buffer Size of for the SCI
;*******************************************************************************

MaxBufSize          equ       50                  ; Size of input character buffer

;*******************************************************************************
; Maximum breakpoints available
;*******************************************************************************

MaxBreak            equ       4                   ; Maximum break point available

;*****************************************************************************
; Offsets to user's registers pointed by UserRegs
;*****************************************************************************

PC_OffSet           equ       0
Y_OffSet            equ       2
X_OffSet            equ       4
A_OffSet            equ       6
B_OffSet            equ       7
CCR_OffSet          equ       8
SP_OffSet           equ       9

;*****************************************************************************
; Miscellaneous equates
;*****************************************************************************

Null                equ       $00                 ; Null character
CR                  equ       $0D                 ; Hex code for carriage return code
LF                  equ       $0A                 ; Hex code for new line
Space               equ       $20                 ; Hex code for space
BackSpace           equ       $08                 ; Hex code for backspace
EOT                 equ       $04                 ; Hex code for end of Text code
CTRLC               equ       $03                 ; Hex code for control-c
; *JSWI            equ     $00F4           Jump vector for SWI interrupt
; *JTOC5           equ     $00D3           Jump vector for output compare 5
;                                        interrupt.
; *JSCI            equ     $00C4           Jump vecotr for SCI interrupt.
;*****************************************************************************
;                             RAM ALLOCATIONS
;*****************************************************************************

                    org       RAMS

;*****************************************************************************
; User Stack Area
;*****************************************************************************

                    rmb       UserStackSize       ; User stack area
UserStack           rmb       1                   ; Point to the top of user stack

;*****************************************************************************
; Monitor Stack Area
;*****************************************************************************

                    rmb       MonStackSize        ; Monitor stack area
MonStack            rmb       1                   ; Point to the top of Monitor stack

;*****************************************************************************
; Temporary storage for user registers and accumulators
;*****************************************************************************

UserRegs            rmb       11                  ; User registers: PC,Y,X,A,B,CCR,SP

;*****************************************************************************
; Flags, Counters and Temporary storage area for the monitor program
;*****************************************************************************

St_Length           rmb       1                   ; Counter for counting the number of
                                                  ; characters in St_Buffer
Ok                  rmb       1                   ; Flag to indicate a condition
Flag                rmb       1                   ; Flag to indicate a condition
Flag2               rmb       1                   ; Flag to indicate a condition
Error               rmb       1                   ; Flag to indicate a condition
Counter             rmb       1                   ; Number Counting
Temp                rmb       2                   ; Temporary storage for monitor vars
Temp2               rmb       2                   ; Temporary storage for address
Temp3               rmb       2                   ; Temporary storage for address

;*****************************************************************************
; Pointers used by the monitor
;*****************************************************************************

Command             rmb       2                   ; Storage for command address.
Ptr                 rmb       2                   ; String pointer.

;*****************************************************************************
; Miscellaneous variables used by the monitor
;*****************************************************************************

OffSet              rmb       2                   ; Loading Offset.
CheckSum            rmb       1                   ; Check sum counter.
FillValue           rmb       1                   ; The value to be filled in memory.
CtrlC_On            rmb       1                   ; Flag to indicate if the user want
                                                  ; to enable control break during
                                                  ; execution. 1=on 0=off

;*****************************************************************************
; Starting and ending address for memory dump
;*****************************************************************************
MDAddr1             rmb       2                   ; Starting address used by memory dump
                                                  ; routine.
MDAddr2             rmb       2                   ; Ending address used by memory dump.

;*****************************************************************************
; Breakpoint table
;*****************************************************************************

BreakTable          rmb       3*MaxBreak          ; User break point table
SS2Break            rmb       3*2                 ; Break point table for single stepping

;*****************************************************************************
; The size of buffer used for holding characters comming from the SCI
;*****************************************************************************

BufferSize          rmb       1                   ; Input buffer size.
St_Buffer           rmb       MaxBufSize          ; Input character buffer.

;*****************************************************************************
; Vector Jump Table
;*****************************************************************************

                    org       RAMS+$C4
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

;*****************************************************************************
; The Start of Monitor Program
;*****************************************************************************
; Description:  This is the main portion of the monitor program.  It consist*
;               of a loop that constantly reads in the command from the host*
;               and compare it with that in the command table.  If the
;               command exists, then it will execute the command.
; *---------------------------------------------------------------------------*
                    org       ROMS

Monitor             sei                           ; Disable all maskable interrupts.
                    lds       #MonStack           ; Set Stack pointer to top of Monitor
;                                       stack area.
                    lda       #$00                ; CLEAR OUTPUT PORT
                    sta       $1100

;*****************************************************************************
; SET UP LCD
;*****************************************************************************

                    ldx       #$FFFF              ; GIVE LCD TIME TO RESET
JKD                 dex
                    bne       JKD
                    bsr       CLRLCD              ; CLEAR AND SETUP LCD
                    ldx       #RESMES             ; PRINT -POWER ON RESET-
                    bsr       LCDTEXT
                    ldx       #$FFFF              ; DELAY
JKD1                dex
                    bne       JKD1
                    lda       #$01                ; CLEAR LCD
                    bsr       WCTRL
                    ldx       #HELLO              ; PRINT -KMON V1.0-
                    bsr       LCDTEXT
                    lda       #$C0                ; ADDRESS SECOND LINE OF LCD
                    bsr       WCTRL
                    ldx       #HELLO1             ; PRINT SECOND LINE
                    bsr       LCDTEXT
                    bra       JKSTART             ; START MONITOR PROGRAM

;****************************************************************************
; LCD SET UP
;****************************************************************************

CLRLCD              psha
                    lda       #$01
                    bsr       WCTRL               ; CLEAR LCD
                    lda       #$02
                    bsr       WCTRL               ; HOME CURSOR
                    lda       #$38
                    bsr       WCTRL               ; SET 8 BIT, 2 LINE, 5X7
                    lda       #$0C
                    bsr       WCTRL               ; DISPLAY ON, CURSOR OFF
                    lda       #$06
                    bsr       WCTRL               ; ENTRY MODE-INC ADDRESS, NO SHIFT
                    pula
                    rts

;*****************************************************************************
; SEND A LINE OF TEXT TO LCD
; PASS START ADDRESS OF TEXT IN X
; TEXT STRING TERMINATED BY $00 OR $04
; TEXT SHOULD NOT EXCEED DISPLAY LINE WIDTH
;*****************************************************************************

LCDTEXT             psha
LCD                 lda       0,X                 ; GET BYTE FROM TEXT STRING
                    cmpa      #$00                ; CHECK FOR END OF STRING
                    beq       DLCD
                    cmpa      #$04
                    beq       DLCD
                    bsr       WDAT                ; SEND CHARACTER TO LCD
                    inx                           ; GET NEXT
                    bra       LCD

DLCD                pula
                    rts

;*****************************************************************************
; OUTPUT CONTROL BYTE TO LCD
; CONTROL BYTE PASSED IN A
;*****************************************************************************

WCTRL               pshb
                    sta       $1400               ; WRITE BYTE TO LCD CONTROL REG
WCTRLL              ldb       $1400               ; TEST BUSY FLAG
                    andb      #$80
                    bne       WCTRLL
                    pulb
                    rts

;*****************************************************************************
; OUTPUT DATA BYTE TO LCD
; DATA BYTE PASSED IN A
;*****************************************************************************

WDAT                pshb
                    sta       $1401               ; WRITE BYTE TO LCD DATA REG
WDATL               ldb       $1400               ; TEST BUSY FLAG
                    andb      #$80
                    bne       WDATL
                    pulb
                    rts

;*****************************************************************************
; MESSAGES
;*****************************************************************************

RESMES              fcs       'POWER ON RESET'
HELLO               fcs       'KMON V1.0'
HELLO1              fcs       'HC11 MPP (C){:year}'

;*****************************************************************************
; Initialize Target System's SCI and monitor program variables.

JKSTART             bsr       Init_SCI
                    bsr       Init_UserVar
                    bsr       Init_MonVar
                    bsr       Init_Host

Main                bsr       Prompt              ; 'Command> '
                    jsr       GetString           ; Get string of characters from host.
                    tst       St_Length
                    beq       Main                ; Branch if string is empty.
                    jsr       SearchCom           ; Find command in command table.
                    tst       Ok
                    beq       Main                ; Branch if command not in table.
                    ldx       Command
                    jsr       ,x                  ; Execute command.
                    bra       Main

;*****************************************************************************
; Init_SCI()
;*****************************************************************************
; Description:  This routine sets up the SCI for 9800 baud @ 8 Mhz or
;               4800 baud @ 4 Mhz crystal.  All interrupts relating to the
;               SCI are disabled.
;
; Entry Condition:  None
;
; Exit Condition:  SCI setup for 9600 baud @ 8Mhz or 4800 baud @ 4Mhz
;                  cyrstal.
;
; Register(s) Changed:  Register A
; *---------------------------------------------------------------------------*

Init_SCI            lda       #%00110000
                    sta       Baud
                    lda       #%00000000
                    sta       SCCR1
                    lda       #%00001100
                    sta       SCCR2
                    rts

;*****************************************************************************
; Init_Host()
;*****************************************************************************
; Description:  This routine initialize the host by sending a string of
;               characters.
;
; Entry Condition:  None
;
; Exit Condition:  Initialization string sent to host.
;
; Register(s) Changed:  Register X
; *---------------------------------------------------------------------------*

Init_Host           ldx       #MSG1               ; '68HC11 Monitor Program'
                    jsr       OutString3
                    rts

;*****************************************************************************
; Prompt()
;*****************************************************************************
; Description:  Routine to display the command prompt on the host.
;
; Entry Condition:      None
;
; Exit Condition:       The prompt string sent to host
;
; Register(s) Changed:  None
; *---------------------------------------------------------------------------*

Prompt              pshx
                    jsr       NewLine
                    ldx       #MSG2               ; 'Command> '
                    jsr       OutString
                    pulx
                    rts

;*****************************************************************************
; Init_UserVar()
;*****************************************************************************
; Description:  Routine to initialize the user registers (A,B,X,Y,PC,CCR,SP)*
;               to a known state.  The accumulators A and B will be set to
;               zero.  Accumulators X, Y and SP will be pointing to the
;               top of the user stack area.  PC will hold the value of
;               $6000 and the CCR will contain the value of %11010000.
;
; Entry Condition:  None
;
; Exit Condition:  The Memory location containing the user registers is set
;                  to a known preset value.
;
; Register(s) changed:  Register D and X
; *---------------------------------------------------------------------------*

Init_UserVar        ldx       #UserRegs
                    ldd       #UserStack
                    std       X_OffSet,X          ; Set the user's x register value.
                    std       Y_OffSet,X          ; Set the user's y register value.
                    std       SP_OffSet,X         ; Set the user's stack pointer point
;                                       to the top the user's stack area.
                    ldd       #$6000
                    std       PC_OffSet,X         ; Set the user's PC to start at $6000.
                    ldd       #$D000
                    stb       A_OffSet,X          ; Set the user's A accumulator value.
                    stb       B_OffSet,X          ; Set the user's B accumulator value.
                    sta       CCR_OffSet,X        ; Set the user's CCR to $D000
;                                               I bit set
;                                               S bit set
;                                               X bit set
                    rts

;*****************************************************************************
; Init_MonVar()
;*****************************************************************************
; Description:  Routine to initialize the variables used by the Monitor
;               program.
;
; Entry Condition:      None
;
; Exit Condition:       The variables used by the Monitor is set to a known
;                       value.
;                               BufferSize = MaxBufSize.
;                               FillValue  = $FF
;                               MDAddr1    = $0000
;                               MDAddr2    = $0080
;                       The address value of the user and the monitor
;                       break point table are set to $FFFF (no entry)
;
; Register(s) Changed:  None
; *---------------------------------------------------------------------------*

Init_MonVar         pshx
                    pshd

; Setting maximum character buffer size to MaxBufSize
                    ldb       #MaxBufSize
                    stb       BufferSize

; Predefine value for fill routine
                    lda       #$FF
                    sta       FillValue

; Predefine the value for CtrlC_On to 0.
                    clr       CtrlC_On

; Predefine the starting and ending address for the memory dump routine
                    ldd       #$0000
                    std       MDAddr1
                    addb      #$80
                    std       MDAddr2

; Clear Break point table for single stepping.  The value $FFFF indicate
; no entry.
                    ldx       #SS2Break
                    ldd       #$FFFF
                    std       ,x
                    std       3,x

; Clear the user's break point table.  The value $FFFF indicate no entry.
                    ldx       #BreakTable
                    lda       #MaxBreak
Loop1               beq       EndLoop1
                    psha
                    ldd       #$FFFF
                    std       ,x
                    ldb       #3
                    abx
                    pula
                    deca
                    bra       Loop1

EndLoop1            puld
                    pulx
                    rts

;*****************************************************************************
; GetString
;*****************************************************************************
; Description:  Routine to get a string of characters from the host.  The
;               carriage return sent from the host indicates the end of
;               the string.
;
; Entry Condition:  None
;
; Exit Condition:  The St_Buffer contains a string of characters of
;                  St_Length long.  The string is terminated with a carriage*
;                  return.
;
; Register(s) Changed:  None
; *---------------------------------------------------------------------------*

GetString           pshx
                    pshy
                    psha
                    pshb
                    ldx       #St_Buffer          ; Points to the start of string buffer.
                    clrb                          ; Clear character counter.
StringLoop          jsr       GetChar2            ; Get character from host.
                    cmpb      BufferSize
                    bhs       StringError         ; Branch if character counter exceed
                                                  ; character buffer size.
                    cmpa      #BackSpace
                    bne       IncPointer          ; Branch if not backspace character.
DecPointer          tstb
                    beq       StringLoop          ; Branch if character counter is zero.
                    decb                          ; Decrement character counter.
                    dex                           ; Decrement string pointer.
                    jsr       OutChar
                    lda       #Space
                    jsr       OutChar             ; Delete character on host screen.
                    lda       #BackSpace
                    bra       SendCharOut

IncPointer          sta       ,x
                    cmpa      #$0D
                    beq       EndString           ; Branch if carriage return.
                    incb                          ; Increment character counter.
                    inx                           ; Increment string pointer.
SendCharOut         jsr       OutChar
                    bra       StringLoop

StringError         ldx       #MSG3               ; 'Command Too Long'
                    jsr       OutString3
                    clrb                          ; Reset character counter to zero.
EndString           stb       St_Length
                    pulb
                    pula
                    puly
                    pulx
                    rts

;*****************************************************************************
; SearchCom
;*****************************************************************************
; Description:  Routine to parse the command from the input string
;               buffer.  Any spaces in front of the command ill be
;               ignored.  The command will then be compared with the
;               commands in the command table.  If the command is not in
;               the command table the flag indicated by Ok is cleared.
;               Otherwise is set and the variable 'Command' contains the
;               starting address of the command routine.
;
; Entry Condition:      St_Buffer contains a carriage return terminated
;                       string.
;
; Exit Condition:       Variables:  Ok      - Cleared if command not found
;                                             otherwise set.
;                                   Command - Starting address of command
;                                             routine.
;                                   Ptr     - Address of the next field in
;                                             St_Buffer.
;
; Register(s) Changed:  None
; *---------------------------------------------------------------------------*

SearchCom           pshx
                    pshy
                    psha
                    clr       Ok                  ; Clear Ok flag.
                    ldy       #ComTable           ; Points to the start of the Command
;                                       table.
                    ldx       #St_Buffer          ; Points to the start of the String
;                                       Buffer.
                    jsr       SkipSpace           ; Remove leading spaces.
                    beq       SearchComEnd        ; Branch if only carriage return.
                    stx       Temp
SearchCom1          lda       ,x
                    jsr       UpCase              ; Convert to upper case.
                    cmpa      #' '
                    beq       SearchCom3          ; Branch if space.
                    cmpa      #CR
                    beq       SearchCom3          ; Branch if carriage return.
SearchCom2          cmpa      ,y
                    bne       SearchCom4          ; Branch if characters pointed by
;                                       x and y do not match.
                    iny
                    inx
                    bra       SearchCom1          ; Compare next characters.

SearchCom3          jsr       FindNull            ; Advance y pointer to a Null character
                    iny
                    ldy       ,y
                    sty       Command             ; Store command address.
                    inc       Ok
                    bra       SearchComEnd

SearchCom4          jsr       NextCom             ; Advance y pointer to next command in
;                                       command table.
                    bne       SearchError         ; Branch if end of table.
                    ldx       Temp                ; Reset x to beginning of command field.
                    bra       SearchCom1

SearchError         ldx       #MSG4               ; 'Bad Command'
                    jsr       OutString3
SearchComEnd        stx       Ptr
                    pula
                    puly
                    pulx
                    rts

;*****************************************************************************
; GetChar(var a: char)
;*****************************************************************************
; Description:  Routine to get a character from the SCI.  It first checks
;               the RDRF bit of the SCSR.  If it is not set then it loops
;               until it is set.  When a character is received, the MSB of
;               the character is removed.
;
; Entry Condition:      None
;
; Exit Condition:       Accumulator A contains the ASCII character sent by
;                       the host.
;
; Register(s) Changed:  None
; *---------------------------------------------------------------------------*

GetChar             proc
                    pshb
Loop@@              ldb       SCSR
                    bitb      #%00100000
                    beq       Loop@@              ; Branch if received data register full
                    lda       SCDR
                    anda      #$7F                ; Remove MSB
                    pulb
                    rts

;*****************************************************************************
; GetChar2(var a: char)
;*****************************************************************************
; Description:  This routine is an extension to the GetChar routine.  In
;               this routine, the escape sequence character for the arrow
;               keys sent by the host is removed.  The function key <F7>
;               is enabled as a shortkey for single stepping.
;
; Entry Condition:      None
;
; Exit Condition:       Accumulator A contains the ASCII character sent by
;                       the host.
;
; Register(s) Changed:  Accumulator A and B.
; *---------------------------------------------------------------------------*

GetChar2            bsr       GetChar
                    cmpa      #$1B                ; Escape Char
                    bne       :AnRTS              ; Branch if not an escape character
GetCharLoop2        bsr       GetChar

; Disabling all cursor keys
                    cmpa      #$5B
                    beq       GetCharLoop2
                    cmpa      #$5D
                    beq       GetCharLoop2        ; Disable Cursor Keys

; Enabling the <F7> key to activate single stepping using interrupts.
                    cmpa      #$71
                    bne       GetCharLoop3        ; Branch if not one of the escape
;                                       sequence character for <F7>.
                    ldx       #St_Buffer          ; Set pointer to point to the start
;                                       of the string buffer.
                    lda       SingleStep
                    sta       ,x                  ; writes the first character 's' to
;                                       the string buffer.
                    inx
                    lda       #CR                 ; Indicate a carriage return has been
;                                       pressed.
                    incb                          ; Increment character counter.
                    bra       :AnRTS

GetCharLoop3        cmpa      #$4F
                    beq       GetCharLoop2        ; Branch if one of the escape sequence
;                                       character of <F7>.

; Enabling the <F8> key to activate single stepping using breakpoints.
                    cmpa      #$72
                    bne       GetCharLoop4        ; Branch if not one of the escape
;                                       sequence character for <F8>.
                    ldx       #St_Buffer          ; Reset string pointer pointing to the
;                                       start of the string buffer.
                    pshb
                    ldd       SingleStep2
                    std       ,x                  ; Store the 'S2' in string buffer.
                    pulb
                    inx
                    inx
                    incb
                    incb                          ; Increment the character counter by 2.
                    lda       #CR                 ; Indicate a carriage return has been
;                                       pressed.
                    bra       :AnRTS

GetCharLoop4        cmpa      #$4F
                    beq       GetCharLoop2        ; Branch if one of the escape sequence
;                                       character for <F8>.

                    bra       GetChar2

;*****************************************************************************
; KeyPressed(var b: char)
;*****************************************************************************
; Description:  Routine to detect if a key was pressed on the host terminal.*
;               If a key is pressed, the Z bit in the CCR is cleared and
;               the ASCII hex character of the key is stored in accumulator
;               B.
;
; Entry Condition:      None
;
; Exit Condition:       Z bit in CCR is set if no key was pressed.
;                       Otherwise is cleared.  If a key is pressed, the
;                       ASCII hex character of the key is stored in
;                       accumulator B.
;
; Register(s) Changed:  Accumulator B
; *---------------------------------------------------------------------------*

KeyPressed          ldb       SCSR
                    bitb      #%00100000
                    beq       :AnRTS              ; Branch if received data register empty
                    ldb       SCDR
                    andb      #$7F                ; Remove MSB
                    rts

;*****************************************************************************
; OutChar(a: char)
;*****************************************************************************
; Description:  Routine to send a character in accumulator A to the host
;               through the SCI.
;
; Entry Condition:      Accumulator A contains the ASCII character to be
;                       sent through the SCI.
;
; Exit Condition:       Character sent through the SCI.
;
; Register(s) Changed:  None
; *---------------------------------------------------------------------------*

OutChar             pshb
OutCharLoop         ldb       SCSR
                    bitb      #%10000000
                    beq       OutCharLoop         ; Branch if transmit data register is
;                                       not empty.
                    sta       SCDR
                    pulb
                    rts

;*****************************************************************************
; OutSpace
;*****************************************************************************
; Description:  Routine to send 'n' space characters to the host through
;               the SCI.
;
; Entry Condition:      None.
;
; Exit Condition:       None.
;
; Register(s) Changed:  None
; *---------------------------------------------------------------------------*

OutSpace            psha
                    tsta
OutSpaceLoop        beq       EndOutSpace
                    psha
                    lda       #Space
                    bsr       OutChar
                    pula
                    deca
                    bra       OutSpaceLoop

EndOutSpace         pula
                    rts

;*****************************************************************************
; Out1Space, Out2Space
;*****************************************************************************
; Description:  Routine to send a space character (either 1,2 or n space) to*
;               the host through the SCI.
;
; Entry Condition:      None.
;
; Exit Condition:       None.
;
; Register(s) Changed:  None
; *---------------------------------------------------------------------------*

Out1Space           psha
                    lda       #1
                    bsr       OutSpace
                    pula
                    rts

Out2Space           bsr:2     Out1Space
                    rts

;*****************************************************************************
; OutString  (x: Pointer) -> Sends string of characters only.
; OutString2 (x: Pointer) -> Sends the string and the CR+LF.
; OutString3 (x: Pointer) -> Sends a CR+LF, String, CR+LF.
; OutString4 (x: Pointer) -> Sends a CR+LF and the string.
;*****************************************************************************
; Description:  Routine to send a string of characters pointed by register
;               X through the SCI.  The string of characters must be
;               terminated by a Null ($00) character.
;
; Entry Condition:      Register X contains the starting address of the
;                       string.
;
; Exit Condition:       The string of characters sent to through the SCI.
;
; Register(s) Changed:  Register X points to the end of the string plus 1.
; *---------------------------------------------------------------------------*

OutString           proc
                    psha
Loop@@              lda       ,x                  ; Load character into accumulator A.
                    beq       OutStringEnd        ; Branch if Null character.
                    bsr       OutChar
                    inx                           ; Next character.
                    bra       Loop@@

OutStringEnd        inx
                    pula
                    rts

OutString2          bsr       OutString           ; Send string through SCI
                    bsr       NewLine             ; Send CR+NL through SCI
                    rts

OutString3          bsr       NewLine             ; Send CR+NL through SCI
                    bsr       OutString2          ; Send string through SCI
                    rts

OutString4          bsr       NewLine             ; Send CR+NL through SCI
                    bsr       OutString
                    rts

;*****************************************************************************
; NewLine()
;*****************************************************************************
; Description:  Routine to send a carriage return and line feed code through*
;               the SCI.
;
; Entry Condition:      None
;
; Exit Condition:       Host performs a carriage return and line feed.
;
; Register(s) Changed:  None
; *---------------------------------------------------------------------------*

NewLine             psha
                    lda       #CR
                    bsr       OutChar
                    lda       #LF
                    bsr       OutChar
                    pula
                    rts

;*****************************************************************************
; SkipSpace (x : Pointer)
;*****************************************************************************
; Description:  Routine to move the character pointer (register x) through
;               the a string of characters until a non space ($20) character*
;               is reached.  If a carriage return is reached, the Z bit in
;               the CCR is set.
;
; Entry Condition:      Register x contains the address of the string.
;
; Exit Condition:       Register x contains the address of a non space
;                       character within the string.  The Z bit in the CCR
;                       is set if a carriage return character is reached.
;
; Register(s) Changed:  None.
; *---------------------------------------------------------------------------*

SkipSpace           proc
                    psha
Loop@@              lda       ,x
                    cmpa      #Space
                    bne       SkipSpaceEnd        ; Branch if not space ($20)
                    inx
                    bra       Loop@@

SkipSpaceEnd        cmpa      #CR                 ; Carriage return?
                    pula
                    rts

;*****************************************************************************

; NextWord (x : Pointer)

;*****************************************************************************

; Description:  Routine to move the word pointer (register x) to the next

;               word.  The next word is determine if a character is followed*

;               after a space. If a carriage return is reached, the Z bit in*

;               the CCR is set.

;

; Entry Condition:      Register x contains the address of the string.

;

; Exit Condition:       Register x contains the address of a the next word

;                       within the string.  The Z bit in the CCR

;                       is set if a carriage return character is reached.

;

; Register(s) Changed:  None.

; *---------------------------------------------------------------------------*

NextWord            proc
                    psha
Loop@@              lda       ,x
                    cmpa      #Space
                    beq       NextWordStart       ; Searching of a space character.
                    cmpa      #CR
                    beq       NextWordStart
                    inx                           ; Increment to next character.
                    bra       Loop@@

NextWordStart       bsr       SkipSpace           ; Point to next word.
                    pula
                    rts

;*****************************************************************************
; FindNull (y: Pointer)
;*****************************************************************************
; Description:  Routine to find a Null character pointed by register y.
;               The register y will increment to the next character if it
;               can't find a Null character.  However, if it finds a EOT
;               character the routine will terminate and an 'Error' flag is
;               set.
;
; Entry Condition:      Register y pointing to a Null terminated string.
;
; Exit Condition:       Register y points to the Null character.
;                       Variable Error is set if a EOT character is reached.*
;
; Register(s) Changed:  None.
; *---------------------------------------------------------------------------*

FindNull            proc
                    psha
Loop@@              lda       ,y
                    cmpa      #EOT
                    beq       FoundEOT            ; Branch if EOT character
                    cmpa      #Null
                    beq       FindNullEnd         ; Branch if Null character
                    iny                           ; Next character
                    bra       Loop@@

FindNullEnd         pula
                    rts

FoundEOT            inc       Error               ; Set error flag
                    pula
                    rts

;*****************************************************************************
; NextCom
;*****************************************************************************
; Description:  Routine to increment the pointer (register y) to the next
;               command string in the command table.  If the end of table,
;               is reached the Z bit in the CCR is cleared.  Otherwise set.
;
; Entry Condition:      Register y is pointing to the command string in the
;                       command table.
;
; Exit Condition:       Register y points to the next command string in the
;                       command table.  If end of table is reached, the
;                       Z-bit in the CCR is cleared.  Otherwise is set.
;
; Register(s) Changed:  The value in register y.
; *---------------------------------------------------------------------------*

NextCom             pshb
                    ldb       #3
                    clr       Error
                    bsr       FindNull
                    tst       Error
                    bne       NextComEnd          ; Branch if EOT
                    aby
NextComEnd          pulb
                    rts

;*****************************************************************************
; Get2HexBin
;*****************************************************************************
; Description:  Routine to extract two hex characters pointed by Ptr and
;               convert it to its binary equivalent.
;               example,  Hex characters -> 2A
;                         binary equivalent -> 00101010
;
; Entry Condition:      Memory location pointed by 'Ptr' contains the
;                       address of the first hex characters to be converted.*
;
; Exit Condition:       Register A contains the binary equivalent of the
;                       two hex characters.  If the characters are not in
;                       the range of 0..9,A..F the error flag 'Flag'
;                       will be set.  Otherwise is cleared.
;
; Register(s) Changed:  Register A and B
; *---------------------------------------------------------------------------*

Get2HexBin          pshx
                    clr       Counter             ; Clear Hex counter
                    ldx       Ptr                 ; Point to first hex character
                    lda       ,x
                    jsr       UpCase
                    inc       Counter             ; Increment hex counter
                    inx                           ; Point to next hex character
                    psha
                    lda       ,x
                    jsr       UpCase
                    tab
                    pula
                    inc       Counter             ; Increment hex counter
                    inx                           ; Point to next hex character
                    stx       Ptr
                    bsr       TwoHex2Bin          ; Convert to binary
                    tst       Flag
                    bne       Get2HexBinEnd       ; Branch if conversion error
                    ldb       ,x
                    cmpb      #Space
                    beq       Get2HexBinEnd
                    cmpb      #CR
                    beq       Get2HexBinEnd
                    inc       Counter             ; Increment hex counter
Get2HexBinEnd       pulx
                    rts

;*****************************************************************************
; Get4HexBin
;*****************************************************************************
; Description:  Routine to extract four hex characters pointed by Ptr and
;               convert it to its binary equivalent.
;               example,  Hex characters -> 2A8F
;                         binary equivalent -> 00101010 10001111
;
; Entry Condition:      Memory location pointed by 'Ptr' contains the
;                       address of the first hex characters to be converted.*
;
; Exit Condition:       Register A contains the binary equivalent of the
;                       first two hex characters.
;                       Register B contains the binary equivalent of the
;                       last two hex characters.
;                       If the characters are not in the range of 0..9,A..F
;                       the error flag 'Flag' will be set.  Otherwise is
;                       cleared.
;
; Register(s) Changed:  Register A and B
; *---------------------------------------------------------------------------*

Get4HexBin          pshx
                    bsr       Get2HexBin          ; Convert first two hex characters to
;                                       binary.
                    tst       Flag
                    bne       Get4HexBinError     ; Branch if conversion error.
                    ldb       Counter
                    cmpb      #3
                    bne       Get4HexBinError     ; Branch if no more hex character.
                    psha
                    bsr       Get2HexBin          ; Convert last two hex characters to
;                                       binary.
                    tab
                    pula
                    tst       Flag
                    bne       Get4HexBinError     ; Branch if conversion error.
                    psha
                    lda       Counter
                    cmpa      #3
                    pula
                    bne       Get4HexBinEnd       ; Branch if no more character.
Get4HexBinError     inc       Flag                ; Set error flag.
Get4HexBinEnd       pulx
                    rts

;*****************************************************************************
; Hex2Bin
;*****************************************************************************
; Description:  Routine to convert an ASCII hex character to binary.  The
;               variable 'Flag' is set if an invalid hex character is
;               encounter.  Otherwise is cleared.
;
; Entry Condition:      Accumulator A contains an ASCII hex character.
;
; Exit Condition:       Accumulator A the binary equivalent of the hex
;                       character.  Flag is set if encounter an invalid hex.*
;                       Otherwise is cleared.
;
; Register(s) Changed:  Accumulator A
; *---------------------------------------------------------------------------*

Hex2Bin             clr       Flag                ; Clear Flag
                    cmpa      #'0'
                    blo       Hex2BinError        ; Branch if lower
                    cmpa      #'9'
                    bls       UnderA              ; Branch if in '0'..'9'
                    cmpa      #'F'
                    bhi       Hex2BinError        ; Branch if higher
                    suba      #7
UnderA              suba      #'0'
                    clc
                    rts

Hex2BinError        inc       Flag                ; Set Flag
                    sec
                    rts

;*****************************************************************************
; TwoHex2Bin
;*****************************************************************************
; Description:  Routine to convert a two ASCII hex character stored in
;               accumulator D to binary.  If an error occurs, the variable
;               Flag is set.  Otherwise is cleared.
;
; Entry Condition:      Accumulator D contains the hex character.
;
; Exit Condition:       Accumulator A contains the binary equivalent of the
;                       hex characters.
;                       If error, Flag is set.  Otherwise is cleared.
;
; Register(s) Changed:  Register A and B.
; *---------------------------------------------------------------------------*

TwoHex2Bin          bsr       Hex2Bin             ; Convert first character.
                    tst       Flag
                    bne       HexError            ; Branch if conversion error.
                    lsla:4
                    sta       Temp
                    tba
                    bsr       Hex2Bin             ; Convert second character.
                    tst       Flag
                    bne       HexError            ; Branch if conversion error.
                    adda      Temp                ; Add the two character together.
                    clc
                    rts

HexError            sec
                    rts

;*****************************************************************************
; FourHex2TwoBin
;*****************************************************************************
; Description:  Routine to convert four ASCII hex character stored in
;               register x and y to two binary number.  If an error occurs,
;               the variable Flag is set.  Otherwise is cleared.
;
; Entry Condition:      Register X contains the first two hex characters.
;                       Register Y contains the last two hex characters.
;
; Exit Condition:       Accumulator D contains the binary equivalent of the
;                       hex characters.
;                       If error, Flag is set.  Otherwise is cleared.
;
; Register(s) Changed:  Register A and B.
; *---------------------------------------------------------------------------*

FourHex2TwoBin      xgdy
                    bsr       TwoHex2Bin          ; Convert the last two hex character.
                    tst       Flag
                    bne       :AnRTS              ; Branch if conversion error.
                    tab
                    pshb
                    xgdx
                    bsr       TwoHex2Bin          ; Convert the first two hex character.
                    pulb
                    rts

;*****************************************************************************
; ReadByte
;*****************************************************************************
; Description:  Routine to read two ASCII hex from the SCI and convert it to*
;               its binary equivalent.  If and error occurs, the 'Flag'
;               variable is set.  Otherwise is cleared.
;
; Entry Condition:      None
;
; Exit Condition:       Accumulator A contains the binary equivalent of the
;                       two hex characters.
;
; Register(s) Changed:  Accumulator A and B.
; *---------------------------------------------------------------------------*

ReadByte            jsr       GetChar             ; Get first character.
                    psha
                    jsr       GetChar             ; Get second character.
                    tab
                    pula
                    bra       TwoHex2Bin          ; Convert the two characters.

;*****************************************************************************
; Read2Byte
;*****************************************************************************
; Description:  Routine to read four ASCII hex from the SCI and convert it
;               to its binary equivalent.  If and error occurs, the 'Flag'
;               variable is set.  Otherwise is cleared.
;
; Entry Condition:      None
;
; Exit Condition:       Accumulator D contains the binary equivalent of the
;                       four hex characters.
;
; Register(s) Changed:  Accumulator A and B.
; *---------------------------------------------------------------------------*

Read2Byte           bsr       ReadByte            ; Get binary equivalent of the first
;                                       two characters.
                    psha
                    bsr       ReadByte            ; Get binary equivalent of the last
;                                       two characters.
                    tab
                    pula
                    rts

;*****************************************************************************
; FourBin2Hex
;*****************************************************************************
; Description:  Routine to convert a 4-bit binary number to an ASCII hex
;               character.
;
; Entry Condition:      Accumulator A contains a 4-bit binary number to be
;                       converted.
;
; Exit Condition:       Accumulator A contains a 8-bit ASCII hex character.
;
; Register(s) Changed:  Accumulator A.
; *---------------------------------------------------------------------------*

FourBin2Hex         anda      #$0F                ; Mask out the higher 4 bit.
                    ora       #$30                ; Convert to ASCII character.
                    cmpa      #$39
                    bls       :AnRTS              ; If ASCII character is not within
                                                  ; A..F then branch.
                    adda      #$07                ; Else need an adjustment of 7.
                    rts

;*****************************************************************************
; EightBin2Hex
;*****************************************************************************
; Description:  Routine to convert an 8-bit binary number to an ASCII hex
;               character.
;
; Entry Condition:      Accumulator A contains a 8-bit binary number to be
;                       converted.
;
; Exit Condition:       Accumulator D contains the ASCII hex character of
;                       the 8-bit binary number.
;
; Register(s) Changed:  Accumulator A & B
; *---------------------------------------------------------------------------*

EightBin2Hex        psha
                    bsr       FourBin2Hex         ; Convert lower four bits.
                    tab
                    pula
                    lsra:4
                    bsr       FourBin2Hex         ; Convert upper four bits.
                    rts

;*****************************************************************************
; Out8Bin2Hex
;*****************************************************************************
; Description:  Routine to send an 8-bit binary number to the host.
;
; Entry Condition:      Accumulator A contains the binary number to be sent
;                       to the host.
;
; Exit Condition:       The 8-bit binary number got converted to 2 byte
;                       ASCII hex character and got sent to the host.
;
; Register(s) Changed:  None.
; *---------------------------------------------------------------------------*

Out8Bin2Hex         pshd
                    bsr       EightBin2Hex
                    jsr       OutChar             ; Display the higher 4 bit binary number.
                    tba
                    jsr       OutChar             ; Display the lower 4 bit binary number.
                    puld
                    rts

;*****************************************************************************
; Out16Bin2Hex
;*****************************************************************************
; Description:  Routine to send a 16-bit binary number to the host.
;
; Entry Condition:      Accumulator D contains the binary number to be sent
;                       to the host.
;
; Exit Condition:       The 16-bit binary number got converted to 4 byte
;                       ASCII hex character and got sent to the host.
;
; Register(s) Changed:  Accumulator D.
; *---------------------------------------------------------------------------*

Out16Bin2Hex        pshb
                    bsr       Out8Bin2Hex         ; Output the higher 8 bits.
                    pula
                    bra       Out8Bin2Hex         ; Output the lower 8 bits.

;*****************************************************************************
; UpCase(a:char)
;*****************************************************************************
; Description:  Routine to conver the hex value in accumulator to its upper
;               case value.
;
; Entry Condition:      Accumulator A contains the ASCII hex character.
;
; Exit Condition:       Accumulator A contains the upper case value of a
;                       lower case ASCII hex character.
;
; Register(s) Changed:  Register A
; *---------------------------------------------------------------------------*

UpCase              cmpa      #'a'
                    blt       :AnRTS              ; Branch if < 'a'.
                    cmpa      #'z'
                    bgt       :AnRTS              ; Branch if > 'z'.
                    suba      #$20                ; Convert to upper case.
                    rts

;*****************************************************************************
; BadArgument
;*****************************************************************************
; Description:  Routine to display the string 'Bad Argument' on the host
;               terminal.
;
; Entry Condition:      None.
;
; Exit Condition:       Message sent to host.
;
; Register(s) Changed:  Register X
; *---------------------------------------------------------------------------*

BadArgument         ldx       #MSG7               ; 'Bad Argument'
                    jsr       OutString3
                    rts

;*****************************************************************************
; Command Table used by the Monitor program.
;*****************************************************************************
; Description:  This is a look up table used by the Monitor Program.  In
;               this look up table, the address location of the routine is
;               given after the Null character.  The string given in front
;               of the Null character is used by the monitor program to
;               search for the command.  The end of the look up table is
;               determined by the EOT character.
;
; Table format:
;               String
;               Null character
;               Memory location of routine
;               .
;               .
;               String
;               Null character
;               Memory location of routine
;               EOT
; *---------------------------------------------------------------------------*

?                   macro
                    mdef      2,~1~
                    fcs       \@~1~\@
                    dw        ~2~
                    endm

ComTable            @?        BREAK,Break
                    @?        CALL,Call
                    @?        FILL,Fill
                    @?        GO,Go
                    @?        HELP,Help
                    @?        ?,Help
                    @?        LOAD,Load
                    @?        MD,MD
                    @?        PB,PB
                    @?        REG,Reg
SingleStep          @?        SS
SingleStep2         @?        S2,SS2
                    fcb       EOT

;*****************************************************************************
; Load(Ptr)
;*****************************************************************************
; Usage:  Load <cr>          <-- loads the S-records from the host.
;
;         Load [+/-]<NUM> <cr>   <-- loads the S-records and offset it with
;                                    the value specified by <NUM>.
;
;         where <NUM> is any two byte hex number.
; *...........................................................................*
; Description:  Routine to load an S-record from the host to the specified
;               memory location of the target system.  The routine will
;               continuously read from the SCI until a valid S-record is
;               found.  The S-record must end with S9.  If not, it will
;               assume that more S-record is coming.  At the end of each
;               line of S-record is sent, a checksum is performed.  If the
;               resulting checksum does not equal FF, then the routine will
;               send an error message to the host.
;
; Entry Condition:      Variable Ptr points to the argument in St_Buffer.
;
; Exit Condition:       S-record load to target memory.
;
; Register(s) Changed:  Accumulator A,B
;                       Register X,Y
; *---------------------------------------------------------------------------*

Load                ldx       Ptr
                    clrb
                    clr       Ok
                    clr       OffSet
                    clr       OffSet+1            ; Initialize offset variable no offset.
                    jsr       SkipSpace
                    beq       NoArgument          ; Branch if no argument.
                    lda       ,x
                    cmpa      #'-'
                    beq       MinusOffSet         ; Branch if '-'
                    cmpa      #'+'
                    beq       PlusOffSet          ; Branch if '+'
                    bra       BadArgument

MinusOffSet         incb                          ; Flag to indicate minus offset.
PlusOffSet          inx
                    jsr       SkipSpace
                    beq       BadArgument         ; Oops. Invalid argument.

NoLoadError1        stx       Ptr
                    pshb
                    jsr       Get4HexBin          ; Get offset
                    std       OffSet
                    pulb
                    tst       Flag
                    beq       NoLoadError2        ; Branch if no invalid argument.
                    jmp       BadArgument         ; Oops. Invalid argument.

NoLoadError2        tstb
                    beq       NoArgument          ; Branch if it is a positive offset.
                    ldd       #$0000
                    subd      OffSet
                    std       OffSet              ; Make offset negative.
NoArgument          jsr       NewLine
                    clr       Error
LoadLoop1           jsr       GetChar
                    cmpa      #'S'
                    bne       LoadLoop1           ; Branch if not S-record.
                    jsr       GetChar
                    cmpa      #'9'
                    beq       LoadS9              ; Branch if S9-record.
                    cmpa      #'1'
                    bne       LoadLoop1           ; Branch if not S1-record.
                    clr       CheckSum
                    jsr       ReadByte            ; Read the number of data bytes.
                    sta       CheckSum            ; Add number to checksum.
                    suba      #2
                    tab
                    pshb
                    jsr       Read2Byte           ; Read the address.
                    psha
                    aba
                    adda      CheckSum
                    sta       CheckSum            ; Update checksum
                    pula
                    addd      OffSet
                    xgdx                          ; Offset memory pointer.
                    pulb
                    dex
LoadData            pshb
                    jsr       ReadByte            ; Read data.
                    pulb
                    psha
                    adda      CheckSum
                    sta       CheckSum            ; Update checksum.
                    pula
                    decb
                    beq       TestCheckSum        ; Branch if end of line.
                    inx                           ; Increment memory pointer.
                    sta       ,x                  ; Store data to memory.
                    sta       Temp
                    lda       ,x
                    cmpa      Temp                ; Check if written correctly.
                    beq       LoadData
                    tst       Error
                    bne       LoadData
                    stx       Temp2
                    inc       Error               ; Not written correctly. Must of
;                                       written into a ROM area.
                    bra       LoadData

LoadS9              jsr       ReadByte            ; Read the number of data bytes.
LoadS9Loop          psha
                    jsr       ReadByte            ; Read data.
                    pula
                    deca
                    bne       LoadS9Loop          ; Branch if not end of line.
                    tst       Error
                    beq       NoError
                    ldx       #MSG11              ; 'Memory Violation: '
                    jsr       OutString
                    jsr       Out2Space
                    ldd       Temp2
                    jsr       Out16Bin2Hex        ; Display the address where the memory
;                                       violation first occurs.
                    jsr       NewLine
NoError             ldx       #MSG5               ; 'Transmit Complete'
                    jsr       OutString2
                    rts

TestCheckSum        inc       CheckSum            ; Increment checksum.
                    bne       TestCheckSum1
                    jmp       LoadLoop1           ; Branch if chechsum is 0

TestCheckSum1       tst       Ok
                    beq       TestCheckSum2
                    jmp       LoadLoop1           ; Branch if not the first checksum
                                                  ; error.
TestCheckSum2       inc       Ok
                    pshx
                    ldx       #MSG6               ; 'Warning: Checksum Error'
                    jsr       OutString2
                    pulx
                    jmp       LoadLoop1

;*****************************************************************************
; Break
;*****************************************************************************
; Usage:    Break <cr>           <----  Display breakpoint table.
;
;           Break <address> <cr> <----  Set/clear breakpoint specified by
;                                       <address>.  Also, display breakpoint*
;                                       table.
;
;       where <address> is a 2 byte hex address.
; *...........................................................................*
; Description:  Routine to set or clear the breakpoint table and display
;               it to the host's monitor.  If no argument is given, then
;               only the breakpoint table is displayed.  The number of
;               breakpoints available is defined by MaxBreak.  If attempt
;               to add another breakpoint when the breakpoint table is full,*
;               the new breakpoint will be ignored.  The format of the
;               breakpoint table is as follow,
;
;               BreakTable      <address>
;                               <data>
;                               <address>
;                               <data>
;                               <address>
;                               <data>
;                               <address>
;                               <data>
;
;               where <address> is the location in memory to place the
;                               breakpoint.
;                     <data>    is the value stored in <address>.
;
; Entry Condition:      Variable Ptr points to the argument in St_Buffer.
;
; Exit Condition:       Breakpoint table modified and sent through the SCI
;
; Register(s) Changed:  Accumulators A,B
;                       Registers X,Y
; *---------------------------------------------------------------------------*

Break               ldx       Ptr
                    jsr       SkipSpace
                    beq       ShowTable           ; Branch if no argument.
                    stx       Ptr
                    jsr       Get4HexBin          ; Get argument (4 hex number)
                    tst       Flag
                    beq       NoBreakError        ; Branch if valid argument.
                    jmp       BadArgument         ; Oops, invalid argument.

NoBreakError        xgdy
                    ldx       #BreakTable         ; Locate the top of breakpoint table.
                    lda       #MaxBreak
BreakLoop           beq       NoBreak             ; Branch if end of breakpoint table.
                    cpy       ,x
                    beq       FoundBreak          ; Branch if break address already
;                                       exists.
                    ldb       #3
                    abx                           ; Point to next breakpoint entry.
                    deca
                    bra       BreakLoop

NoBreak             ldx       #BreakTable         ; Locate the top of breakpoint table.
                    lda       #MaxBreak
BreakLoop2          beq       BreakFull           ; Branch if end of breakpoint table.
                    psha
                    ldd       ,x
                    cpd       #$FFFF
                    pula
                    beq       FoundEmpty          ; Branch if found an empty slot ($FFFF)
;                                       in the breakpoint table
                    ldb       #3
                    abx                           ; Point to next breakpoint entry.
                    deca
                    bra       BreakLoop2

FoundEmpty          sty       ,x                  ; Store argument (<address>) into table
                    bra       BreakEnd

BreakFull           ldx       #MSG8               ; 'Break Table Full'
                    jsr       OutString3
                    bra       BreakEnd

                    rts

FoundBreak          ldd       #$FFFF
                    std       ,x                  ; Clear breakpoint specified by argument
                    ldx       #MSG9               ; 'Breakpoint removed'
                    jsr       OutString3

BreakEnd            nop

; Display break point table.

ShowTable           ldx       #MSG10              ; 'Breakpoint Table'
                    jsr       OutString3
                    ldx       #BreakTable         ; X points to top of breakpoint table.
                    lda       #MaxBreak
ShowLoop            beq       :AnRTS              ; Branch if end of table.
                    psha
                    ldd       ,x
                    cpd       #$FFFF
                    beq       Empty               ; Branch if empty slot.
                    jsr       Out16Bin2Hex        ; Output the breakpoint address.
                    jsr       NewLine
Empty               ldb       #3
                    abx                           ; Point to next breakpoint.
                    pula
                    deca
                    bra       ShowLoop

;*****************************************************************************
; Fill
;*****************************************************************************
; Usage:   Fill <addr1>                  <- Content specified by <addr1>
;                                           gets replace by previous <data>
;          Fill <addr1> <data>           <- Content specified by <addr1>
;                                           gets replace by <data>
;          Fill <addr1> <addr2>          <- Contents bounded by <addr1> to
;                                           <addr2> gets replace by previous*
;                                           <data>
;          Fill <addr1> <addr2> <data>   <- Contents bounded by <addr1> to
;                                           <addr2> gets
;
;       where <addr1>, <addr2> - memory location (four ascii hex characters)*
;             <data>           - value to replace with (two ascii hex
;                                                       characters)
; *...........................................................................*
; Description:  Routine to modify the content in the specified memory
;               location.  If two address argument is given, then a block
;               of memory content from <addr1> to <addr2> will be replace
;               by <value> or from the previous used value.  Otherwise, if
;               one address is given as an argument, only that address
;               content is modify.  The main fill routine uses two smaller
;               routine, "ByteFill" and "BlockFill".  This routine calls the*
;               other two routine by performing a 'jmp'.  This is not a
;               practical way of calling the routine, but it eliminate the
;               extra branching.
;
; Entry Condition:      Ptr pointing to the start of the argument stored in
;                       memory.
;
; Exit Condition:       Ptr pointing to the end of the argument.
;                       Memory content specified by <addr1> to <addr2> got
;                       replaced by a valid hex <value>
;
; Register changed:     All register value is changed.
; *---------------------------------------------------------------------------*

Fill                ldx       Ptr
                    jsr       SkipSpace
                    bne       CheckArgument       ; Branch if valid argument.
                    jmp       BadArgument         ; Invalid argument.

CheckArgument       stx       Ptr
                    jsr       Get4HexBin          ; Get first memory address.
                    tst       Flag
                    beq       Continue1           ; Branch if valid hex address.
                    jmp       BadArgument

Continue1           std       Temp2
                    ldx       Ptr
                    jsr       SkipSpace
                    bne       MoreArgument        ; Branch if more argument.
                    ldx       Temp2
                    lda       FillValue           ; Load value to be fill in ACC. A
                    bra       ByteFill            ; Fill one address location.

MoreArgument        stx       Ptr
                    stx       Temp3
                    jsr       Get4HexBin          ; Get second memory address.
                    tst       Flag
                    beq       GetData1            ; Branch if valid hex.
                    ldx       Temp3
                    stx       Ptr                 ; Point back to start of second
;                                       argument.
                    ldd       Temp2
GetData1            std       Temp3
                    ldx       Ptr
                    jsr       SkipSpace
                    beq       NoDataValue         ; Branch if no more argument.
                    stx       Ptr
                    jsr       Get2HexBin          ; Get data.
                    tst       Flag
                    beq       Continue2           ; Branch if valid data.
                    jmp       BadArgument

Continue2           ldb       Counter
                    cmpb      #3
                    bne       Continue3           ; Branch if only 2 hex number.
                    jmp       BadArgument

Continue3           sta       FillValue           ; Store <data> to FillValue.
NoDataValue         ldx       Temp2               ; X points to <addr1>
                    ldy       Temp3               ; Y points to <addr2>

          ;falls thru to BlockFill

;*****************************************************************************
; BlockFill
;*****************************************************************************
; Description:  Routine to fill a block of memory location with the value
;               in accumulator A.  The memory block is specified by the
;               registers X and Y.  The fill routine terminates if the value*
;               in register Y is greater than the value in register X.
;
; Entry Condition:      Accumulator A contains the value to be fill.
;                       Register X points to the start of the block.
;                       Register Y points to the end of the block.
;
; Exit Condition:       Register X points to the end of the block.
;                       The value in the other registers does not change.
;
; Register Charged:     Register X
; *---------------------------------------------------------------------------*

                    rts

BlockFill           lda       FillValue
                    sty       Temp2
BlockFillLoop       cpx       Temp2
                    bhi       :AnRTS              ; Branch if end of block.
                    bsr       ByteFill
                    inx
                    bra       BlockFillLoop

;*****************************************************************************
; ByteFill
;*****************************************************************************
; Description:  Routine to fill a memory location with the value in
;               accumulator A.  The memory to be fill is specified by the
;               register X.
;
; Entry Condition:      Accumulator A contains the value to be fill.
;                       Register X points to the start of the block.
;
; Exit Condition:       Content in accumulator A is stored in memory
;                       location pointed by register X.
;
; Register Changed:     None.
; *---------------------------------------------------------------------------*

ByteFill            sta       ,x
                    rts

;*****************************************************************************
; Reg
;*****************************************************************************
; Usage :   Reg                              <- Display User Registers
;           Reg <User Register> <data>       <- Replace the content
;                                               of the User Register
;                                               with the new value <data>
;
;       where <User Register> - X            <- X register
;                               Y            <- Y register
;                               A            <- A accumulator
;                               B            <- B accumulator
;                               C            <- CCR
;                               S            <- SP
;                               P            <- PC
;             <data> - is a 1 byte or 2 byte hex value.
;                      (depends on the <User Register> specified.)
; *...........................................................................*
; Description:  Routine to display and alter the content of the user
;               registers.  Bad argument occurs when a 2 byte data is given
;               to a 1 byte register, or vice versa.
;
; Entry Condition:      Ptr pointing to the start of the argument stored in
;                       memory.
;
; Exit Condition:       If there is no invalid argument then, the content
;                       of the user registers is either modified or not
;                       (depending on the choice of argument).  The whole
;                       list of user register is also sent to the terminal
;                       of the host.
;
; Register Changed:     All registers and accumulators.
; *---------------------------------------------------------------------------*

Reg                 ldx       Ptr
                    jsr       SkipSpace
                    beq       DisplayReg          ; If no argument then display registers
                    stx       Ptr
                    bsr       GetReg              ; Get type of register.
                    tst       Flag
                    beq       ValidReg            ; Branch if valid register.
                    jmp       BadArgument         ; Bad argument. Terminate reg routine.

ValidReg            jsr       SkipSpace
                    beq       DisplayReg          ; Branch if no data for register.
                    stx       Ptr
                    stb       Temp3
                    cmpb      #SP_OffSet
                    beq       Read4Hex            ; Branch, it is a 4 hex character.
                    cmpb      #A_OffSet
                    blo       Read4Hex            ; Branch, it is a 4 hex character.
                    jsr       Get2HexBin          ; Need a two hex character.
                    tst       Flag
                    beq       Valid2Hex           ; Branch if valid argument.
                    jmp       BadArgument         ; Oops. Invalid argument.

Valid2Hex           ldb       Temp3
                    ldx       #UserRegs
                    abx                           ; Point to user register.
                    sta       ,x                  ; Replace old value with new value.
                    bra       DisplayReg

Read4Hex            jsr       Get4HexBin
                    tst       Flag
                    beq       Valid4Hex           ; Branch if it is a valid 4 character
;                                       hex.
                    jmp       BadArgument         ; Oops. Invalid argument.

Valid4Hex           pshb
                    ldb       Temp3
                    ldx       #UserRegs
                    abx                           ; Point to user register.
                    pulb
                    std       ,x                  ; Replace old value with new value.
DisplayReg          jsr       PrintReg
                    rts

;*****************************************************************************
; GetReg (Ptr)
;*****************************************************************************
; Description:  This routine is used by the routine 'Reg'.  It determines
;               the user register to be modified.  It checks for the first
;               letter in the argument string and compares it with its
;               constant.  If there is no match then the Flag is set.
;               Otherwise is cleared.
;
; Entry Condition:      Ptr pointing to the first character to the word.
;
; Exit Condition:       Accumulator B contains the offset to the user
;                       register.  The Flag is set if the first character
;                       is an invalid character.  Otherwise is clear.
;
; Register Changed:     Accumulator B.
; *---------------------------------------------------------------------------*

GetReg              clr       Flag                ; Clear valid register flag.
                    clrb
                    ldx       Ptr
                    lda       ,x
                    jsr       UpCase
AccA                cmpa      #'A'                ; Is it accumulator A?
                    bne       AccB
                    ldb       #A_OffSet
AccB                cmpa      #'B'                ; Is it accumulator B?
                    bne       RegX
                    ldb       #B_OffSet
RegX                cmpa      #'X'                ; Is it register X?
                    bne       RegY
                    ldb       #X_OffSet
RegY                cmpa      #'Y'                ; Is it register Y?
                    bne       RegCCR
                    ldb       #Y_OffSet
RegCCR              cmpa      #'C'                ; Is it CCR?
                    bne       RegSP
                    ldb       #CCR_OffSet
RegSP               cmpa      #'S'                ; Is it stack pointer?
                    bne       RegPC
                    ldb       #SP_OffSet
RegPC               cmpa      #'P'                ; How about program counter?
                    bne       InvalidReg
                    ldb       #PC_OffSet
                    bra       EndGetReg

InvalidReg          tstb
                    bne       EndGetReg
                    inc       Flag                ; Oops. It is not a register or
;                                       accumulator.
EndGetReg           jsr       NextWord
                    stx       Ptr                 ; Advance to next argument.
                    rts

;*****************************************************************************
; MD - Memory Dump
;*****************************************************************************
; Usage:        MD                    <- Performs a memory dump (128 bytes)
;                                        starting at the PREVIOUS
;                                        <addr1>.
;               MD <addr1>            <- Performs a memory dump (128 bytes)
;                                        starting at <addr1>.
;               MD <addr1> <addr2>    <- Performs a memory dump starting at
;                                        <addr1> to <addr2>.
;       where <addr1> and <addr2> are 2 bytes address. (4 ascii hex char.)
; *...........................................................................*
; Description:  Routine to display the content of the specified memory
;               location.  If no starting address is specified, it will
;               display the first 128 bytes starting at the previous
;               address specified.  If two address is specified, the block
;               of memory bounded by <addr1> to <addr2> will be displayed.
;               If the value of <addr1> is greater than the value of
;               <addr2>, the values of the two address will be swapped.
;
; Entry Condition:      Ptr pointing to the start of the argument.
;
; Exit Condition:       128 bytes of memory or block of memory bounded by
;                       <addr1> to <addr2> is sent to the host terminal
;
; Register Changed:     All registers and accumulators.
; *---------------------------------------------------------------------------*

MD                  jsr       NewLine
                    ldx       Ptr
                    jsr       SkipSpace
                    beq       MD1                 ; Branch if no argument.
                    stx       Ptr
                    jsr       Get4HexBin          ; Get the first address argument.
                    tst       Flag
                    beq       SetUpStartAddr      ; Branch if valid argument (Flag = 0)
                    jmp       BadArgument         ; Oops. Invalid argument.

SetUpStartAddr      andb      #$F0
                    std       MDAddr1             ; Mask out the lower four bit.
                    ldx       Ptr
                    jsr       SkipSpace
                    beq       MD1                 ; Branch if no more argument.
                    stx       Ptr
                    jsr       Get4HexBin          ; Get the ending address argument.
                    tst       Flag
                    beq       SetUpEndAddr        ; Branch if valid address (Flag = 0).
                    jmp       BadArgument         ; Oops. Invalid argument.

SetUpEndAddr        andb      #$F0                ; Mask out the lower four bit.
; Set the starting and ending address in order.
                    cpd       MDAddr1
                    bhi       MDAddrOK
                    ldx       MDAddr1
                    xgdx
                    stx       MDAddr1
MDAddrOK            std       MDAddr2
                    bra       MD2

; Calculate the ending address for one or no argument value.

MD1                 ldx       MDAddr1
                    ldb       #$80
                    abx
                    stx       MDAddr2
; The core portion of memory dump.
MD2                 ldx       MDAddr1
                    bsr       DisplayHeader       ; Display header across the top.
                    jsr       NewLine
MDLoop              stx       Temp2
                    jsr       KeyPressed
                    beq       NoKeyPressed
                    cmpb      #CTRLC
                    beq       :AnRTS              ; If the user pressed <ctrl-C> then
;                                       branch.
NoKeyPressed        xgdx
                    jsr       Out16Bin2Hex        ; Output the address.
                    jsr       Out2Space
                    ldx       Temp2
                    bsr       MDBytes             ; Output 16 bytes of memory content.
                    jsr       Out2Space
                    bsr       MDChar              ; Output the character representation
                                                  ; of the 16 bytes of memory content.
                    jsr       NewLine
                    cpx       #$FFF0
                    beq       :AnRTS              ; Is it top of memory? Is so, branch.
                    cpx       MDAddr2
                    beq       :AnRTS              ; Is it end of memory dump. Is so
                                                  ; branch.
                    ldb       #$10
                    abx                           ; Increment address by 16 bytes.
                    bra       MDLoop

;*****************************************************************************
; DisplayHeader()
;*****************************************************************************
; Description:  This routine is used by the MD routine.  It is used to
;               the header at the beginning of the memory dump.  This allows*
;               easy reading of the content in the memory.
;
; Entry Condition:  None.
;
; Exit Condition:   Hex value from 0 to F is sent to the host terminal
;
; Register(s) Changed:  Accumulator A.
; *---------------------------------------------------------------------------*

DisplayHeader       lda       #6
                    jsr       OutSpace            ; Output 6 space to the host terminal.
                    lda       #$00                ; Set initial number to Zero.
DisplayHeaderL      jsr       Out8Bin2Hex         ; Output number to host terminal.
                    jsr       Out1Space
                    inca                          ; Increment number.
                    cmpa      #$10
                    bne       DisplayHeaderL      ; Sent $0F ? If not branch.
                    rts

;*****************************************************************************
; MDBytes (X:ptr)
;*****************************************************************************
; Description:  Routine to display the first 16 bytes of memory content to
;               the host terminal pointed by register X.
;
; Entry Condition:  Register X points to the start of the 16 bytes of memory*
;                   to by display.
;
; Exit Condition:   The first 16 bytes of memory content pointed by X sent
;                   to the host.
;
; Register Changed:  Accumulator B.
; *---------------------------------------------------------------------------*

MDBytes             pshx
                    psha
                    ldb       #$10
MDBytesLoop         lda       ,x
                    jsr       Out8Bin2Hex         ; Display the memory content pointed
;                                       by X.
                    jsr       Out1Space
                    inx                           ; Increment to next memory location.
                    decb
                    bne       MDBytesLoop         ; If 16 bytes of memory had been display
;                                       then branch.
                    pula
                    pulx
                    rts

;*****************************************************************************
; MDChar
;*****************************************************************************
; Description:  Routine to display the first 16 bytes of printable
;               character in memory to the host terminal pointed by
;               register X.
;
; Entry Condition:  Register X points to the start of the 16 bytes of memory*
;                   to by display.
;
; Exit Condition:   The first 16 bytes of memory content pointed by X sent
;                   to the host.
;
; Register Changed:  Accumulator B.
; *---------------------------------------------------------------------------*

MDChar              pshx
                    psha
                    ldb       #$10
MDCharLoop          lda       ,x
                    cmpa      #Space
                    bhs       Test1
                    lda       #Space              ; Oops. Not a printable character.
;                                       Replace it with a space.
Test1               cmpa      #'~'
                    bls       PrintableChar
                    lda       #Space              ; Oops. Not a printable character.
;                                       Replace it with a space.
PrintableChar       jsr       OutChar
                    inx                           ; Increment to next memory location.
                    decb
                    bne       MDCharLoop          ; 16 bytes of memory displayed? If not
;                                       branch.
                    pula
                    pulx
                    rts

;*****************************************************************************
; Call
;*****************************************************************************
; Usage         Call          <-- Run test program starting at address
;                                 specified by user's program counter.
;               Call <Addr>   <-- Run test program starting at address
;                                 specified by <Addr>.
;
;    where <Addr> is 2 byte ASCII hex value.
; *...........................................................................*
; Description:  Routine to run the test program in memory at full speed.
;               Note that this routine is different from the RUN routine.
;               In the RUN routine, termination of the test program is done
;               by SWI.  However, in this routine the RTS and the SWI opcode*
;               will terminate the test program.  When the RTS is
;               encountered, control is handed back to the monitor program.
;               The I bit in the condition code register is not affected.
;               If an interrupt occurs the interrupt will be served by the
;               user's interrupt service routine.
;               The method of setting up the user's stack is the same as
;               the RUN routine.  The user's registers and accumulators are
;               all pushed onto the stack before the call for RTI.  However,*
;               before any pushing is done, the address of the CALLRTS is
;               placed onto the user's stack first.  So, when RTS is
;               encountered the monitor can regain control.
;               WARNING:  The interrupt vector for the SWI is used by this
;                         routine.  Any attempt to alter the vector by
;                         the test program will inhibit this monitor program*
;                         from regaining control after SWI.  Therefore, a
;                         reset is needed to restart the monitor program.
;
; Entry Condition:      Ptr is pointing to the first argument in memory.
;
; Exit Condition:       Termination of the user's test program.
;
; Register(s) Changed:  All registers and accumulators.
; *---------------------------------------------------------------------------*

Call                ldx       Ptr
                    jsr       SkipSpace
                    beq       Call1
                    stx       Ptr
                    jsr       Get4HexBin          ; Get address.
                    tst       Flag
                    beq       CallArgOk
                    jmp       BadArgument         ; Invalid address

CallArgOk           std       UserRegs+PC_OffSet  ; Place address into user PC.
Call1               ldx       UserRegs+SP_OffSet
                    ldd       #CallRTS
                    dex
                    std       ,x                  ; Place address of CallRTS
;                                               routine into user stack.
                    dex
                    stx       UserRegs+SP_OffSet
                    jsr       SetUpBreak          ; Setup breakpoints
                    ldx       #SWI_ISR
                    jsr       SetUpSWI            ; Setup softvector for the
;                                               SWI interrupt.
                    jsr       RunUser             ; Execute user program.
                    rts

;*****************************************************************************
; CALLRTS()
;*****************************************************************************
; Description:  This routine is a portion of the CALL routine.  When the
;               opcode RTS is encountered in the running program, control
;               is handed to this routine.  The user's registers and
;               accumulators are all saved and printed to the host's
;               terminal.  However, the user program counter is not saved.
;
; Entry Condition:      Called by the RTS instruction.
;
; Exit Condition:       User's registers and accumulators are all saved and
;                       sent to the host computer.
;
; Register(s) Changed:  All registers and accumulators.
; *---------------------------------------------------------------------------*

CallRTS             psha
                    tpa
                    sta       UserRegs+CCR_OffSet  ; Save CCR.
                    pula
                    sta       UserRegs+A_OffSet   ; Save acc. A.
                    stb       UserRegs+B_OffSet   ; Save acc. B.
                    sts       UserRegs+SP_OffSet  ; Save SP.
                    ldx       Ptr
                    txs                           ; Set Stack pointer to point
;                                               monitor stack area.
                    jsr       RemoveBreak
                    jsr       PrintReg
                    rts

;*****************************************************************************
; SS()
;*****************************************************************************
; Usage:        SS              Performs single stepping starting at user
;                               program counter.
;               SS <addr>       Performs single stepping starting at the
;                               address specified by <addr>.
;
;       where <addr> is a 2 byte ASCII hex value
; *...........................................................................*
; Description:  Routine to single step through a test program in memory.
;               This routine uses the interrupt method to perform the
;               single stepping process.  It uses the output compare 5
;               of the M68HC11 processor.  The value of the output compare
;               is set in a way that the interrupt occurs after one
;               instruction.  After each instruction, the user registers
;               and accumulators are displayed to the host terminal.
;               WARNING:  The interrupt vectors for the SWI and the OC5 is
;                         used by this routine.  Any attempt in altering
;                         the content of these vectors by the running
;                         program will inhibit the monitor program from
;                         regaining control.  Therefore a reset in required
;                         to restart the monitor program.
;
; Entry Condition:      None.
;
; Exit Condition:       Termination of one instruction from the running
;                       program.
;
; Register Changed:     All registers and Accumulators.
;
; *---------------------------------------------------------------------------*

SS                  ldx       Ptr
                    jsr       SkipSpace
                    beq       NoSSArgument
                    stx       Ptr
                    jsr       Get4HexBin          ; Get start address if any.
                    tst       Flag
                    beq       NoSSError
                    jmp       BadArgument         ; Invalid address.

NoSSError           std       UserRegs            ; Saves argument in user PC
NoSSArgument        bsr       SetUpSoftVector
                    bsr       SetUpUserInt        ; Clear I bit in user CCR.
                    bsr       SetUpTimer          ; Initialize the output compare of the
;                                       free running counter.
                    jsr       RunUser
                    rts

;*****************************************************************************
; SeUpUserInt()
;*****************************************************************************
; Description:  Used by SS routine to clear the I bit in the User's
;               Condition Code Register to allow interrupt for the output
;               compare.
;
; Entry Condition:      None.
;
; Exit Condition:       I bit in User's CCR is cleared.
;                       If bit is already cleared, the Flag variable is set.*
;                       Otherwise is cleared.
;
; Register(s) Changed:  Accumulator A.
; *---------------------------------------------------------------------------*

SetUpUserInt        clr       Flag2               ; Clear flag to indicate that
;                                               the I bit in user CCR is set.
                    lda       UserRegs+CCR_OffSet
                    bita      #%00010000
                    bne       NotSet
                    inc       Flag2               ; I bit in user CCR is already
;                                               set.
NotSet              anda      #%11101111
                    sta       UserRegs+CCR_OffSet
                    rts

;*****************************************************************************
; SetUpSoftVector()
;*****************************************************************************
; Description:  Used by the SS routine to setup the interrupt vector for the*
;               output compare and the SWI.
;
; Entry Condition:      None.
;
; Exit Condition:       Address of the service routine for the output
;                       compare and the SWI is stored into their appropriate*
;                       location in the interrupt vector table.
;
; Register(s) Changed:  Register X and Accumulator A
; *---------------------------------------------------------------------------*

SetUpSoftVector     lda       #$7E                ; JMP opcode.
                    sta       JTOC5
                    ldx       JTOC5+1
                    stx       Temp2
                    ldx       #SS_ISR
                    stx       JTOC5+1             ; Setup jmp vector for output compare 5
                    ldx       #SS_ISR
                    jsr       SetUpSWI            ; Setup jmp vector for swi.
                    rts

;*****************************************************************************
; SetUpTimer()
;*****************************************************************************
; Description:  Used by the SS routine to setup and enable the output
;               compare 5 to interrupt.  The value added to the output
;               compare register 5 is determined by adding all the cycles
;               required the execution of the program being run.  In this
;               case, it requires 126 cycles from the start of SetUpTimer
;               to the execution of the program being run.
;
; Entry Condition:      None.
;
; Exit Condition:       Output Compare 5 is set to interrupt.
;
; Register(s) Changed:  Accumulators A and B.
; *---------------------------------------------------------------------------*

SetUpTimer          ldd       TCNT
                    addd      #126
                    std       TOC5
                    lda       #%00001000
                    sta       TFLG1               ; Clear output compare 5 flag.
                    sta       TMSK1               ; Enable interrupt from TOC5.
                    rts

;*****************************************************************************
; SS_ISR()
;*****************************************************************************
; Description:  Interrupt Service Routine for the SS routine.  This routine
;               is invoke by the interrupt from the output compare register
;               5.  Control is handed back to the monitor program and all
;               user's registers and accumulators are saved and printed
;               to the host terminal.
;
; Entry Condition:      Interrupt from the Output Compare 5.
;
; Exit Condition:       Monitor program regains control.
;                       User's registers and accumulators are saved and
;                       displayed to the host.
; *---------------------------------------------------------------------------*

SS_ISR              anda      #%11110111
                    sta       TMSK1               ; Inhibit output compare interrupt.
                    lda       #%00001000
                    sta       TFLG1               ; Clear OC5F flag.
                    tsx
                    ldy       Ptr
                    tys                           ; Set stack point to point to monitor
;                                       stack area.
                    lda       TMSK1
                    ldd       Temp2
                    std       JTOC5+1             ; Restore vector for output
;                                               compare 5.
                    ldd       Temp
                    std       JSWI+1              ; Restore vector for swi.
                    jsr       SaveUserStack
                    ldd       UserRegs+PC_OffSet
                    addd      #1
                    std       UserRegs+PC_OffSet  ; Locate the next opcode in
;                                               memory.
                    tst       Flag2
                    bne       PrintUserReg
                    lda       UserRegs+CCR_OffSet
                    ora       #%00010000          ; Restore I bit in user CCR.
                    sta       UserRegs+CCR_OffSet
PrintUserReg        jsr       PrintReg
                    rts

;*****************************************************************************
; SS2()
;*****************************************************************************
; Usage:        S2         <--  Performs single stepping starting at user
;                               program counter.
;               S2 <addr>  <--  Performs single stepping starting at the
;                               address specified by <addr>.
;
;       where <addr> is a 2 byte ASCII hex value.
; *...........................................................................*
; Description:  This routine performs a similar task as the SS routine.
;               Instead, this routine uses SWI breakpoints rather than
;               interrupt from the output compare.  The advantage of this
;               routine is that it allows the user to use all the output
;               compare.  If this routine is used to single step, all the
;               break points set by the user are inhibited.
;
; Entry Condition:      Ptr points to the argument (in any).
;
; Exit Condition:       Termination of one instruction of the running
;                       program.
;
; Register(s) Change:   All registers and accumulators.
; *---------------------------------------------------------------------------*

SS2                 ldx       Ptr
                    jsr       SkipSpace
                    beq       NoSS2Argument
                    stx       Ptr
                    jsr       Get4HexBin          ; Get starting address.
                    tst       Flag
                    beq       NoSS2Error
                    jmp       BadArgument         ; Invalid address.

NoSS2Error          std       UserRegs            ; Saves address in user PC
NoSS2Argument       ldx       #SS2_ISR
                    jsr       SetUpSWI            ; Setup softvector to SS2_ISR.
                    bsr       SetUpSS2Break       ; Setup SWI in the appropriate location.
                    jsr       SWI_at_start
                    jsr       RunUser
                    rts

;*****************************************************************************
; SetUpSS2Break ()
;*****************************************************************************
; Description:  This routine is used by the SS2 routine.  It setup the
;               breakpoints at a specific location in memory defined by the
;               addressing mode of the next opcode.
;
; Entry Condition:      None.
;
; Exit Condition:       The opcode SWI is place in memory where only one
;                       instruction is allowed to execute.
;
; Register(s) Changed:  All registers and accumulators.
; *---------------------------------------------------------------------------*

SetUpSS2Break       ldx       UserRegs+PC_OffSet
                    bsr       RegY_Adjustment
                    stx       Temp3               ; Make a copy of the user PC.
                    lda       ,x                  ; Opcode at user PC.
Not_Y               bsr       LinearBreakAddr
                    jsr       GetBranchBreak
                    jsr       SetSS2Break         ; Place SWI in appropriate
                                                  ; location.
                    rts

;*****************************************************************************
; RegY_Adjustment()
;*****************************************************************************
; Description:  This routine is used by the SetUpSS2Break.  It increment
;               the value in X (user PC.) if the opcode pointed by X is a
;               y indexing opcode.  In the y indexing mode, there is an
;               extra byte at the beginning, say $18.  This enables the CPU
;               to distinguish between x or y indexing mode.
;
; Entry Condition:      Register X contains the user PC value.
;
; Exit Condition:       If it is a y indexing mode the user PC in register
;                       x is incremented by 1.
;
; Register(s) Changed:  All registers and accumulators.
; *---------------------------------------------------------------------------*

RegY_Adjustment     psha
                    clr       Flag                ; Assume not y indexing mode.
                    lda       ,x
                    cmpa      #$18                ; Is it $18 ?
                    beq       NeedAdjustment
                    cmpa      #$CD                ; Is it $CD ?
                    beq       NeedAdjustment
                    cmpa      #$1A                ; Is it $1A ?
                    beq       NeedAdjustment
                    bra       NoAdjustment        ; Not y indexing mode.

NeedAdjustment      inx
                    inc       Flag                ; Ok, it is y indexing mode.
NoAdjustment        pula
                    rts

;*****************************************************************************
; SS2_ISR()
;*****************************************************************************
; Description:  This is the interrupt service routine used by the SS2
;               routine.  Control is handed back to the monitor program
;               after exiting this routine.
;               The user's registers and accumulators are saved and printed
;               to the host terminal.
;
; Entry Condition:      Interrupt due to SWI.
;
; Exit Condition:       Control is handed back to the monitor program.
;                       User's registers and accumulators are saved and
;                       printed to the host terminal.
; *---------------------------------------------------------------------------*

SS2_ISR             tsx
                    ldy       Ptr
                    tys                           ; Set stack pointer to monitor stack
                                                  ; area.
                    ldd       Temp
                    std       JSWI+1              ; Restore softvector.
                    jsr       SaveUserStack
                    jsr       RemoveSS2Break
                    jsr       PrintReg
                    rts

;*****************************************************************************
; LinearBreakAddr()
;*****************************************************************************
; Description:  This routine is used by the SetUpSS2Break to determine
;               where to place the SWI.  This routine is not responsible
;               for any branching or jumping.  It is responsible for
;               locating the next opcode linearly down the program.
;               The way this routine knows how many byte to skip is
;               determined by the lookup tables.
;
; Entry Condition:      Accumulator A contains the opcode of the current
;                       instruction.
;
; Exit Condition:       Address of the next instruction is placed into
;                       the breakpoint table.
;
; Register(s) Changed:  All registers and accululators.
; *---------------------------------------------------------------------------*

LinearBreakAddr     sta       Temp2               ; Saves a copy of the opcode in memory.
                    lsra:4                        ; Mask Lower 4 bits
                    psha
                    ldx       #Higher4Bit         ; Use main lookup table.
                    tab
                    abx
                    ldb       ,x                  ; Get offset to next opcode address.
                    pula

          ; Checking odd cases of opcode.

                    cmpa      #$01
                    bne       CheckNext
                    ldy       #OffSetTable1       ; Use lookup table 1.
                    bra       AddressAdjust

CheckNext           cmpa      #$08
                    bne       CheckNext2
                    ldy       #OffSetTable8       ; Use lookup table 8.
                    bra       AddressAdjust

CheckNext2          ldy       #OffSetTableC       ; Use lookup table C.
                    cmpa      #$0C
                    bne       EndGetLinear

AddressAdjust       lda       Temp2
                    anda      #$0F
                    sta       Temp2               ; Mask out higher 4 bit of opcode.

; Adjustment loop for odd cases of opcode.

AdjustLoop          lda       ,y
                    cmpa      #$00
                    beq       EndGetLinear
                    cmpa      Temp2
                    beq       FoundAdjustment
                    iny
                    iny
                    bra       AdjustLoop

; Read Adjustment value for odd cases of opcode.

FoundAdjustment     iny
                    lda       ,y
                    aba
                    tab

; Save address value to breakpoint table.

EndGetLinear        ldx       UserRegs+PC_OffSet
                    bsr       RegY_Adjustment
                    abx
                    ldy       #SS2Break
                    stx       ,y
                    rts

;*****************************************************************************
; GetBranchBreak()
;*****************************************************************************
; Description:  This routine is used by the SetUpSS2Break routine.  It
;               determines where to place the breakpoints for the jumping,
;               branching, RTS and RTI instruction.  When the address is
;               found, it stores it in the second entry of the breakpoint
;               table.
;
; Entry Condition:      Registers X contains the user PC.
;
; Exit Condition:       Predicted address is stored in the second entry
;                       of the breakpoint table.
;
; Register(s) Changed:  All registers and accumulators.
; *---------------------------------------------------------------------------*

?                   macro     CheckSubroutine
          #if :index <= 5
                    bsr       ~1~                 ; Checking Branch opcode.
          #else
                    jsr       ~1~                 ; Checking Branch opcode.
          #endif
                    bcs       StoreAddress
                    endm

GetBranchBreak      ldx       Temp3
                    lda       ,x

                    @?        CheckBranchOp       ; Checking Branch opcode.
                    @?        CheckJmpOpcode      ; Checking Jmp opcode.
                    @?        CheckRTSOpcode      ; Checking RTS opcode.
                    @?        CheckBrclrOp        ; Checking Brclr opcode.
                    @?        CheckBrsetOp        ; Checking Brset opcode.
                    @?        CheckJsrOpcode      ; Checking JSR opcode.
                    @?        CheckRtiOpcode      ; Checking RTI opcode.

                    bra       :AnRTS

StoreAddress        ldy       #SS2Break
                    stx       3,y                 ; Save address in table.
                    rts

;*****************************************************************************
; CheckBranchOp()
;*****************************************************************************
; Description:  This routine is used by the GetBranchBreak routine.  It
;               is responsible for checking the branch opcode.  If a branch
;               opcode is encountered, it will determine where it will
;               branch.
;
; Entry Condition:      Accumulator A contains the current opcode.
;
; Exit Condition:       If a branch opcode is encountered, the predicted
;                       branch address is saved in register X.  The C-bit
;                       in the CCR is set.  Otherwise is cleared.
;
; Register(s) Changed:  Register X and accumulator A
; *---------------------------------------------------------------------------*

CheckBranchOp       cmpa      #$80                ; BSR opcode
                    beq       BSR_Opcode
                    cmpa      #$20
                    blo       NotBranchOpcode     ; If opcode is below $20 then
                                                  ; it is not a branch opcode.
                    cmpa      #$2F
                    bls       BSR_Opcode          ; If opcode is above $2F then

NotBranchOpcode     clc
                    rts
                                                  ; it is not a branch opcode.
BSR_Opcode          ldd       Temp3
                    addb      1,x
                    addb      #$02
                    xgdx                          ; Determining the branching address.
                    sec
                    bra       :AnRTS

;*****************************************************************************
; CheckJmpOpcode()
;*****************************************************************************
; Description:  This routine is used by the GetBranchBreak routine.  It
;               is responsible for checking the jmp opcode.  If a jmp
;               opcode is encountered, it will determine where it will
;               jump.
;
; Entry Condition:      Accumulator A contains the current opcode.
;
; Exit Condition:       If a jmp opcode is encountered, the predicted
;                       address is saved in register X.  The C-bit
;                       in the CCR is set.  Otherwise is cleared.
;
; Register(s) Changed:  Register X and accumulator A
; *---------------------------------------------------------------------------*

CheckJmpOpcode      cmpa      #$7E                ; Extended mode opcode.
                    bne       CheckJmpIndex       ; If not extended mode, then check
;                                       index mode.
                    ldx       1,x
                    sec
                    bra       :AnRTS

CheckJmpIndex       cmpa      #$6E                ; Index mode opcode.
                    bne       NotJmpOpcode
                    ldb       1,x
                    tst       Flag
                    bne       Y_Index
                    ldx       UserRegs+X_OffSet   ; X indexing mode.
                    bra       AdjustJmpAddr

Y_Index             ldx       UserRegs+Y_OffSet   ; Y indexing mode.
AdjustJmpAddr       abx
                    sec
                    bra       :AnRTS

NotJmpOpcode        clc
                    rts

;*****************************************************************************
; CheckRTSOpcode()
;*****************************************************************************
; Description:  This routine is used by the GetBranchBreak routine.  It
;               is responsible for checking the RTS opcode.  If the RTS
;               opcode is encountered, it will determine where it will
;               branch.
;
; Entry Condition:      Accumulator A contains the current opcode.
;
; Exit Condition:       If the RTS opcode is encountered, the predicted
;                       return address is saved in register X.  The C-bit
;                       in the CCR is set.  Otherwise is cleared.
;
; Register(s) Changed:  Register X and accumulator A
; *---------------------------------------------------------------------------*

CheckRTSOpcode      cmpa      #$39                ; Opcode for RTS
                    bne       NotRTSOpcode
                    ldd       UserRegs+SP_OffSet
                    addd      #1
                    xgdx
                    ldx       ,x                  ; Get return address.
                    sec
                    bra       :AnRTS

NotRTSOpcode        clc
                    rts

;*****************************************************************************
; CheckBrclrOp()
;*****************************************************************************
; Description:  This routine is used by the GetBranchBreak routine.  It
;               is responsible for checking the BRCLR opcode.  If the BRCLR
;               opcode is encountered, it will determine where it will
;               branch.
;
; Entry Condition:      Accumulator A contains the current opcode.
;
; Exit Condition:       If the BRCLR opcode is encountered, the predicted
;                       branch address is saved in register X.  The C-bit
;                       in the CCR is set.  Otherwise is cleared.
;
; Register(s) Changed:  Register X and accumulator A
; *---------------------------------------------------------------------------*

CheckBrclrOp        cmpa      #$13                ; Direct mode for BRCLR.
                    beq       BrclrOpcode
                    cmpa      #$1F                ; Index mode for BRCLR.
                    bne       NotBrclrOpcode
BrclrOpcode         ldd       Temp3
                    addb      3,x
                    addb      #4
                    xgdx                          ; Determining the branch address.
                    sec
                    bra       :AnRTS

NotBrclrOpcode      clc
                    rts

;*****************************************************************************
; CheckBrsetOp()
;*****************************************************************************
; Description:  This routine is used by the GetBranchBreak routine.  It
;               is responsible for checking the BRSET opcode.  If the BRSET
;               opcode is encountered, it will determine where it will
;               branch.
;
; Entry Condition:      Accumulator A contains the current opcode.
;
; Exit Condition:       If a BRSET opcode is encountered, the predicted
;                       branch address is saved in register X.  The C-bit
;                       in the CCR is set.  Otherwise is cleared.
;
; Register(s) Changed:  Register X and accumulator A
; *---------------------------------------------------------------------------*

CheckBrsetOp        cmpa      #$12                ; Direct mode for BRSET.
                    beq       BrsetDirect
                    cmpa      #$1E                ; Index mode for BRSET.
                    bne       NotBrset
BrsetDirect         ldd       Temp3
                    addb      3,x
                    addb      #4
                    xgdx                          ; Determining the branch address.
                    sec
                    bra       :AnRTS

NotBrset            clc
                    rts

;*****************************************************************************
; CheckJsrOpcode()
;*****************************************************************************
; Description:  This routine is used by the GetBranchBreak routine.  It
;               is responsible for checking the JSR opcode.  If the JSR
;               opcode is encountered, it will determine where it will
;               jump.
;
; Entry Condition:      Accumulator A contains the current opcode.
;
; Exit Condition:       If the JSR opcode is encountered, the predicted
;                       address is saved in register X.  The C-bit
;                       in the CCR is set.  Otherwise is cleared.
;
; Register(s) Changed:  Register X and accumulator A
; *---------------------------------------------------------------------------*

CheckJsrOpcode      cmpa      #$9D                ; Direct mode for JSR.
                    beq       JsrDirect
                    cmpa      #$BD                ; Extended mode for JSR.
                    beq       JsrExtended
                    cmpa      #$AD                ; Index mode for JSR.
                    bne       NotJsrOpcode
                    ldb       1,x
                    tst       Flag
                    bne       JsrY_Index
                    ldx       UserRegs+X_OffSet   ; X indexing mode.
                    bra       JsrIndexAdjust

JsrY_Index          ldx       UserRegs+Y_OffSet   ; Y indexing mode.
JsrIndexAdjust      abx                           ; Address for indexing mode.
                    bra       SetCBit

JsrExtended         ldx       1,x                 ; Address for Extended mode.
                    bra       SetCBit

JsrDirect           ldd       Temp3
                    addb      1,x
                    xgdx                          ; Address for Direct mode.
SetCBit             sec
                    bra       :AnRTS

NotJsrOpcode        clc
                    rts

;*****************************************************************************
; CheckRtiOpcode()
;*****************************************************************************
; Description:  This routine is used by the GetBranchBreak routine.  It
;               is responsible for checking the RTI opcode.  If the RTI
;               opcode is encountered, it will determine the return address.*
;
; Entry Condition:      Accumulator A contains the current opcode.
;
; Exit Condition:       If the RTI opcode is encountered, the predicted
;                       address is saved in register X.  The C-bit
;                       in the CCR is set.  Otherwise is cleared.
;
; Register(s) Changed:  Register X and accumulator A
; *---------------------------------------------------------------------------*

CheckRtiOpcode      cmpa      #$3B                ; Opcode for the RTI.
                    bne       NotRtiOpcode
                    ldx       UserRegs+SP_OffSet
                    ldx       8,x                 ; Get return address from user
;                                               stack area.
                    sec
                    bra       :AnRTS

NotRtiOpcode        clc
                    rts

;*****************************************************************************
; SetSS2Break()
;*****************************************************************************
; Description:  This routine is used by the SetUpSS2Break.  This routine
;               sets up the breakpoint in the user program.  It goes through*
;               the breakpoint table for the SS2.  If an entry is found,
;               i.e. address is not $FFFF, then the SWI opcode is swapped
;               with the current opcode pointed by the address in the table.*
;
; Entry Condition:      None.
;
; Exit Condition:       The SWI opcode is placed in the user program.
;
; Register(s) Changed:  Register Y ,accumulator A and B
; *---------------------------------------------------------------------------*

SetSS2Break         ldy       #SS2Break
                    lda       #2
SS2BreakLoop        beq       :AnRTS
                    ldx       ,y
                    cpx       #$FFFF
                    beq       NoSS2Break          ; Branch if no entry. (i.e. $FFFF)
                    ldb       ,x
                    stb       2,y                 ; Make a copy of the opcode.
                    ldb       #$3F
                    stb       ,x                  ; Replace opcode with SWI opcode.
NoSS2Break          ldb       #3
                    aby                           ; Next breakpoint entry.
                    deca
                    bra       SS2BreakLoop

;*****************************************************************************
; RemoveSS2Break()
;*****************************************************************************
; Description:  This routine is used by the SS2_ISR.  It is responsible for
;               removing all the breakpoints setup in the user program by
;               SetSS2Break routine.
;
; Entry Condition:      None.
;
; Exit Condition:       All breakpoints are removed.
;
; Register(s) Changed:  All registers and accumulators.
; *---------------------------------------------------------------------------*

RemoveSS2Break      ldy       #SS2Break
                    lda       #2
RemoveBreakL        beq       :AnRTS
                    ldx       ,y
                    cpx       #$FFFF
                    beq       SkipBreak
                    ldb       2,y                 ; Get saved opcode.
                    stb       ,x                  ; Store saved opcode back into program.
                    ldx       #$FFFF
                    stx       ,y
SkipBreak           ldb       #3
                    aby                           ; Next breakpoint entry.
                    deca
                    bra       RemoveBreakL

;*****************************************************************************
; Higher4Bit
;*****************************************************************************
; Description:  This is a lookup table for the upper 4 bit of all opcode.
;               It gives the base offset of the predicted address.
;               For opcode with a upper 4 bit of 1, 8, and C requires an
;               additional lookup table.
; *---------------------------------------------------------------------------*

Higher4Bit          fcb       1                   ; for 0
                    fcb       1                   ; 1
                    fcb       2                   ; 2
                    fcb       1                   ; 3
                    fcb       1                   ; 4
                    fcb       1                   ; 5
                    fcb       2                   ; 6
                    fcb       3                   ; 7
                    fcb       2                   ; 8
                    fcb       2                   ; 9
                    fcb       2                   ; A
                    fcb       3                   ; B
                    fcb       2                   ; C
                    fcb       2                   ; D
                    fcb       2                   ; E
                    fcb       3                   ; F

;*****************************************************************************
; OffSetTable1
;*****************************************************************************
; Desription:   This lookup table is for the odd cases where the upper 4
;               bit is 1.  The first value is the value of the lower 4 bit.
;               The second value is the additional offset required.
;               The end of this table is indicated by a Null Character.
; *---------------------------------------------------------------------------*

OffSetTable1        fcb       2                   ; Lower 4 Bit Value.
                    fcb       3                   ; Additional offset.
                    fcb       3                   ; Lower 4 Bit Value.
                    fcb       3                   ; Additional offset.
                    fcb       4                   ; Lower 4 Bit Value.
                    fcb       2                   ; Additional offset.
                    fcb       5                   ; .
                    fcb       2                   ; .
                    fcb       $C                  ; .
                    fcb       2                   ; .
                    fcb       $D
                    fcb       2
                    fcb       $E
                    fcb       3
                    fcb       $F
                    fcb       3
                    fcb       0                   ; End of table.

;*****************************************************************************
; OffsetTable8
;*****************************************************************************
; Desription:   This lookup table is for the odd cases where the upper 4
;               bit is 8.  The first value is the value of the lower 4 bit.
;               The second value is the additional offset required.
;               The end of this table is indicated by a Null Character.
; *---------------------------------------------------------------------------*

OffSetTable8        fcb       3                   ; Lower 4 Bit Value.
                    fcb       1                   ; Additional offset.
                    fcb       $C                  ; Lower 4 Bit Value.
                    fcb       1                   ; Additional offset.
                    fcb       $D                  ; .
                    fcb       -1                  ; .
                    fcb       $E                  ; .
                    fcb       1                   ; .
                    fcb       $F
                    fcb       -1
                    fcb       0                   ; End of table.

;*****************************************************************************
; OffSetTableC
;*****************************************************************************
; Desription:   This lookup table is for the odd cases where the upper 4
;               bit is C.  The first value is the value of the lower 4 bit.
;               The second value is the additional offset required.
;               The end of this table is indicated by a Null Character.
; *---------------------------------------------------------------------------*

OffSetTableC        fcb       3                   ; Lower 4 Bit Value.
                    fcb       1                   ; Additional offset.
                    fcb       $C                  ; Lower 4 Bit Value.
                    fcb       1                   ; Additional offset.
                    fcb       $E                  ; .
                    fcb       1                   ; .
                    fcb       $F                  ; .
                    fcb       -1                  ; .
                    fcb       0                   ; End of Table.

;*****************************************************************************
; Help()
;
; Usage:   Help  <-- Displays the list of commands available and usage.
;          ?     <-- Same as Help.
; ..........................................................................*
; Description:  Routine to display the help string to the host terminal.
;               The help string contains all the commands available and
;               what are the arguments required.
;
; Entry Condition:      None.
;
; Exit Condition:       A string of characters terminated by a null
;                       character is sent to the host terminal.
;
; Register(s) Changed:  All registers and Accumulators.
; *---------------------------------------------------------------------------*

Help                ldx       #HelpString
                    jsr       OutString
                    rts

;*****************************************************************************
; Go(ptr)
;
; Usage:    Go <cr>           <----  Run test program starting at address
;                                    specified by the user PC.
;
;           Go <address> <cr> <----  Run test program starting at the
;                                    specified <address>.
;
;       where <address> is a 2 byte hex address.
;
; Description:  Routine to run the test program in memory at full speed.
;               The user's registers and accumulators are all pushed onto
;               the stack before the call for RTI.  Termination of the test
;               program is done by a reset or by break points, ie. SWI.
;               Once a SWI is encountered, the monitor regains control.
;               WARNING:  The interrupt vector for the SWI is used by this
;                         routine.  Any attempt to alter the vector by
;                         the test program will inhibit this monitor program
;                         from regaining control after SWI.  Therefore, a
;                         reset is needed to restart the monitor program.
;
; Entry Condition:      Ptr is pointing to the first argument in memory.
;
; Exit Condition:       Termination of the user's test program.
;
; Register(s) Changed:  All registers and accumulators.
;
; *----------------------------------------------------------------------------

Go                  ldx       Ptr
                    jsr       SkipSpace
                    beq       NoGoArgument
                    stx       Ptr
                    jsr       Get4HexBin          ; Get starting address.
                    tst       Flag
                    beq       NoGoError
                    jmp       BadArgument         ; Oops. Bad starting address.

NoGoError           std       UserRegs            ; Saves address in user PC
NoGoArgument        bsr       SWI_at_start
                    bsr       SetUpBreak
                    ldx       #SWI_ISR
                    jsr       SetUpSWI
                    bsr       SetCtrlC
                    jsr       RunUser
                    rts

;*****************************************************************************
; SetCtrlC
;*****************************************************************************
; Description:  Routine to enable the SCI interrupt of reveiving mode.
;               It also clear the I-bit in the user CCR and set the SCI
;               softvector.
; Entry Condition:      None.
; Exit Condition:       The above described in the description is performed.*
; Register Changed:     All registers, accumulators, and I-bit in user CCR.
; *---------------------------------------------------------------------------*

SetCtrlC            tst       CtrlC_On
                    beq       :AnRTS
                    clr       Temp3
                    lda       SCCR2
                    bita      #%00100000
                    beq       NotEnable
                    inc       Temp3
NotEnable           ora       #%00100000
                    sta       SCCR2               ; Enable receive interrupt.
                    bsr       SetSCIVector
                    jsr       SetUpUserInt
                    rts

;*****************************************************************************
; SetSCIVector
;*****************************************************************************
; Description:  Routine to setup the SCI vector in the RAM.
; Entry Condition:      None.
; Exit Condition:       SCI vector points the SCI_ISR.
; *----------------------------------------------------------------------------

SetSCIVector        lda       #$7E
                    sta       JSCI
                    ldd       JSCI+1
                    std       Temp2
                    ldd       #SCI_ISR
                    std       JSCI+1
                    rts

;*****************************************************************************
; SCI_ISR
;*****************************************************************************
; Description:  This is an interrupt service routine for the SCI interrupt.
;               It checks if the user pressed the Ctrl-C key.  If not, it
;               continues on with the user program.  If so, the user program
;               is terminated.  The SCI interrupt is disabled and the user
;               CCR is return to normal.  The user registers are displayed
;               on the host terminal.
; Entry Condition:      Interrupt from the SCI.
; Exit Condition:       If Ctrl-C is pressed, user CCR is return to original,
;                       SCI interrupt is disabled and user registers are sent
;                       to the host.
;                       If no Ctrl-C is pressed, the above is not perform.
; *----------------------------------------------------------------------------

SCI_ISR             lda       SCSR
                    lda       SCDR
                    cmpa      #CTRLC
                    bne       NotCtrlC
                    ldd       Temp2
                    std       JSCI+1              ; Restore SCI interrupt vector.
                    tst       Flag2
                    bne       DisableSCI
                    lda       UserRegs+CCR_OffSet
                    ora       #%00010000          ; Restore I bit in user CCR.
                    sta       UserRegs+CCR_OffSet
DisableSCI          tst       Temp3
                    bne       RIE_Enable
                    lda       SCCR2
                    anda      #%11011111
                    sta       SCCR2               ; Disable SCI interrupt.

          ; Adjust user program counter to next opcode.

                    tsx
                    ldd       7,x
                    addd      #1                  ; Increment user PC.
                    std       7,x
                    txs
RIE_Enable          bra       SWI_ISR

NotCtrlC            rti

;*****************************************************************************
; SWI_at_start
;*****************************************************************************
; Description:  This routine is used by the Go routine.  It is responsible
;               for checking if the starting opcode is a SWI.  If it is,
;               then it will advance to the next opcode.
;
; Entry Condition:      None.
;
; Exit Condition:       The user PC is advanced to the next opcode if the
;                       PC is pointing a SWI instruction.
; *---------------------------------------------------------------------------*

SWI_at_start        ldx       UserRegs+PC_OffSet
                    lda       ,x                  ; Get current opcode.
                    cmpa      #$3F                ; SWI opcode ?
                    bne       :AnRTS
                    inx                           ; Yes. Advance to next opcode.
                    stx       UserRegs+PC_OffSet
                    rts

;*****************************************************************************
; SetUpBreak
;*****************************************************************************
; Description:  This routine is used by the Go routine.  This routine
;               sets up the breakpoint in the user program.  It goes through*
;               the user breakpoint table.  If an entry is found, i.e.
;               address is not $FFFF, then the SWI opcode is swapped
;               with the current opcode pointed by the address in the table.*
;
; Entry Condition:      None.
;
; Exit Condition:       The SWI opcode is placed in the user program.
;
; Register(s) Changed:  Register Y ,accumulator A and B
; *---------------------------------------------------------------------------*

SetUpBreak          ldx       #BreakTable         ; Locate beginning of table.
                    ldb       #3
                    lda       #MaxBreak
SetUpBreakLoop      tsta
                    beq       :AnRTS
                    ldy       ,x
                    cpy       #$FFFF              ; Check for entry in table.
                    beq       NextBreak
                    psha
                    lda       ,y
                    sta       2,x                 ; Make a copy of user opcode.
                    cpy       UserRegs+PC_OffSet
                    beq       CannotReplace
                    lda       #$3F                ; SWI opcode.
                    sta       ,y                  ; Replace user opcode with SWI.
CannotReplace       pula
NextBreak           abx
                    deca
                    bra       SetUpBreakLoop

;*****************************************************************************
; SetUpSWI()
;
; Description:  Routine to setup the soft vector for the SWI vector.  The
;               previous value of the vector is stored in the variable
;               TEMP.
;
; Entry Condition:      X points to the interrupt service routine for SWI.
;
; Exit Condition:       The address of the interrupt service routine
;                       pointed by register X is placed in the soft vector.
;
; Register(s) Changed:  Register X and Accumulator A.
; *---------------------------------------------------------------------------*

SetUpSWI            lda       #$7E
                    sta       JSWI                ; Write a JMP instruction into vector.
                    ldd       JSWI+1
                    std       Temp
                    stx       JSWI+1              ; Write the address of the service
                                                  ; routine into vector.
                    rts

;*****************************************************************************
; RunUser
;*****************************************************************************
; Description:  This routine is used by the Go routine.  It is responsible
;               for stacking all the user accumulators and registers into
;               the user stack.  It then performs an RTI instruction to
;               run the user program.
;
; Entry Condition:      None.
;
; Exit Condition:       Control is given to the user program.
; *---------------------------------------------------------------------------*

RunUser             tsx
                    stx       Ptr                 ; Make of copy of the monitor SP.
                    ldy       #UserRegs
                    ldd       SP_OffSet,y
                    addd      #1
                    xgdx
                    txs                           ; SP now points to user stack area.

          ; Pushing all necessary registers and accumulators into the user stack.

                    ldx       PC_OffSet,y
                    pshx
                    ldx       Y_OffSet,y
                    pshx
                    ldx       X_OffSet,y
                    pshx
                    lda       A_OffSet,y
                    psha
                    lda       B_OffSet,y
                    psha
                    lda       CCR_OffSet,y
                    psha
                    RTI                           ; Run the user program.

;*****************************************************************************
; SWI_ISR
;*****************************************************************************
; Description:  This is a interrupt service routine for the SWI interrupt.
;               Control is handed back to the monitor program.  All user
;               registers and accumulators are saved into memory.
;
; Entry Condition:      None.
;
; Exit Condition:       Control is handed back to the monitor.  SP is
;                       pointing back to the monitor stack area.
; *---------------------------------------------------------------------------*

SWI_ISR             tsx
                    ldy       Ptr
                    tys                           ; Restore SP to monitor stack area.
                    ldd       Temp
                    std       JSWI+1              ; Restore softvector for SWI.
                    bsr       SaveUserStack
                    bsr       RemoveBreak
                    bsr       PrintReg
                    rts

;*****************************************************************************
; SaveUserStack
;*****************************************************************************
; Description:  This routine saves all user registers and accumulators into
;               memory.  It gets the value out of the user stack.
; Entry Condition:      Must return from the interrupt.
; *---------------------------------------------------------------------------*

SaveUserStack       lda       ,x
                    sta       UserRegs+CCR_OffSet
                    lda       1,x
                    sta       UserRegs+B_OffSet
                    lda       2,x
                    sta       UserRegs+A_OffSet
                    ldd       3,x
                    std       UserRegs+X_OffSet
                    ldd       5,x
                    std       UserRegs+Y_OffSet
                    ldd       7,x
                    subd      #1
                    std       UserRegs+PC_OffSet
                    ldb       #8
                    abx
                    stx       UserRegs+SP_OffSet
                    rts

;*****************************************************************************
; RemoveBreak
;*****************************************************************************
; Description:  This routine removes all the breakpoint from the user
;               program.  The SWI in the user program gets replaced with
;               the original opcode.
;
; Entry Condition:      None.
;
; Exit Condition:       All breakpoints in the user program is removed.
;
; Register(s) Changed:  All.
; *---------------------------------------------------------------------------*

RemoveBreak         ldx       #BreakTable         ; Locate top of breakpoint table.
                    ldb       #3
                    lda       #MaxBreak
Re_Loop             tsta
                    beq       :AnRTS              ; At the end of table?
                    ldy       ,x
                    cpy       #$FFFF              ; Checking for valid entry.
                    beq       NextRemoveBreak
                    psha
                    lda       2,x
                    sta       ,y                  ; Restoring original opcode.
                    pula
NextRemoveBreak     abx
                    deca
                    bra       Re_Loop

;*****************************************************************************
; PrintReg
;*****************************************************************************
; Description:  This routine prints the content of the user's registers and
;               accumulators onto the host terminal.
; Register(s) Changed:  All.
; *---------------------------------------------------------------------------*

PrintReg            ldx       #Labels
                    jsr       OutString4          ; 'A: '
                    lda       UserRegs+A_OffSet
                    jsr       Out8Bin2Hex
                    jsr       OutString4          ; 'B: '
                    lda       UserRegs+B_OffSet
                    jsr       Out8Bin2Hex
                    jsr       OutString4          ; 'X: '
                    ldd       UserRegs+X_OffSet
                    jsr       Out16Bin2Hex
                    jsr       OutString4          ; 'Y: '
                    ldd       UserRegs+Y_OffSet
                    jsr       Out16Bin2Hex
                    jsr       OutString4          ; 'PC: '
                    ldd       UserRegs+PC_OffSet
                    jsr       Out16Bin2Hex
                    jsr       OutString4          ; 'CCR: '
                    lda       UserRegs+CCR_OffSet
                    jsr       Out8Bin2Hex
                    jsr       OutString4          ; 'SP: '
                    ldd       UserRegs+SP_OffSet
                    jsr       Out16Bin2Hex
                    rts

;*****************************************************************************
; The following is a lookup table of the labels needed for the printing of
; the user's registers and accumulators.
;*****************************************************************************

Labels              fcs       'A: '
                    fcs       'B: '
                    fcs       'X: '
                    fcs       'Y: '
                    fcs       'PC: '
                    fcs       'CCR: '
                    fcs       'SP: '
                    fcb       EOT

;*****************************************************************************
; The following string of characters are used by the Help routine.  It is
; composed of all the commands available and the required arguments.
; The string is supposed to help the user in using this monitor program.
;*****************************************************************************

?                   macro
                    mset      1,~@~
                    mstr      1
                    fcc       CR,LF,~1~
                    endm

HelpString          @?        'BREAK [<addr>]  : Add/Delete breakpoints'
                    @?        'CALL [<addr>]  : Call subroutine'
                    @?        'FILL <addr1> [<addr2>] [<data>]  : Block/Byte fill'
                    @?        'GO [<addr>]  : Run program'
                    @?        'HELP or ?  : Display help'
                    @?        'LOAD [<OffSet>]  : Load Program'
                    @?        'MD [<addr1>] [<addr2>]  : Memory dump'
                    @?        '   Key Stroke:  <Ctrl-C> terminates MD'
                    @?        'PB <on/off>  : Enable <Ctrl-C>'
                    @?        'REG [<A/B/X/Y/CCR/PC/SP> <data>] : Display/modify Registers'
                    @?        'SS [<addr>]  : Single Step (using interrupt)'
                    @?        '   HotKey:  <F7>'
                    @?        'S2 [<addr>]  : Single Step (using breakpoints)'
                    @?        '   HotKey:  <F8>'
                    fcb       Null

;*****************************************************************************
; PB()
;*****************************************************************************
; Description:  This Routine toggles the user's break on/off.  This enable
;               the user to terminate the program during execution.  It
;               is useful when the program is stuck in a continuous loop.
;               The default value is Off because this command uses the
;               interrupt vector of the SCI.  If the user program uses this
;               interrupt vector, then this command would not work properly.
;
; Entry Condition:      String pointer is pointing to the argument of the
;                       string.
;
; Exit Condition:       Toggles the value stored in CtrlC_On.
;                       1 = Enable, 0 = Disable.
;
; Register(s) Changed:  All registers and accumulators.
; *---------------------------------------------------------------------------*

PB                  ldx       Ptr
                    jsr       SkipSpace
                    beq       NoPB_argument
                    stx       Ptr
                    bsr       GetPB_argument      ; Get on/off argument.
                    bcs       ArgumentOk
                    jmp       BadArgument         ; Invalid argument.

ArgumentOk          nop
NoPB_argument       jsr       NewLine
                    ldx       #PBLabel1
                    jsr       OutString
                    lda       #3
                    ldb       CtrlC_On
                    mul
                    ldx       #PBLabel2
                    abx
                    jsr       OutString           ; Display status of PB.
                    rts

;*****************************************************************************
; String labels for PB routine.
;*****************************************************************************

PBLabel1            fcs       'Break O'
PBLabel2            fcs       'ff'
                    fcs       'n '

;*****************************************************************************
; GetPB_argument
;*****************************************************************************
; Description:  This routine determines the argument for the PB command.
;               If an invalid argument is encountered, the C bit is cleared.
;               Otherwise is set.  If the argument <on> is encountered, the
;               variable CtrlC_On is set.  Otherwise is cleared.
;
; Entry Condition:      Register X contains the address of the PB argument.
;
; Exit Condition:       CtrlC_On variable is set accordingly.
;
; Register Changed:     Register A, accumulator X and C-bit in the CCR.
; *----------------------------------------------------------------------------

GetPB_argument      lda       ,x
                    jsr       UpCase
                    cmpa      #'O'
                    bne       InvalidPB
                    inx
                    lda       ,x
                    jsr       UpCase
                    cmpa      #'N'
                    bne       CheckOff            ; Is the argument 'On'?
                    lda       #1
                    sta       CtrlC_On            ; Yes, set CtrlC_On.
                    bra       ValidPB

CheckOff            cmpa      #'F'                ; Is the argument 'Off'?
                    bne       InvalidPB
                    clr       CtrlC_On            ; Yes, clr CtrlC_On.
ValidPB             sec
                    bra       :AnRTS

InvalidPB           clc
                    rts

;*****************************************************************************
; Messages used by the Monitor Program
;*****************************************************************************

MSG1                fcs       '68HC11 Monitor Program v(1.0) by Kin-Hung Leung'
MSG2                fcs       'Command> '
MSG3                fcs       'Command Too Long'
MSG4                fcs       'Bad Command'
MSG5                fcs       'Transmit Complete'
MSG6                fcs       'Warning: Checksum Error'
MSG7                fcs       'Bad Argument'
MSG8                fcs       'Break Table Full'
MSG9                fcs       'Breakpoint removed'
MSG10               fcs       'Breakpoint Table'
MSG11               fcs       'Memory Violation:'

;*****************************************************************************
; Jump Table of Subroutine in Monitor Program
;*****************************************************************************

                    org       $E000

                    jmp       GetChar
                    jmp       OutChar
                    jmp       OutString
                    jmp       NewLine
                    jmp       Hex2Bin
                    jmp       TwoHex2Bin
                    jmp       FourBin2Hex
                    jmp       EightBin2Hex
                    jmp       UpCase

;*****************************************************************************
; Interrupt Vector table in ROM
;*****************************************************************************
;                   org       ROMBS+$1FD6
                    org       $FFD6

          ;*** Vectors ***

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
VRST                dw        Monitor

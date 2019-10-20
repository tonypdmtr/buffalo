;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
; EZ-MON for the 6811 by Randy Sargent (rsargent@media.mit.edu)
;
; Adapted to ASM11 by Tony Papadimitriou (99.04.17)
; Fixed bug with ADPU setting not using immediate mode, and optimized a bit
;*******************************************************************************

VERSION             equ       20                  ;version as x.xx

REQUIRED_STACK      def       49

                    #ListOff
                    #Uses     811e2.inc
                    #ListOn

; Serial port

PORTD_WOM           equ       $20
BAUD1200            equ       $B3                 ; Assumes 8 MHz crystal
BAUD9600            equ       $B0                 ; Assumes 8 MHz crystal
TRENA               equ       $0C                 ; Transmit, Receive ENAble
RDRF                equ       $20                 ; Receive Data Register Full

; ASCII characters

SPACE               equ       ' '
DEL                 equ       127

;*******************************************************************************
                    #RAM
;*******************************************************************************

input_buf           rmb       130                 ; Input buffer length
input_buf_end       rmb       1                   ; plus one for null termination
tempword1           rmb       2                   ; Temporary word of storage (2 byte value)

HalfPeriod          rmb       2
CycleCountEnd       rmb       2                   ; Stores end of cycles
CountCountBuf       rmb       70

;*******************************************************************************
                    #ROM
;*******************************************************************************

Start               proc
                    lds       #STACKTOP           ; Set stack pointer to top of ram
                    jsr       InitSerial
                    jsr       InitAnalogs

                    ldx       #MsgBoot            ; Write the boot message
                    jsr       PutS

MainLoop            ldx       #Prompt
                    jsr       PutS

                    jsr       GetS
                    ldx       #input_buf

                    bsr       Execute
                    bra       MainLoop

;*******************************************************************************

Execute             proc
                    jsr       SkipSpaces          ; Skip over spaces in input
                    ldb       ,x

                    beq       Done@@              ; Hit null termination, done
                    inx                           ; Skip to next char

                    subb      #'a'                ; Change a-z to 0-25
                    blo       error_handler       ; if underflow, error

                    cmpb      #25
                    bhi       error_handler       ; if larger than 25, error

                    aslb                          ; Multiply B by 2
                    ldy       #command_table
                    aby                           ; X has command_table + 2*(command#)

                    ldy       ,y                  ; get vector for command
                    jsr       ,y                  ; go there
                    bra       Execute
Done@@              rts

;*******************************************************************************
; Purpose: If there is an error reading input, go here.
;        : Gives help message, restarts interaction loop
; Note(s): It is OK to jump to error_handler from anywhere -- even inside
;        : a subroutine. Error_hander sets the stack pointer back to the
;        : beginning before it restarts the interaction loop
;

error_handler       proc
                    ldx       #MsgError
                    jsr       PutS                ; print error message
                    ldx       #MsgHelp
                    jsr       PutS                ; print help message
                    lds       #STACKTOP           ; reset stack (sort of like C longjmp)
                    bra       MainLoop

;*******************************************************************************
; command_table
;
; This table contains jump points for each command
; There are entries for 'a' through 'z'.  If a command
; hasn't yet been defined, it simply vectors to 'help_command',
; which gives the help message
;
; In order to add your own command, modify one of the following
; lines to point to your new command routine.  You should
; probably add a line to the help information as well
; (see "MsgHelp     :")
;
; Be sure not to delete or add lines inadvertently.  The command
; dispatch code simply does an address offset from `command_table'.
; The `command_a' through `command_z' labels are ignored;  if you
; get them out of order
;

command_table

command_a           dw        AnalogCommand
command_b           dw          error_handler
command_c           dw        ClearCommand
command_d           dw          error_handler
command_e           dw        EchoCommand
command_f           dw          error_handler
command_g           dw          error_handler
command_h           dw          error_handler
command_i           dw          error_handler
command_j           dw          error_handler
command_k           dw          error_handler
command_l           dw        LoopCommand
command_m           dw           error_handler
command_n           dw           error_handler
command_o           dw        TimerOutputCommand
command_p           dw           error_handler
command_q           dw           error_handler
command_r           dw        ReadCommand
command_s           dw        SetCommand
command_t           dw           error_handler
command_u           dw           error_handler
command_v           dw           error_handler
command_w           dw        WriteCommand
command_x           dw           error_handler
command_y           dw           error_handler
command_z           dw        SleepCommand

LoopCommand         proc
                    jsr       ReadY
                    pshx                          ; Save pointer to rest of line
Loop@@              pulx                          ; Point back to rest of line
                    pshx
                    pshy                          ; Save Y

                    bsr       Execute             ; execute rest of line

                    puly                          ; Restore Y
                    dey
                    bne       Loop@@              ; Keep going?
                    puly                          ; Get rid of stored pointer
                    rts

;*******************************************************************************

SleepCommand        proc
                    jsr       ReadY
                    pshx
                              #Cycles
MainLoop@@          ldx       #DELAY@@            ; # of inner loops in 1 ms
                              #Cycles
Loop@@              dex                           ; 3 cycles
                    bne       Loop@@              ; 3 cycles
                              #temp :cycles
                    dey
                    bne       MainLoop@@
                    pulx
                    rts

DELAY@@             equ       BUS_KHZ/:temp

;*******************************************************************************

EchoCommand         proc
                    jsr       ReadY
                    jsr       PutY
                    jmp       PutCRLF

;*******************************************************************************

ReadCommand         proc
                    jsr       ReadY
                    ldb       ,y
                    jsr       PutB
                    jmp       PutCRLF

;*******************************************************************************

WriteCommand        proc
                    jsr       ReadY
                    jsr       ReadB
                    stb       ,y
                    rts

;*******************************************************************************

SetCommand          proc
                    jsr       ReadY
                    jsr       ReadB
                    orab      ,y
                    stb       ,y                  ; mem = mem | val
                    rts

;*******************************************************************************

ClearCommand        proc
                    jsr       ReadY
                    jsr       ReadB
                    comb                          ; 1's complement
                    andb      ,y
                    stb       ,y                  ; mem = mem & ~val
                    rts

;*******************************************************************************

AnalogCommand       proc
                    jsr       ReadB
                    jsr       MeasureAnalog
                    bsr       PutB
                    jmp       PutCRLF

          ; First get the port number.
          ; For now we ignore this and just output everything to PORTA:6

TimerOutputCommand  jsr       ReadB

          ; Next, get number of E clocks per half cycle

                    jsr       ReadY
                    sty       HalfPeriod

          ; Next, store up all the cycle counts

                    ldy       #CountCountBuf

GetCycleCountLoop   jsr       SkipSpaces
                    ldb       ,x
                    cmpb      #'$'
                    beq       GetCycleCount
                    jsr       decode_dec_digit
                    bcs       GotAllCycleCounts

GetCycleCount       xgdy                          ; D now has ptr
                    jsr       ReadY
                    xgdy                          ; Y has ptr, D has data
                    std       ,y
                    iny:2
                    bra       GetCycleCountLoop

GotAllCycleCounts   sty       CycleCountEnd

                    pshx                          ; Save X
                    ldx       #CountCountBuf
                    bra       ?ol_20

          ; For now this code only deals with OC2,
          ; which controls bit 6 of PORTA

                    clr       TCTL1

OutputLoop          ldy       ,x
          #ifdef
                    jsr       PutY
                    jsr       PutSpace
          #endif
                    lda       #%01000000          ; Toggle on OC2
                    eora      TCTL1
                    sta       TCTL1               ; Swap from toggle pin to leave pin alone

                    pshx
                    ldx       #REGS
                    ldd       TCNT
                    addd      HalfPeriod          ; 5

          ; This is the tightest code I've thought of so far.
          ; It handles HalfPeriod  of 27 and higher, which
          ; gives frequencies up to 37 KHz.  I believe this should
          ; be OK for most remote control devices

ToggleAgain
WaitTOC2            cpd       [TCNT,X             ; 6+
                    bpl       WaitTOC2            ; 3+
                    addd      HalfPeriod          ; 5
                    std       [TOC2,X             ; 5
                    dey                           ; 4
                    bne       ToggleAgain         ; 3

          ; The following implementation is a fair bit slower,
          ; which is why it is disabled within #ifdef .. #endif

          #ifdef

ToggleAgain         bset      [TFLG1,X,#%01000000 ; 7 Clear compare
                    addd      HalfPeriod          ; 5
                    std       [TOC2,X             ; 5

                    brclr     [TFLG1,X,#%01000000,*  ; 7+ Wait until TOC2 has compared

                    dey                           ; 4
                    bne       ToggleAgain         ; 3
          #endif

                    bclr      [PORTA,X,#%01000000 ; Clear PORTA:6 (OC2)

                    pulx
                    inx:2
?ol_20              cpx       CycleCountEnd
                    bne       OutputLoop

                    clr       TCTL1

                    pulx                          ; Restore X
                    rts

;*******************************************************************************
; Purpose: Puts 8-bit unsigned value in b out serial port (in decimal)

PutB                proc
                    pshd                          ; Save D
                    pshy                          ; Save Y
                    clra
                    xgdy                          ; Y = [0:B]
                    bsr       PutY
                    puly                          ; Restore Y
                    puld                          ; Restore D
                    rts

;*******************************************************************************
; Purpose: Puts 16-bit unsigend value in Y out serial port (in decimal)

PutY                proc
                    push

          ; Decode 5 digits, place on stack

                    xgdy
                    ldy       #5

Decode@@            ldx       #10
                    idiv                          ; X=X/10, D=X%10  (D=[A:B])
                    pshb                          ; Push 1-byte remainder
                    xgdx
                    dey
                    bne       Decode@@

          ; Print 5 digits, but truncate leading zeros
          ;   A =  0, truncating leading zeros
          ;   A != 0, have printed non-zero digit, so must print zeros

                    ldy       #5
                    clra

Loop@@              pulb
                    tsta
                    bne       Print@@             ; Seen non-zero digit, must print this one
                    tstb
                    beq       Skip@@              ; This is zero, skip it
Print@@             addb      #'0'                ; Add ascii `0' (48)
                    jsr       PutChar
                    tba                           ; Give A non-zero value
Skip@@              dey
                    bne       Loop@@

          ; Done with digits.  If A is still zero, then
          ; we skipped all digits.  We had better print
          ; one `0' digit!

                    tsta
                    bne       Done@@
                    ldb       #'0'                ; Print 0
                    jsr       PutChar

Done@@              pull
                    rts

;*******************************************************************************
; Purpose: Read 8 bit unsigned number from ASCII string
; Input  : X -> String
; Output : B = 8 bit unsigned number
;        : X -> first char after the last digit of the number
; Note(s): Default base is decimal;  if preceded by $, will be in hexadecimal
;        : Jumps to "error_handler" if error parsing number

ReadB               proc
                    pshy                          ; Save Y
                    psha                          ; Save B
                    bsr       ReadY
                    xgdy                          ; Swap Y and [A:B] (a= msb, b= lsb)
                    tsta                          ; Are there high order bits?
                    jne       error_handler       ; If so, # was too high
                    pula                          ; Restore B
                    puly                          ; Restore Y
                    rts

;*******************************************************************************
; Purpose: Read 16 bit unsigned number from ASCII string
; Input  : X -> String
; Output : Y = 16 bit unsigned integer
;        : X -> first char after the last digit of the number
; Note(s): Default base is decimal;  if preceded by $, will be in hexadecimal
;        : Jumps to "error_handler" if error parsing number

ReadY               proc
                    pshb                          ; Save B
                    jsr       SkipSpaces
                    ldb       ,x
                    cmpb      #'$'
                    beq       ReadY__hexadecimal
                    cmpb      #'p'
                    beq       ReadY__symbol

ReadY__decimal      ldy       #0
                    ldb       ,x
                    jsr       decode_dec_digit
                    jcs       error_handler       ; If first char not digit, error

Loop@@              ldb       ,x
                    bsr       decode_dec_digit
                    bcs       Done@@              ; If char not digit, at end of number

                    pshb                          ; Save decoded digit

          ; Multiply Y by 10 by computing (2*Y)+(8*Y)

                    xgdy                          ; Exchange Y and D
                    lsld                          ; D=Y*2
                    std       tempword1           ; tempword1= Y*2
                    lsld:2                        ; D=Y*8
                    addd      tempword1           ; D=Y*2+Y*8
                    xgdy                          ; Exchange Y and D

          ; End of multiply Y by 10

                    pulb                          ; Restore decoded digit
                    aby                           ; Add B to Y

                    inx
                    bra       Loop@@              ; Next digit
ry_end
Done@@              pulb                          ; Restore B
                    rts

;*******************************************************************************

ReadY__hexadecimal  proc
                    clry
                    inx                           ; Skip $
                    ldb       ,x
                    bsr       decode_hex_digit
                    jcs       error_handler       ; If first char not digit, error

Loop@@              ldb       ,x
                    bsr       decode_hex_digit
                    bcs       Done@@              ; If char not digit, at end of number

                    pshb                          ; Save decoded digit

          ; Multiply Y by 16

                    xgdy
                    lsld:4
                    xgdy

          ; End of multiply Y by 16

                    pulb                          ; Restore decoded digit
                    aby                           ; Add B to Y

                    inx
                    bra       Loop@@              ; Next digit

Done@@              equ       ry_end

;ry_error           jmp       error_handler

;*******************************************************************************

ReadY__symbol       proc
                    inx                           ; Skip 'p'
                    ldb       ,x
                    inx

                    cmpb      #'a'
                    beq       PortA@@

                    cmpb      #'b'
                    beq       PortB@@

                    cmpb      #'c'
                    beq       PortC@@

                    cmpb      #'d'
                    beq       PortD@@

                    cmpb      #'e'
                    beq       PortE@@

                    jmp       error_handler

PortA@@             ldy       #PORTA
                    bra       Done@@

PortB@@             ldy       #PORTB
                    bra       Done@@

PortC@@             ldy       #PORTC
                    bra       Done@@

PortD@@             ldy       #PORTD
                    bra       Done@@

PortE@@             ldy       #PORTE
                    bra       Done@@

Done@@              equ       ry_end

;*******************************************************************************
; Purpose: If X points to an ascii `space', increment X until it no longer
;        : points at one.

SkipSpaces          proc
                    pshb                          ; Save B
                    ldb       #SPACE              ; get a SPACE ready
                    dex                           ; needed for first pass
Loop@@              inx                           ; Skip to next char
                    cmpb      ,x                  ; (SPACE ascii for `space', 32)
                    beq       Loop@@              ; If not a space, we're done
                    pulb                          ; Restore B
                    rts

;*******************************************************************************
; Purpose: Decodes decimal digit in B.
;
; If B is an ascii digit `0'-`9',
; subtract ascii `0' and leave 0-9 in B
; Clear carry
;
; If B is not in range `0'-`9', set carry

decode_dec_digit    proc
                    subb      #'0'                ; Subtract ascii `0' (48)
                    blo       Done@@

                    cmpb      #9
                    bhi       ?Error

                    clc                           ; Successful, so clear carry
Done@@              rts

?Error              sec                           ; Digit not in range, so set carry
                    rts

;*******************************************************************************
; Purpose: Decodes hexadecimal digit in B.
;
; If B is a hexadecimal digit `0'-`9'
; `a'-`f', or `A'-`F', convert it to
; 0-15 and put in B.  Clear carry.
;
; If B is not a valid hexadecimal digit,
; set carry

decode_hex_digit    proc
                    cmpb      #'9'                ; If `9' or below, decode as decimal
                    bls       decode_dec_digit

                    cmpb      #'a'                ; Lowercase letter?
                    blo       Lower@@

                    addb      #'A'-'a'            ; Yes, convert lowercase to uppercase

Lower@@             subb      #'A'
                    blo       ?Error              ; Letter < `A'?  If so, it's invalid.

                    cmpb      #5
                    bhi       ?Error              ; Letter > `F'? If so, it's invalid.

                    addb      #10                 ; Add 10 so in range 10-15

                    clc                           ; Successful, so clear carry
                    rts

;*******************************************************************************
; Purpose: Get a character from the serial port.
; Input  : None
; Output : B = character

GetChar             proc
Loop@@              ldb       SCSR
                    andb      #RDRF               ; Receive register full?
                    beq       Loop@@              ; If not, loop
                    ldb       SCDR                ; Get received character
                    rts

;*******************************************************************************
; Purpose: Puts character to serial port.
; Input  : B = character
; Output : None
; Note(s): LF is sent as CR, LF

PutChar             proc
                    cmpb      #LF
                    bne       PutChar@@

                    ldb       #CR
                    bsr       PutChar@@

                    ldb       #LF

PutChar@@           tst       SCSR
                    bpl       PutChar@@
                    stb       SCDR                ; Transmit B
                    rts

;*******************************************************************************
; Purpose: Gets line from serial port
;        : Places into "input_buf"
;        : Allows backspace (8 or 127)
;        : Waits for carriage return (13)
;        : Kills line on ctrl-x (24)

GetS                proc
                    pshb                          ; save B
                    pshx                          ; save X
                    pshy                          ; save Y

CancelLine@@        ldx       #input_buf

Loop@@              bsr       GetChar
                    andb      #$7f                ; strip off high bit

                    cmpb      #BS                 ; backspace key? (8)
                    beq       Backspace@@

                    cmpb      #DEL                ; delete key? (127)
                    beq       Backspace@@

                    cmpb      #CR                 ; carriage return key? (13)
                    beq       CR@@

                    cmpb      #24                 ; cancel line key (ctrl-x)
                    beq       CancelLine@@

                    cpx       #input_buf_end
                    beq       Error@@             ; input buffer full!

                    cmpb      #SPACE
                    blo       Error@@             ; no control characters

                    stb       ,x                  ; character was OK.  add it
                    bsr       PutChar             ; echo char

                    inx
                    bra       Loop@@

Backspace@@         cpx       #input_buf
                    bls       Error@@

                    ldb       #BS                 ; back up cursor
                    bsr       PutChar

                    ldb       #SPACE              ; erase char by sending a space
                    bsr       PutChar

                    ldb       #BS                 ; back up cursor
                    bsr       PutChar

                    dex
                    bra       Loop@@

Error@@             ldb       #BELL               ; bell
                    bsr       PutChar

                    bra       Loop@@

CR@@                clr       ,x                  ; null terminate input

                    bsr       PutCRLF
                    puly                          ; restore Y
                    pulx                          ; restore X
                    pulb                          ; restore B
                    rts

;*******************************************************************************
; Purpose: Puts null-terminated string to serial port
; Input  : X -> String

PutS                proc
                    pshx                          ; Save X
                    pshb                          ; Save B

Loop@@              ldb       ,x
                    beq       Done@@              ; If at null termination, exit

                    bsr       PutChar

                    inx
                    bra       Loop@@

Done@@              pulb                          ; Restore B
                    pulx                          ; Restore X
                    rts

;*******************************************************************************

PutSpace            proc
                    pshb
                    ldb       #SPACE
                    bsr       PutChar
                    pulb
                    rts

;*******************************************************************************

PutCRLF             proc
                    pshb
                    ldb       #LF
                    bsr       PutChar
                    pulb
                    rts

;*******************************************************************************

Debug1              proc
                    pshb
                    ldb       #'1'
                    bsr       Debug
                    pulb
                    rts

;*******************************************************************************

Debug2              proc
                    pshb
                    ldb       #'2'
                    bsr       Debug
                    pulb
                    rts

;*******************************************************************************

Debug3              proc
                    pshb
                    ldb       #'3'
                    bsr       Debug
                    pulb
                    rts

;*******************************************************************************

Debug4              proc
                    pshb
                    ldb       #'4'
                    bsr       Debug
                    pulb
                    rts

;*******************************************************************************

Debug               proc
                    pshb
                    ldb       #'D'
                    jsr       PutChar
                    pulb
                    jmp       PutChar

;*******************************************************************************
; Purpose: Turns on serial port and initializes to reasonable values, speed
;        : 9600 baud. Unfortunately, given an 8 MHz crystal, we can't get
;        : accurate divisors for 19.2K, 38.4K, 57.6K, 115.2K baud

InitSerial          proc
                    psha

                    lda       SPCR                ; Turn off wired-or serial output
                    anda      #[PORTD_WOM
                    sta       SPCR

                    lda       #BAUD9600           ; Set baud rate to 9600
                    sta       BAUD

                    lda       #TRENA              ; Enable transmit and receive
                    sta       SCCR2

                    pula
                    rts

;*******************************************************************************
; Purpose:
; Input  : B = Analog channel 0-7
; Output : B = Analog value 0-255
; Note(s): Be sure to turn on analog subsystem first with "InitAnalogs "

MeasureAnalog       proc
                    andb      #15                 ; Mask off top 4 bits
                    stb       ADCTL               ; Start conversion

          ; Note that the completion flag is set only after _4_ conversions
          ; take place.  You could be more efficient by waiting the exact #
          ; of clocks required for the first reading to be good.
          ;
          ; On the other hand, 4 conversions gives the internal capacitance of
          ; the A/D time to charge up in case you have an input that's has
          ; an impedance that's a bit too high.

Loop@@              tst       ADCTL
                    bpl       Loop@@              ; CCF = conversion complete flag
                    ldb       ADR1                ; Load result
                    rts

;*******************************************************************************
; Purpose: Turns on analog ports

InitAnalogs         proc
                    lda       OPTION
                    ora       #$80                ; ADPU = AD Power Up (BUG FIX: added missing #)
                    sta       OPTION
                    rts

;*******************************************************************************
; Strings

Prompt              fcs       'ez-mon> '
MsgBoot             fcb       LF
                    fcc       'Welcome to EZ-MON {VERSION(2)}     10/9/95    Randy Sargent',LF
                    fcs       '   http://www.ai.mit.edu/people/rsargent/ez-mon.html',LF
                    #size     MsgBoot
MsgError            fcs       'Error in input.',LF
MsgHelp             fcb       LF
                    fcc       'EZ-MON help:',LF
                    fcc       'r <addr>        Read memory byte',LF
                    fcc       'w <addr> <data> Write memory byte',LF
                    fcc       's <addr> <data> Set bits (*addr = *addr | data)',LF
                    fcc       'c <addr> <data> Clear bits (*addr = *addr & ~data)',LF
                    fcc       'a <num>         Read analog channel',LF
                    fcc       'e <num>         Echo number',LF
                    fcc       'o <chan> <clocks/cycle> <cycles-on> (<cycs-off> <cycs-on> ...)',LF
                    fcc       '                "Beep" PORTA:6 (<chan> currently ignored)',LF
                    fcc       'l <num> ...     Loop over ... <num> times',LF
                    fcc       'z <num>         Sleep <num> milliseconds',LF
                    fcc       'Numbers can be entered in decimal (default), hexadecimal (start with $)',LF
                    fcc       'or symbolically (pa, pb, pc, pd, pe for ports a-e)',LF
                    fcc       'Examples:',LF
                    fcc       'r $1000         Reads byte from port A',LF
                    fcc       'r 4096          Reads byte from port A',LF
                    fcs       'r pa            Reads byte from port A',LF,LF
                    #size     MsgHelp

;*******************************************************************************
; Jump Vectors
;*******************************************************************************
; These are typically located in the $FF00 page (unless the chip is running
; in special mode, in which case they should be in the $BF00 page)

          #ifdef BOARD6.270
Vreset              set       Vreset-$4000        ; This is for 6.270 board (in special mode)
                    #MEMORY   Vreset Vreset+1
          #endif
                    @vector   Vreset,Start

                    end       :s19crc

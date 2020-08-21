;*******************************************************************************
; Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
; MONITOR
; 06.08.2013 T Schaer
; Based on KERNEL1E.ASM Nov 5 2004
; Modifications by Tony G. Papadimitriou <tonyp@acm.org>
;*******************************************************************************
                    #CaseOn
                    #ExtraOn
;-------------------------------------------------------------------------------
; Some useful I/O registers
;-------------------------------------------------------------------------------

REGS                def       $1000
SCSR                equ       REGS+$2E
SCDR                equ       REGS+$2F
SCCR2               equ       REGS+$2D
BAUD                equ       REGS+$2B
DDRD                equ       REGS+$09
TMSK2               equ       REGS+$24
TFLG2               equ       REGS+$25
PACTL               equ       REGS+$26
BUFSIZE             equ       16                  ; keyboard buffer size

CR                  equ       13                  ; Carriage Return
LF                  equ       10                  ; Line Feed
BS                  equ       8                   ; Backspace

;*******************************************************************************
                    #RAM                          ; Variables
;*******************************************************************************
                    org       $0080
          ;-------------------------------------- ; Real-time clock
hours               rmb       1
minutes             rmb       1
seconds             rmb       1
ticks               rmb       1
          ;-------------------------------------- ; Used by GetByte - get two ASCII chars and convert to 1 hex byte
conv                rmb       1                   ; 8-bit hex value
          ;-------------------------------------- ; For d [xxxx][y] command
top8                rmb       1                   ; upper 8 bits of dump address
low8                rmb       1                   ; lower 8 bits of dump address
bytes               rmb       1                   ; # of 16-byte paragraphs to display
dump                rmb       16                  ; save current paragraph for ASCII conv.
sixteen             rmb       1                   ; loop counter for within a paragraph
          ;-------------------------------------- ; Used by EDIT for command line & args
buffer              rmb       BUFSIZE
          ;-------------------------------------- ; Used by BufferToHex: points to first cmd-line argument
.arg                rmb       2
          ;-------------------------------------- ; Used by 'shell'
yes_mx1             rmb       1                   ; MX1 installed. 1=yes, 0=no
.faa                rmb       2                   ; first available RAM address

;*******************************************************************************
                    #ROM                          ; Program
;*******************************************************************************
                    org       $F800

Start               proc
                    sei
                    lds       #$7F                ; put stack in bottom half of RAM
          ;-------------------------------------- ; SCI initialization
                    lda       #$02                ; make TxD pin an output
                    sta       DDRD
                    lda       #$30                ; 4800 baud
                    sta       BAUD
                    lda       #$0C                ; TE=1, RE=1, no interrupts
                    sta       SCCR2
          ;-------------------------------------- ; RTI Initialization
                    lda       #$02                ; RTI rate = 16.384ms
                    sta       PACTL
                    lda       #$40                ; Enable RTI interrupt
                    sta       TMSK2
                    lda       #$40                ; Start RTI-ing
                    sta       TFLG2
          ;--------------------------------------
          ; Initialize Time data structure: initial time 23:59:55:00
          ;--------------------------------------
                    lda       #23                 ; init hours to 23
                    sta       hours
                    lda       #59                 ; init minutes to 59
                    sta       minutes
                    lda       #55                 ; init seconds to 55
                    sta       seconds
                    clr       ticks               ; start ticks at zero
                    cli                           ; enable interrupts
          ;-------------------------------------- ; Miscellaneous initializations
                    ldx       #msg_reset          ; print reset message
                    jsr       OutString
                    ldx       #msg_help           ; show available commands
                    jsr       OutString
                    lda       #$42
                    sta       $200                ; attempt to store to expansion RAM
                    cmpa      $200                ; if $42 retrieved, RAM present
                    beq       _32K@@
                    clr       yes_mx1             ; yes_mx1=false
                    ldx       #.faa               ; load address of faa ptr
                    inx:2
                    stx       .faa                ; store first available RAM addx
                    lda       .faa                ; display upper byte of faa
                    jsr       OutByte
                    lda       .faa+1              ; display lower byte of faa
                    jsr       OutByte
                    bra       WarmStart
_32K@@              lda       #1
                    sta       yes_mx1             ; yes_mx1=true
                    ldx       #msg_y32            ; indicate mx1 detected
                    jsr       OutString
;                   bra       WarmStart

;*******************************************************************************
; Command Interpreter loop

WarmStart           proc
                    jsr       CrLf
                    ldx       #msg_wrm_strt       ; display warm start message
                    jsr       OutString
MainLoop            jsr       CrLf
                    ldx       #msg_prompt
                    jsr       OutString           ; display prompt
                    jsr       Edit                ; get command line string
                    jsr       CrLf
                    ldx       #buffer             ; look at command
                    lda       ,x
                    beq       MainLoop            ; ignore empty buffer
          ;-------------------------------------- ; Command interpreter
                    cmpa      #'d'
                    beq       d_ump
          ;--------------------------------------
                    cmpa      #'x'
                    jeq       x_amine
          ;--------------------------------------
                    cmpa      #'h'
                    jeq       h_elp
          ;--------------------------------------
                    cmpa      #'s'
                    jeq       SetTime
          ;--------------------------------------
                    cmpa      #'t'
                    jeq       DisplayTime
          ;--------------------------------------
                    cmpa      #'m'
                    jeq       FreeRam
          ;--------------------------------------
                    cmpa      #'j'
                    jeq       j_ump
          ;--------------------------------------
                    cmpa      #'l'
                    jeq       l_oad
          ;--------------------------------------
                    cmpa      #'?'
                    bne       Help@@
                    ldx       #msg_error          ; unrecognized command, error
                    bra       Print@@
          ;--------------------------------------
Help@@              ldx       #msg_help
Print@@             jsr       OutString
                    bra       MainLoop

;*******************************************************************************
; Sub - Programs - The commands that are 'shell' commands
;*******************************************************************************

;*******************************************************************************
; D_UMP: Dump memory contents to screen in hex & ASCII format
; displays paragraphs of 16 memory locations, also shows ASCII
; representation of memory location if its value is greater
; than $30 and less than $7F inclusive.
; ARGUMENTS: 4-digit hex starting address, optional 2-digit number
; of paragraps to display, also in hex.
; Re-written to use BufferToHex and the keyboard buffer to pass args
; Optimized Jan 11 2001 to use .arg

d_ump               proc
                    ldx       #buffer             ; point to keyboard buffer
                    jsr       BufferToHex         ; convert arguments to hex
                    ldy       .arg                ; point to location in buffer
                    ldx       ,y                  ; get argument [xxxx]
                    stx       top8                ; save as dump start address
                    iny:2                         ; skip start address
                    lda       ,y                  ; examine next char in buffer
                    cmpa      #' '                ; is it a space?
                    beq       Args@@              ; if so, there may be more args
NoArgs@@            lda       #1                  ; set bytes = 1 * 16
                    sta       bytes
                    bra       Loop@@
Args@@              iny                           ; skip space after [xxxx]
                    lda       ,y                  ; read [yy]
                    beq       NoArgs@@            ; if it is NULL, there are no args
                    sta       bytes               ; if not, save as # of paragraphs
Loop@@              lda       bytes
                    beq       Done@@              ; if bytes = 0, exit
                    jsr       CrLf                ; start new line
                    lda       top8
                    jsr       OutByte             ; display top 8 bits of address
                    lda       low8
                    jsr       OutByte             ; display low 8 bits of address
                    jsr       PrintSpace          ; display space after address
                    lda       #16                 ; init loop counter to 16
                    sta       sixteen
                    ldx       top8                ; load starting address of dump
                    ldy       #dump               ; pointer to temp. storage
Read@@              lda       ,x                  ; get byte from memory
                    sta       ,y                  ; save in temp. storage
                    jsr       OutByte             ; display it
                    jsr       PrintSpace          ; space between entries
                    inx                           ; point to next source byte
                    iny                           ; point to next temp. byte
                    dec       sixteen             ; decrement loop counter
                    bne       Read@@              ; quit when sixteen = 0
                    stx       top8                ; save updated address
                    dec       bytes               ; decrement # of 16 byte paragraphs
                    jsr       PrintSpace
                    ldy       #dump               ; prepaer to output ASCII rendition
                    lda       #16
                    sta       sixteen             ; re init loop counter
Dump@@              lda       ,y
                    cmpa      #' '                ; lower than space?
                    bhs       NoCtrl@@            ; branch if not a control char.
                    lda       #'.'                ; if yes replace with '.'
NoCtrl@@            cmpa      #$7F
                    bls       Ascii@@             ; $7F or lower is ok
                    lda       #'.'                ; if over $7F replace with '.'
Ascii@@             jsr       OutChar             ; display ASCII memory byte
                    iny                           ; point to next source byte
                    dec       sixteen             ; decrement loop counter
                    bne       Dump@@              ; loop while sixteen !=0
                    bra       Loop@@              ; display another 16-byte paragraph
Done@@
?MainLoop           jmp       MainLoop

;*******************************************************************************
; X_AMINE: view/change memory byte
; Useful for setting registers, flags, etc.
; Status: stub function written Jan 09 2001
; First version Jan 11 2001

x_amine             proc
                    ldx       #buffer
                    jsr       BufferToHex         ; hex-ify arguments
                    ldy       .arg
                    ldx       ,y
Loop@@              stx       top8                ; use same vars as d_ump
                    lda       top8                ; display upper 8 bits of address
                    jsr       OutByte
                    lda       low8                ; display lower 8 bits of address
                    jsr       OutByte
                    jsr       PrintSpace          ; display space
                    lda       ,x                  ; get current memory value
                    jsr       OutByte
                    jsr       PrintSpace
                    jsr       GetByte             ; wait for user input
                    bcs       Abort@@             ; quit if nothing entered
                    sta       ,x                  ; save byte in location
                    inx                           ; increment memory pointer
                    lda       #CR                 ; display new line
                    jsr       OutChar             ; cannot use CRLF() b/c it fucks X
                    lda       #LF
                    jsr       OutChar
                    bra       Loop@@              ; do it all again
Abort@@             equ       ?MainLoop

;*******************************************************************************
; J_UMP: begin executing program code at a user-specified address
; The last link in creating a complete ROM monitor
; jump address is supplied as command-line argument
; IMPORTANT: Do not use JSR to call this function!

j_ump               proc
                    ldx       #buffer             ; point to keyboard buffer
                    jsr       BufferToHex         ; convert args to hexadecimal
                    ldy       .arg                ; get location of first argument
                    beq       Abort@@             ; no address supplied, abort
                    ldx       ,y                  ; load jump address into X
                    jmp       ,x                  ; jump to address in X
Abort@@             equ       ?MainLoop           ; return on error

;*******************************************************************************
; SetTime: alter value of Time data structure to something other
; than the power-on test values
; option to quit without entering seconds value by pressing enter
; data-format conversion routine still missing (BCD2H)
; Altered Jan 11 2001 to use revised GetByte

SetTime             proc
                    clr       TMSK2               ; turn off RTI
                    jsr       GetByte             ; get the hours digits (echo automatically)
                    bcs       Done@@              ; if no data entered, quit
;                   jsr       bcd2h               ; convert to hex from user-input BCD
                    sta       hours               ; store in data structure
                    jsr       PrintColon          ; display the colon separator
                    jsr       GetByte             ; get minutes
;                   jsr       bcd2h               ; convert to hex
                    sta       minutes
                    jsr       PrintColon          ; display the colon separator
                    jsr       GetChar             ; get single char (10's of seconds)
                    cmpa      #CR                 ; check if it's the return character
                    beq       Done@@              ; if so, job is done
                    jsr       OutChar             ; echo character
                    jsr       CharToHex           ; convert to hex
                    lsla:4
                    tab                           ; copy ACCA to ACCB
                    jsr       GetChar             ; get 1's of seconds
                    jsr       OutChar             ; echo
                    jsr       CharToHex           ; convert to hex
                    aba                           ; combine digits in ACCA
                    sta       seconds             ; finally store it in data structure
Done@@              lda       #$40
                    sta       TMSK2               ; turn RTI back on
??MainLoop          jmp       MainLoop

;*******************************************************************************
; DisplayTime: prints the current time to the screen.
; no variables passed or returned. ACCA altered.
; Displays time in decimal format.

DisplayTime         proc
                    lda       hours               ; get hours
                    bsr       Print@@
                    jsr       PrintColon
          ;--------------------------------------
                    lda       minutes             ; get minutes
                    bsr       Print@@
                    jsr       PrintColon
          ;--------------------------------------
                    lda       seconds             ; get seconds
Print@@             jsr       HexToBcd            ; convert to BCD
                    tba                           ; (only tens and units)
                    jmp       OutByte             ; ASCII-fy and output

;*******************************************************************************
; H_ELP: Command help for various 'shell' commands
; Temporary function, may assign help function to each subprogram
; Very inefficient code, could be fixed up using a jump table

h_elp               proc
                    ldx       #buffer             ; point to keyboard buffer
Loop@@              lda       ,x
                    beq       Usage@@             ; quit if NULL hit before space found
                    inx                           ; point to next char
                    cmpa      #' '                ; search for space
                    bne       Loop@@              ; keep looking
          ;--------------------------------------
                    lda       ,x                  ; get command 'word'
                    cmpa      #'d'                ; is it 'd'?
                    bne       x?@@                ; if not, skip around it
                    ldx       #msg_d_help         ; load help string for 'd'
                    bra       Print@@             ; display string
          ;--------------------------------------
x?@@                cmpa      #'x'                ; is it 'x'?
                    bne       s?@@                ; if not, skip around it
                    ldx       #msg_x_help         ; load string
                    bra       Print@@             ; display it
          ;--------------------------------------
s?@@                cmpa      #'s'                ; is it 's'?
                    bne       h?@@                ; if not, skip around it
                    ldx       #msg_s_help         ; load string
                    bra       Print@@             ; display it
          ;--------------------------------------
h?@@                cmpa      #'h'                ; is it 'h'?
                    bne       t?@@                ; if not, skip around it
                    ldx       #msg_h_help         ; load string
                    bra       Print@@             ; display it
          ;--------------------------------------
t?@@                cmpa      #'t'                ; : t?
                    bne       j?@@                ; if not, skip around it
                    ldx       #msg_t_help         ; load string
                    bra       Print@@             ; display
          ;--------------------------------------
j?@@                cmpa      #'j'                ; j?
                    bne       Help@@              ; if not, skip around it
                    ldx       #msg_j_help
                    bra       Print@@
          ;--------------------------------------
Help@@              ldx       #msg_no_help        ; no help entry
                    bra       Print@@
          ;--------------------------------------
Usage@@             ldx       #msg_usage          ; display usage help
Print@@             jsr       OutString
                    jsr       CrLf
                    bra       ??MainLoop

;*******************************************************************************
; L_OAD: downline loader for S19 files generated by AS11
; hacking-around first version, Jan 17 2001
; completely re-written Nov. 5 2004

l_oad               proc
                    jsr       CrLf                ; display 'begin loading'
                    ldx       #msg_l_begin
                    jsr       OutString
                    clr       bytes               ; init program length counter
          ;--------------------------------------
Start@@             jsr       GetChar             ; get the S
                    cmpa      #'S'
                    beq       S@@                 ; if yes, start new S19 record
                    bra       Start@@             ; if no, keep trying
          ;--------------------------------------
S@@                 jsr       GetChar
                    cmpa      #'1'                ; S1 record
                    beq       DoS1@@
                    cmpa      #'9'                ; S9 record: S9030000FC always
                    beq       S9@@
                    bra       Done@@              ; if neither, quit
          ;--------------------------------------
DoS1@@              ldx       #buffer             ;** Start processing an S1 record**
                    bsr       GetChar             ; ACCA=first byte of length field
                    sta       ,x                  ; buffer[0]=ACCA
                    bsr       GetChar             ; ACCA=second byte of length field
                    sta       1,x                 ; buffer[1]=ACCA
                    jsr       DoubleCharToHex     ; ACCA=hex length field value
                    tab                           ; ACCB=hex length (loop ctr)
                    subb      #3                  ; correct length for addx & CRC fields
                    ldx       #buffer             ; x -> buffer[0]
                    bsr       GetChar             ; address byte 1
                    sta       ,x                  ; buffer[0]=ASCII address byte 1
                    bsr       GetChar             ; address byte 2
                    sta       1,x                 ; buffer[1]=ASCII address byte 2
                    bsr       GetChar             ; address byte 3
                    sta       2,x                 ; buffer[2]=ASCII address byte 3
                    bsr       GetChar             ; address byte 4
                    sta       3,x                 ; buffer[3]=ASCII address byte 4
                    jsr       QuadCharToHex       ; x=program load address
                    txy                           ; copy X into Y - y=program load address
                    ldx       #buffer             ; x -> buffer[0]
Loop@@              bsr       GetChar             ; ASCII program byte 1
                    sta       ,x                  ; buffer[0]=ASCII program byte 1
                    bsr       GetChar             ; ASCII program byte 2
                    sta       1,x                 ; buffer[1]=ASCII program byte 2
                    jsr       DoubleCharToHex     ; ACCA=object code byte
                    sta       ,y                  ; store code byte in RAM
                    inc       bytes               ; advance the program length counter
                    iny                           ; point to next code byte location
                    decb                          ; loop ctr --
                    beq       Start@@             ; loop ctr=0, get next S19 record
                    bra       Loop@@              ; else finish this record
          ;--------------------------------------
S9@@                bsr:4     GetChar             ; get the 03 00 00 FC
                    bsr       CrLf
                    ldx       #msg_lmesg          ; display 'Done.'
                    bsr       OutString
                    bsr       CrLf
                    lda       .faa                ; display upper byte of faa
                    bsr       OutByte
                    lda       .faa+1              ; display lower byte of faa
                    bsr       OutByte
                    bsr       CrLf                ; new line
                    lda       bytes               ; ACCA = loaded program length
                    bsr       HexToBcd            ; HUNDREDS, ACCA=tens:ones
                    bsr       OutByte
                    tba                           ; ACCA = 10's:1's digits
                    bsr       OutByte
                    ldx       #msg_l_bytes        ; display ' bytes loaded.'
                    bsr       OutString
                    bsr       CrLf
Done@@              jmp       MainLoop

;*******************************************************************************
; Subroutines for writing sub-programs and useful user functions
;*******************************************************************************

;*******************************************************************************
; CrLf: print a new line to the screen
; occurs so often it warrants its own subroutine

CrLf                proc
                    ldx       #msg_newline
                    bra       OutString

;*******************************************************************************
; GetChar: a polling loop that gets a character from the serial port
; returns char in ACCA

GetChar             proc
Loop@@              lda       SCSR                ; get serial status register
                    anda      #$20                ; mask off all bits but RDRF flag
                    beq       Loop@@              ; keep checking if RDRF=0
                    lda       SCDR                ; load received character
                    rts

;*******************************************************************************

PrintColon          proc
                    lda       #':'                ; colon separator
                    bra       OutChar             ; display it

;*******************************************************************************

PrintSpace          proc
                    lda       #' '
;                   bra       OutChar

;*******************************************************************************
; OutChar: sends one character to serial port receives character in ACCA

OutChar             proc
Loop@@              tst       SCSR                ; read serial status register
                    bpl       Loop@@              ; loop until TDRE = 1
                    sta       SCDR                ; send char
                    rts

;*******************************************************************************
; OutString: sends a string of characters to serial port
; address of string is passed in X

OutString           proc
Loop@@              lda       ,x                  ; get char in string
                    beq       Done@@              ; exit if null terminator
                    bsr       OutChar             ; output char
                    inx                           ; point to next char
                    bra       Loop@@
Done@@              equ       :AnRTS

;*******************************************************************************
; HexToASCII: Converts an 8-bit hex number into two ASCII characters
; Hex number passed in ACCA, lower ASCII returned in ACCB

HexToASCII          proc
                    tab                           ; copy hex number into ACCB
                    lsra:4                        ; higher hex digit now in LSB's of ACCA
                    adda      #'0'                ; ASCII-fy both registers in one shot
                    cmpa      #'9'                ; is it below '9'?
                    bls       _2@@                ; if lower, conversion correct
                    adda      #7                  ; if not, adjust for A-F
_2@@                andb      #$0F                ; mask off upper 4 bits
                    addb      #'0'
                    cmpb      #'9'
                    bls       Done@@              ; quit if conversion correct
                    addb      #7                  ; if not, adjust for A-F
Done@@              rts

;*******************************************************************************
; GetByte: Gets two ASCII chars from serial port and converts to 8-bit hex
; Gets its own chars from the serial port, returns hex in CONV & ACCA
; Carry flag set if nothing entered

GetByte             proc
                    bsr       GetChar             ; get char from serial port
                    cmpa      #CR                 ; is it enter?
                    beq       AbortEQ@@           ; user is aborting
                    bsr       OutChar             ; echo it
                    jsr       CharToHex           ; convert to 4-bit hex
                    lsla:4                        ; shift it left 4 times
                    sta       conv                ; save it
                    bsr       GetChar             ; get second char
                    cmpa      #CR                 ; is it enter?
AbortEQ@@           sec                           ; return error code
                    beq       Done@@
                    bsr       OutChar             ; echo it
                    jsr       CharToHex           ; convert to 4-bit hex
                    adda      conv                ; add first char to it
                    sta       conv                ; save full 8-bit hex
Done@@              rts

;*******************************************************************************
; OutByte: Converts a hex number to ASCII chars and outputs it
; Hex is passed in ACCA. Returns nothing.
; Fixed Jan 10 2001 to take advantage of OutChar pushing B to stack

OutByte             proc
                    bsr       HexToASCII
                    bsr       OutChar
                    tba
                    bra       OutChar

;*******************************************************************************
; HexToBcd: Converts 8-bit hex to 12-bit BCD
; Input  : A = hex
; Output : A = hundreds digit
;        : B = tens (upper nibble) and units (lower nibble)
; Note(s): Call this routine just before OutByte to display decimal instead of hex

                    #spauto

HexToBcd            proc
                    pshx                          ; protect caller's X
                    #ais
          ;--------------------------------------
                    tab
                    clra                          ; clear upper byte of D
                    ldx       #100                ; load divisor
                    idiv                          ; X=hundreds, D=remainder
                    pshx      hundreds@@          ; (hundreds in lower byte)
          ;--------------------------------------
                    ldx       #10                 ; load divsor
                    idiv                          ; X=10's digit, D=1's
                    pshb      units@@
                    pshx                          ; tens
                    pulb:2                        ; B = tens
                    lda       #16                 ; multiplying by 16 will
                    mul                           ; shift 10's into upper 4 bits
          ;--------------------------------------
                    tsx
                    addb      units@@,spx         ; combine 10's and 1's in ACCB
          ;--------------------------------------
                    pula::ais                     ; A = 100's
          ;--------------------------------------
                    pulx                          ; restore caller's X
                    rts
#sp
;*******************************************************************************
; EDIT: The input routine for the command interpreter
; One line of 15 chars max (to change, alter BUFFER length)
; Last entry in string is reserved for the NULL ($00)
; ENTER terminates routine, BACKSPACE allows editing

Edit                proc
                    ldx       #buffer             ; X points to command-line buffer
                    ldb       #BUFSIZE-1          ; set loop counter=15 chars max
Loop@@              jsr       GetChar             ; get a character from keyboard
                    cmpa      #CR                 ; check for enter
                    beq       Done@@              ; quit if it is
                    cmpa      #BS                 ; check for backspace character
                    beq       Backspace@@         ; do the thing if it is
                    sta       ,x                  ; store character in buffer
                    jsr       OutChar
                    inx                           ; point to next location in buffer
                    decb                          ; decrement loop counter
                    beq       Done@@              ; exit on loop counter=0
                    bra       Loop@@              ; otherwise repeat
Backspace@@         cmpb      #BUFSIZE-1          ; is backspace the first char typed?
                    beq       Loop@@              ; ignore it
                    jsr       OutChar             ; try and move cursor back one
                    jsr       PrintSpace          ; clear that letter
                    lda       #BS                 ; try and move cursor back one
                    jsr       OutChar
                    dex                           ; move buffer pointer back by one
                    bra       Loop@@              ; get next char
Done@@              clr       ,x                  ; terminate string with NULL
                    rts

;*******************************************************************************
; BufferToHex: Converts any command-line arguments in the keyboard
; buffer to hexadecimal from ASCII. Starts converting after the first space is
; found in the buffer - this allows future commands of more than a single letter
; leaves spaces as separators for the arguments. .arg points to the first
; converted item. To call: address of buffer must be in X
; Written in Florida December 2000
; Revised Jan 11 2001 to add .arg

BufferToHex         proc
Loop@@              lda       ,x                  ; get char in buffer
                    beq       Done@@              ; exit if end of buffer ($00) hit
                    cmpa      #' '                ; look for space character
                    beq       Space@@             ; start converting if space found
                    inx                           ; point to next char
                    bra       Loop@@
          ;--------------------------------------
Space@@             inx                           ; skip space character
                    stx       .arg                ; save location of first argument
                    ldy       .arg                ; Copy X into Y: X=src ptr, Y=dest ptr
          ;--------------------------------------
Hexify@@            lda       ,x                  ; get first char after space
                    beq       Done@@              ; exit if null detected
                    cmpa      #' '                ; is it space character?
                    beq       SkipSpace@@         ; if so, preserve it as a separator
                    bsr       CharToHex           ; convert char to hex
                    sta       ,y                  ; save in buffer
                    inx                           ; point to next source char
                    lda       ,x                  ; get next source char
                    beq       Done@@              ; quit if null detected
                    bsr       CharToHex           ; convert this to hex also
                    ldb       ,y                  ; get previous converted char
                    lslb:4                        ; combine the two chars
                    aba                           ; full 8-bit hex number
SkipSpace@@         sta       ,y                  ; store in buffer
                    inx                           ; next source entry
                    iny                           ; next dest entry
                    bra       Hexify@@            ; continue converting
          ;--------------------------------------
Done@@              sta       ,x                  ; place NULL at end of converted buffer
                    rts

;*******************************************************************************
; FreeRam: displays the free RAM in the system. Takes no arguments
; Works only for unexpanded system
; Uses the first available address pointer .faa as bottom of RAM
; and $FF as top of RAM - change this if expanded
; Written Jan. 13 2001

FreeRam             proc
                    jsr       CrLf                ; new line
                    lda       #$FF                ; put top of RAM in ACCA
                    suba      .faa+1              ; subtract highest free RAM address
                    jsr       HexToBcd            ; convert to decimal
                    tba
                    jsr       OutByte             ; display free memory bytes
                    ldx       #msg_mem_free
                    jsr       OutString
                    jmp       MainLoop

;*******************************************************************************
; CharToHex: Convert one ASCII byte into 4-bit hex
; ASCII passed in A, value returned in A
; Input                         : ACCA=aaaa aaaa (ASC)
; Output                        : ACCA=0000 hhhh (HEX)
; Parameter passing             : ACCA (value to be converted)
; Registers used internally     : ACCA

CharToHex           proc
                    suba      #'0'
                    cmpa      #9
                    bls       Done@@
                    suba      #7
Done@@              rts

;*******************************************************************************
; DoubleCharToHex: Convert 2 ASCII bytes into 8-bit hex
; X points to a buffer containing two ASCII chars
; A holds return value
;
; Input                         : INDX -> X:Y (2 ASCII chars)
; Output                        : ACCA = XY   (1 hex byte)
; Parameter passing             : INDX (pointer to buffer)
;                               : ACCA (return value)
; Registers used internally     : ACCA, ACCB, INDX

DoubleCharToHex     proc
                    pshb                          ; preserve ACCB
                    lda       ,x                  ; ACCA = aaaa aaaa (ASC)
                    bsr       CharToHex           ; ACCA = 0000 hhhh (HEX)
                    tab                           ; ACCB = 0000 hhhh (HEX)
                    lda       1,x                 ; ACCA = bbbb bbbb (ASC)
                    bsr       CharToHex           ; ACCA = 0000 iiii (HEX)
                    lslb:4                        ; ACCB = hhhh 0000
                    aba                           ; ACCA = hhhh iiii (HEX)
                    pulb                          ; restore ACCB
                    rts

;*******************************************************************************
; QuadCharToHex: Convert 4 ASCII bytes into 2 hex bytes
; X points to a buffer containing 4 ASCII chars
; X holds 2-byte hex return value
;**
; Input                         : INDX -> X:Y:Z:W (4 ASCII chars)
; Output                        : INDX =  XYZW    (1 hex byte)
; Parameter Passing             : INDX (pointer to buffer)
; Registers used internally     : ACCA, ACCB, INDX, INDY

QuadCharToHex       proc
                    pshy                          ; save INDY
                    pshd
                    txy                           ; copy X into Y - Y = buffer base address
                    bsr       DoubleCharToHex     ; ACCA = XY (HEX)
                    tab                           ; ACCB = XY (HEX)
                    tyx                           ; copy Y into X - X = buffer base address
                    inx:2                         ; point to next two chars
                    bsr       DoubleCharToHex     ; ACCA = ZW (HEX)
                    xgab                          ; swap ACCA & ACCB
                    xgdx                          ; INDX=ACCD=A:B=XYZW (HEX)
                    puld                          ; put everything back
                    puly
                    rts

;*******************************************************************************
; Interrupt Service Routines
;*******************************************************************************

;*******************************************************************************
; RTI interrupt: Real Time Clock
; updates the Time data structure on each interrupt
; Currently set up for 16.384ms RTI rate
; Accuracy: 1s = 0.999424s, or: slow by 49.7664s per 24h period

RTI_Handler         proc
                    lda       #$40
                    sta       TFLG2               ; clear interrupt flag
          ;-------------------------------------- ; update ticks
                    inc       ticks               ; increment ticks
                    lda       ticks
                    cmpa      #1000000/16384      ; one second based on 16.384ms ticks
                    blo       Done@@              ; if lower, exit
          ;-------------------------------------- ; ticks rollover and seconds update
                    clr       ticks               ; if not, set ticks=0
                    inc       seconds             ; increment seconds
                    lda       seconds
                    cmpa      #60                 ; compare with 60 seconds
                    blo       Done@@              ; if lower, exit
          ;-------------------------------------- ; seconds rollover and minutes update
                    clr       seconds             ; set seconds=0
                    inc       minutes             ; increment minutes
                    lda       minutes
                    cmpa      #60                 ; compare with 60 minutes
                    blo       Done@@              ; if lower, exit
          ;-------------------------------------- ; minutes rollover and seconds update
                    clr       minutes             ; minutes=0
                    inc       hours               ; increment hours
                    lda       hours
                    cmpa      #24                 ; compare with 24 hours
                    blo       Done@@              ; if lower, exit
          ;-------------------------------------- ; hours rollover
                    clr       hours               ; hours=0
Done@@              rti                           ; return from interrupt

;*******************************************************************************
; Constants & Strings
;*******************************************************************************

; Kernel strings

msg_done            fcs       'Done.',CR,LF
msg_wrm_strt        fcs       'MONITOR 01'
msg_mem_free        fcs       ' bytes free.'
msg_y32             fcs       'mx1 detected.',CR,LF
msg_error           fcs       'Error.',CR,LF
msg_help            fcc       'Commands: [d x h s t m j l]'
msg_newline         fcs       CR,LF
msg_prompt          fcs       '[monitor]> '
msg_reset           fcs       'Reset.',CR,LF
msg_lmesg           fcs       'Done.'
msg_l_begin         fcs       'Begin Loading.'
msg_l_bytes         fcs       ' bytes loaded.'

; On-line Documentation
; Explains CI commands.

msg_no_help         fcs       'No help entry.'
msg_d_help          fcs       'd [xxxx] [yy] : Dump. [x]=hex start addx, [y]=# of 16-byte blocks.'
msg_x_help          fcs       'x [xxxx] : Xamine. Enter byte at addx [x].'
msg_s_help          fcs       's : Set clock in hex. 24-h format [HH:MM:SS]. :SS opt.'
msg_h_help          fcs       'h [x] : Help on command [x].'
msg_t_help          fcs       't : Display time in 24-h format. SEE s to set.'
msg_j_help          fcs       'j [xxxx] : Jump. Load specified address into PC.'
msg_usage           fcs       'usage: h [d x h s t m j l].'

;*******************************************************************************
; Temporary Jump Table to display subroutine hex addresses
; useful for writing programs using 'x' command
;*******************************************************************************
                    org       $FF00

j_crlf              dw        CrLf
j_getchar           dw        GetChar
j_outchar           dw        OutChar
j_h2a               dw        HexToASCII
j_a2h                                             ;same as j_chr2hex
j_chr2hex           dw        CharToHex           ;same as j_a2h
j_getbyt            dw        GetByte
j_outbyt            dw        OutByte
j_hex2bcd           dw        HexToBcd            ;WAS: j_h2bcd (renamed due to different in/out parms)

                    #Export   j_crlf,j_getchar,j_outchar,j_h2a,j_chr2hex,j_a2h
                    #Export   j_getbyt,j_outbyt,j_hex2bcd

;*******************************************************************************
; Warm Start Jump instruction
; User programs include a JMP $FFD3 to return control to kernel
;*******************************************************************************

                    org       $FFD3
                    jmp       WarmStart

;*******************************************************************************
                    #VECTORS                      ; Interrupt Vector Table (unused cause Reset)
;*******************************************************************************
                    org       $FFD6

SCI_VECT            dw        Start
SPI_VECT            dw        Start
PAI_VECT            dw        Start
PAO_VECT            dw        Start
TOF_VECT            dw        Start

TOC5_VECT           dw        Start
TOC4_VECT           dw        Start
TOC3_VECT           dw        Start
TOC2_VECT           dw        Start
TOC1_VECT           dw        Start
TIC3_VECT           dw        Start
TIC2_VECT           dw        Start
TIC1_VECT           dw        Start

RTI_VECT            dw        RTI_Handler
IRQ_VECT            dw        Start
XIRQ_VECT           dw        Start
SWI_VECT            dw        Start
TRAP_VECT           dw        Start
COP_FAIL_VECT       dw        Start
COP_CMF_VECT        dw        Start
RESET_VECT          dw        Start

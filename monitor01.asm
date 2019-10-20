;*******************************************************************************
; MONITOR
; 06.08.2013 T Schaer
; Based on KERNEL1E.ASM Nov 5 2004
; Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
                    #ExtraOn
;-------------------------------------------------------------------------------
; Some useful I/O registers
;-------------------------------------------------------------------------------

SCSR                equ       $102E
SCDR                equ       $102F
SCCR2               equ       $102D
BAUD                equ       $102B
DDRD                equ       $1009
TMSK2               equ       $1024
TFLG2               equ       $1025
PACTL               equ       $1026
BUFSIZE             equ       $10                 ; keyboard buffer size = 16 chars

CR                  equ       13                  ; Carriage Return
LF                  equ       10                  ; Line Feed
BACKSPACE           equ       8

;*******************************************************************************
                    #RAM                          ; Variables
;*******************************************************************************
                    org       $0080
; Real-time clock
hours               rmb       1
minutes             rmb       1
seconds             rmb       1
ticks               rmb       1
; Used by GETBYT - get two ASCII chars and convert to 1 hex byte
conv                rmb       1                   ; 8-bit hex value
; Used by H2BCD
hundreds            rmb       1                   ; stores 100's digit
decimal             rmb       1                   ; stores 10's and 1's in packed BCD
; For d [xxxx][y] command
top8                rmb       1                   ; upper 8 bits of dump address
low8                rmb       1                   ; lower 8 bits of dump address
bytes               rmb       1                   ; # of 16-byte paragraphs to display
dump                rmb       16                  ; save current paragraph for ASCII conv.
sixteen             rmb       1                   ; loop counter for within a paragraph
; Used by EDIT for command line & args
buffer              rmb       BUFSIZE
; Used by BUFFER2HEX: points to first cmd-line argument
.arg                rmb       2
; Used by 'shell'
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
                    ldx       #reset              ; print reset message
                    jsr       outstr
                    ldx       #help               ; show available commands
                    jsr       outstr
                    lda       #$42
                    sta       $200                ; attempt to store to expansion RAM
                    cmpa      $200                ; if $42 retrieved, RAM present
                    beq       _32K@@
                    clr       yes_mx1             ; yes_mx1=false
                    ldx       #.faa               ; load address of faa ptr
                    inx:2
                    stx       .faa                ; store first available RAM addx
                    lda       .faa                ; display upper byte of faa
                    jsr       outbyt
                    lda       .faa+1              ; display lower byte of faa
                    jsr       outbyt
                    bra       WarmStart
_32K@@              lda       #1
                    sta       yes_mx1             ; yes_mx1=true
                    ldx       #y32                ; indicate mx1 detected
                    jsr       outstr
;                   bra       WarmStart

;*******************************************************************************
; Command Interpreter loop

WarmStart           proc
                    jsr       crlf
                    ldx       #wrm_strt           ; display warm start message
                    jsr       outstr
MainLoop            jsr       crlf
                    ldx       #prompt
                    jsr       outstr              ; display prompt
                    jsr       edit                ; get command line string
                    jsr       crlf
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
                    jeq       set_time
          ;--------------------------------------
                    cmpa      #'t'
                    jeq       display_time
          ;--------------------------------------
                    cmpa      #'m'
                    jeq       free_ram
          ;--------------------------------------
                    cmpa      #'j'
                    jeq       j_ump
          ;--------------------------------------
                    cmpa      #'l'
                    jeq       l_oad
          ;--------------------------------------
                    cmpa      #'?'
                    bne       Help@@
                    ldx       #error              ; unrecognized command, error
                    bra       Print@@
          ;--------------------------------------
Help@@              ldx       #help
Print@@             jsr       outstr
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
; Re-written to use BUFFER2HEX and the keyboard buffer to pass args
; Optimized Jan 11 2001 to use .arg

d_ump               proc
                    ldx       #buffer             ; point to keyboard buffer
                    jsr       buffer2hex          ; convert arguments to hex
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
                    jsr       crlf                ; start new line
                    lda       top8
                    jsr       outbyt              ; display top 8 bits of address
                    lda       low8
                    jsr       outbyt              ; display low 8 bits of address
                    lda       #' '
                    jsr       outchar             ; display space after address
                    lda       #16                 ; init loop counter to 16
                    sta       sixteen
                    ldx       top8                ; load starting address of dump
                    ldy       #dump               ; pointer to temp. storage
Read@@              lda       ,x                  ; get byte from memory
                    sta       ,y                  ; save in temp. storage
                    jsr       outbyt              ; display it
                    lda       #' '
                    jsr       outchar             ; space between entries
                    inx                           ; point to next source byte
                    iny                           ; point to next temp. byte
                    dec       sixteen             ; decrement loop counter
                    bne       Read@@              ; quit when sixteen = 0
                    stx       top8                ; save updated address
                    dec       bytes               ; decrement # of 16 byte paragraphs
                    lda       #' '
                    jsr       outchar
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
Ascii@@             jsr       outchar             ; display ASCII memory byte
                    iny                           ; point to next source byte
                    dec       sixteen             ; decrement loop counter
                    bne       Dump@@              ; loop while sixteen !=0
                    bra       Loop@@              ; display another 16-byte paragraph
Done@@              jmp       MainLoop

;*******************************************************************************
; X_AMINE: view/change memory byte
; Useful for setting registers, flags, etc.
; Status: stub function written Jan 09 2001
; First version Jan 11 2001

x_amine             proc
                    ldx       #buffer
                    jsr       buffer2hex          ; hex-ify arguments
                    ldy       .arg
                    ldx       ,y
Loop@@              stx       top8                ; use same vars as d_ump
                    lda       top8                ; display upper 8 bits of address
                    jsr       outbyt
                    lda       low8                ; display lower 8 bits of address
                    jsr       outbyt
                    lda       #' '
                    jsr       outchar             ; display space
                    lda       ,x                  ; get current memory value
                    jsr       outbyt
                    lda       #' '
                    jsr       outchar
                    jsr       getbyt              ; wait for user input
                    bcs       Abort@@             ; quit if nothing entered
                    sta       ,x                  ; save byte in location
                    inx                           ; increment memory pointer
                    lda       #CR                 ; display new line
                    jsr       outchar             ; cannot use CRLF() b/c it fucks X
                    lda       #LF
                    jsr       outchar
                    bra       Loop@@              ; do it all again
Abort@@             jmp       MainLoop

;*******************************************************************************
; J_UMP: begin executing program code at a user-specified address
; The last link in creating a complete ROM monitor
; jump address is supplied as command-line argument
; IMPORTANT: Do not use JSR to call this function!

j_ump               proc
                    ldx       #buffer             ; point to keyboard buffer
                    jsr       buffer2hex          ; convert args to hexadecimal
                    ldy       .arg                ; get location of first argument
                    beq       AbortJmp@@          ; no address supplied, abort
                    ldx       ,y                  ; load jump address into X
                    jmp       ,x                  ; jump to address in X
AbortJmp@@          jmp       MainLoop            ; return on error

;*******************************************************************************
; SET_TIME: alter value of Time data structure to something other
; than the power-on test values
; option to quit without entering seconds value by pressing enter
; data-format conversion routine still missing (BCD2H)
; altered Jan 11 2001 to use revised GETBYT

set_time            proc
                    clr       TMSK2               ; turn off RTI
                    jsr       getbyt              ; get the hours digits (echo automatically)
                    bcs       Done@@              ; if no data entered, quit
;                   jsr       bcd2h               ; convert to hex from user-input BCD
                    sta       hours               ; store in data structure
                    lda       #':'                ; display the colon separator
                    jsr       outchar
                    jsr       getbyt              ; get minutes
;                   jsr       bcd2h               ; convert to hex
                    sta       minutes
                    lda       #':'                ; display the colon separator
                    jsr       outchar
                    jsr       getchar             ; get single char (10's of seconds)
                    cmpa      #CR                 ; check if it's the return character
                    beq       Done@@              ; if so, job is done
                    jsr       outchar             ; echo character
                    jsr       chr2hex             ; convert to hex
                    lsla:4
                    tab                           ; copy ACCA to ACCB
                    jsr       getchar             ; get 1's of seconds
                    jsr       outchar             ; echo
                    jsr       chr2hex             ; convert to hex
                    aba                           ; combine digits in ACCA
                    sta       seconds             ; finally store it in data structure
Done@@              lda       #$40
                    sta       TMSK2               ; turn RTI back on
                    jmp       MainLoop

;*******************************************************************************
; DISPLAY_TIME: prints the current time to the screen.
; no variables passed or returned. ACCA altered.
; Displays time in decimal format.

display_time        proc
                    lda       hours               ; get hours
                    bsr       Show@@
                    lda       minutes             ; get minutes
                    bsr       Show@@
                    lda       seconds             ; get seconds
                    jsr       h2bcd               ; convert to BCD
                    jsr       outbyt
                    jmp       MainLoop

Show@@              jsr       h2bcd               ; convert to BCD
                    jsr       outbyt              ; ascii-fy and output
                    lda       #':'                ; colon separator
                    jmp       outchar             ; display it

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
                    ldx       #d_help             ; load help string for 'd'
                    bra       Print@@             ; display string
          ;--------------------------------------
x?@@                cmpa      #'x'                ; is it 'x'?
                    bne       s?@@                ; if not, skip around it
                    ldx       #x_help             ; load string
                    bra       Print@@             ; display it
          ;--------------------------------------
s?@@                cmpa      #'s'                ; is it 's'?
                    bne       h?@@                ; if not, skip around it
                    ldx       #s_help             ; load string
                    bra       Print@@             ; display it
          ;--------------------------------------
h?@@                cmpa      #'h'                ; is it 'h'?
                    bne       t?@@                ; if not, skip around it
                    ldx       #h_help             ; load string
                    bra       Print@@             ; display it
          ;--------------------------------------
t?@@                cmpa      #'t'                ; : t?
                    bne       j?@@                ; if not, skip around it
                    ldx       #t_help             ; load string
                    bra       Print@@             ; display
          ;--------------------------------------
j?@@                cmpa      #'j'                ; j?
                    bne       Help@@              ; if not, skip around it
                    ldx       #j_help
                    bra       Print@@
          ;--------------------------------------
Help@@              ldx       #no_help            ; no help entry
                    bra       Print@@
          ;--------------------------------------
Usage@@             ldx       #usage              ; display usage help
Print@@             jsr       outstr
                    jsr       crlf
                    jmp       MainLoop

;*******************************************************************************
; L_OAD: downline loader for S19 files generated by AS11
; hacking-around first version, Jan 17 2001
; completely re-written Nov. 5 2004

l_oad               proc
                    jsr       crlf                ; display 'begin loading'
                    ldx       #l_begin
                    jsr       outstr
                    clr       bytes               ; init program length counter
          ;--------------------------------------
Start@@             jsr       getchar             ; get the S
                    cmpa      #'S'
                    beq       S@@                 ; if yes, start new S19 record
                    bra       Start@@             ; if no, keep trying
          ;--------------------------------------
S@@                 jsr       getchar
                    cmpa      #'1'                ; S1 record
                    beq       S1@@
                    cmpa      #'9'                ; S9 record: S9030000FC always
                    beq       S9@@
                    bra       Done@@              ; if neither, quit
          ;--------------------------------------
S1@@                ldx       #buffer             ;** Start processing an S1 record**
                    bsr       getchar             ; ACCA=first byte of length field
                    sta       ,x                  ; buffer[0]=ACCA
                    bsr       getchar             ; ACCA=second byte of length field
                    sta       1,x                 ; buffer[1]=ACCA
                    jsr       dchr2hex            ; ACCA=hex length field value
                    tab                           ; ACCB=hex length (loop ctr)
                    subb      #3                  ; correct length for addx & CRC fields
                    ldx       #buffer             ; x -> buffer[0]
                    bsr       getchar             ; address byte 1
                    sta       ,x                  ; buffer[0]=ASCII address byte 1
                    bsr       getchar             ; address byte 2
                    sta       1,x                 ; buffer[1]=ASCII address byte 2
                    bsr       getchar             ; address byte 3
                    sta       2,x                 ; buffer[2]=ASCII address byte 3
                    bsr       getchar             ; address byte 4
                    sta       3,x                 ; buffer[3]=ASCII address byte 4
                    jsr       qchr2hex            ; x=program load address
                    pshx                          ; copy X into Y
                    puly                          ; y=program load address
                    ldx       #buffer             ; x -> buffer[0]
Loop@@              bsr       getchar             ; ASCII program byte 1
                    sta       ,x                  ; buffer[0]=ASCII program byte 1
                    bsr       getchar             ; ASCII program byte 2
                    sta       1,x                 ; buffer[1]=ASCII program byte 2
                    jsr       dchr2hex            ; ACCA=object code byte
                    sta       ,y                  ; store code byte in RAM
                    inc       bytes               ; advance the program length counter
                    iny                           ; point to next code byte location
                    decb                          ; loop ctr --
                    beq       Start@@             ; loop ctr=0, get next S19 record
                    bra       Loop@@              ; else finish this record
          ;--------------------------------------
S9@@                bsr:4     getchar             ; get the 03 00 00 FC
                    bsr       crlf
                    ldx       #lmesg              ; display 'Done.'
                    bsr       outstr
                    bsr       crlf
                    lda       .faa                ; display upper byte of faa
                    bsr       outbyt
                    lda       .faa+1              ; display lower byte of faa
                    bsr       outbyt
                    bsr       crlf                ; new line
                    lda       bytes               ; ACCA = loaded program length
                    bsr       h2bcd               ; HUNDREDS, ACCA=tens:ones
                    tab                           ; ACCB = tens:ones
                    lda       hundreds            ; ACCA = 100's digit
                    bsr       outbyt
                    tba                           ; ACCA = 10's:1's digits
                    bsr       outbyt
                    ldx       #l_bytes            ; display ' bytes loaded.'
                    bsr       outstr
                    bsr       crlf
Done@@              jmp       MainLoop

;*******************************************************************************
; Subroutines for writing sub-programs and useful user functions
;*******************************************************************************

;*******************************************************************************
; CRLF: print a new line to the screen
; occurs so often it warrants its own subroutine

crlf                proc
                    ldx       #newline
                    bra       outstr

;*******************************************************************************
; GETCHAR: a polling loop that gets a character from the serial port
; returns char in ACCA

getchar             proc
Loop@@              lda       SCSR                ; get serial status register
                    anda      #$20                ; mask off all bits but RDRF flag
                    beq       Loop@@              ; keep checking if RDRF=0
                    lda       SCDR                ; load received character
                    rts

;*******************************************************************************
; OUTCHAR: sends one character to serial port receives character in ACCA

outchar             proc
Loop@@              tst       SCSR                ; read serial status register
                    bpl       Loop@@              ; loop until TDRE = 1
                    sta       SCDR                ; send char
                    rts

;*******************************************************************************
; OUTSTR: sends a string of characters to serial port
; address of string is passed in X

outstr              proc
Loop@@              lda       ,x                  ; get char in string
                    beq       Done@@              ; exit if null terminator
                    bsr       outchar             ; output char
                    inx                           ; point to next char
                    bra       Loop@@
Done@@              equ       :AnRTS

;*******************************************************************************
; H2A: Converts an 8-bit hex number into two ASCII characters
; Hex number passed in ACCA, lower ASCII returned in ACCB

h2a                 proc
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
; GETBYT: Gets two ASCII chars from serial port and converts to 8-bit hex
; Gets its own chars from the serial port, returns hex in CONV & ACCA
; Carry flag set if nothing entered

getbyt              proc
                    bsr       getchar             ; get char from serial port
                    cmpa      #CR                 ; is it enter?
                    beq       Abort@@             ; user is aborting
                    bsr       outchar             ; echo it
                    jsr       chr2hex             ; convert to 4-bit hex
                    lsla:4                        ; shift it left 4 times
                    sta       conv                ; save it
                    bsr       getchar             ; get second char
                    cmpa      #CR                 ; is it enter?
                    beq       Abort@@
                    bsr       outchar             ; echo it
                    jsr       chr2hex             ; convert to 4-bit hex
                    adda      conv                ; add first char to it
                    sta       conv                ; save full 8-bit hex
                    bra       Done@@
Abort@@             sec                           ; return error code
Done@@              rts

;*******************************************************************************
; OUTBYT: Converts a hex number to ASCII chars and outputs it
; Hex is passed in ACCA. Returns nothing.
; fixed Jan 10 2001 to take advantage of OUTCHAR pushing B to stack

outbyt              proc
                    bsr       h2a
                    bsr       outchar
                    tba
                    bra       outchar

;*******************************************************************************
; H2BCD: converts 8-bit hex to 12-bit BCD
; call this routine just before OUTBYT to display decimal instead of hex
; hex is passed in ACCA, hundreds digit is stored in HUNDREDS
; tens and ones are stored in packed format in DECIMAL
; ACCA also returns tens and ones

h2bcd               proc
                    tab                           ; put into lower byte of D
                    clra                          ; clear upper byte of D
                    ldx       #100                ; load divisor
                    idiv                          ; X=hundreds, D=remainder
                    xgdx                          ; D=100's digit (actually in B)
                    stb       hundreds            ; store third digit of number
                    xgdx                          ; put remainder back in D
                    ldx       #10                 ; load divsor
                    idiv                          ; X=10's digit, D=1's
                    stb       decimal             ; store 1's digit
                    xgdx                          ; D=10's digit (actually in B)
                    lslb:4                        ; shift 10's into upper 4 bits
                    addb      decimal             ; combine 10's and 1's in ACCB
                    stb       decimal             ; store BCD
                    tba                           ; make decimal available in ACCA
                    rts

;*******************************************************************************
; EDIT: the input routine for the command interpreter
; One line of 15 chars max (to change, alter BUFFER length)
; Last entry in string is reserved for the NULL ($00)
; ENTER terminates routine, BACKSPACE allows editing

edit                proc
                    ldx       #buffer             ; X points to command-line buffer
                    ldb       #BUFSIZE-1          ; set loop counter=15 chars max
GetChar@@           jsr       getchar             ; get a character from keyboard
                    cmpa      #CR                 ; check for enter
                    beq       Done@@              ; quit if it is
                    cmpa      #BACKSPACE          ; check for backspace character
                    beq       Backspace@@         ; do the thing if it is
                    sta       ,x                  ; store character in buffer
                    jsr       outchar
                    inx                           ; point to next location in buffer
                    decb                          ; decrement loop counter
                    beq       Done@@              ; exit on loop counter=0
                    bra       GetChar@@           ; otherwise repeat
Backspace@@         cmpb      #BUFSIZE-1          ; is backspace the first char typed?
                    beq       GetChar@@           ; ignore it
                    jsr       outchar             ; try and move cursor back one
                    lda       #' '                ; clear that letter
                    jsr       outchar
                    lda       #BACKSPACE          ; try and move cursor back one
                    jsr       outchar
                    dex                           ; move buffer pointer back by one
                    bra       GetChar@@           ; get next char
Done@@              clr       ,x                  ; terminate string with NULL
                    rts

;*******************************************************************************
; BUFFER2HEX: Converts any command-line arguments in the keyboard
; buffer to hexadecimal
; from ASCII. Starts converting after the first space is found in
; the buffer - this allows future commands of more than a single letter
; leaves spaces as separators for the arguments. .arg points to
; the first converted item. To call: address of buffer must be in X
; Written in Florida December 2000
; revised Jan 11 2001 to add .arg

buffer2hex          proc
Loop@@              lda       ,x                  ; get char in buffer
                    beq       Done@@              ; exit if end of buffer ($00) hit
                    cmpa      #' '                ; look for space character
                    beq       Space@@             ; start converting if space found
                    inx                           ; point to next char
                    bra       Loop@@
Space@@             inx                           ; skip space character
                    stx       .arg                ; save location of first argument
                    ldy       .arg                ; Copy X into Y: X=src ptr, Y=dest ptr
Hexify@@            lda       ,x                  ; get first char after space
                    beq       Done@@              ; exit if null detected
                    cmpa      #' '                ; is it space character?
                    beq       SkipSpace@@         ; if so, preserve it as a separator
                    bsr       chr2hex             ; convert char to hex
                    sta       ,y                  ; save in buffer
                    inx                           ; point to next source char
                    lda       ,x                  ; get next source char
                    beq       Done@@              ; quit if null detected
                    bsr       chr2hex             ; convert this to hex also
                    ldb       ,y                  ; get previous converted char
                    lslb:4                        ; combine the two chars
                    aba                           ; full 8-bit hex number
SkipSpace@@         sta       ,y                  ; store in buffer
                    inx                           ; next source entry
                    iny                           ; next dest entry
                    bra       Hexify@@            ; continue converting
Done@@              sta       ,x                  ; place NULL at end of converted buffer
                    rts

;*******************************************************************************
; FREE_RAM: displays the free RAM in the system. Takes no arguments
; Works only for unexpanded system
; Uses the first available address pointer .faa as bottom of RAM
; and $FF as top of RAM - change this if expanded
; Written Jan. 13 2001

free_ram            proc
                    jsr       crlf                ; new line
                    lda       #$FF                ; put top of RAM in ACCA
                    suba      .faa+1              ; subtract highest free RAM address
                    jsr       h2bcd               ; convert to decimal
                    jsr       outbyt              ; display free memory bytes
                    ldx       #mem_free
                    jsr       outstr
                    jmp       MainLoop

;*******************************************************************************
; CHR2HEX: convert one ASCII byte into 4-bit hex
; ASCII passed in A, value returned in A
; Input                         : ACCA=aaaa aaaa (ASC)
; Output                        : ACCA=0000 hhhh (HEX)
; Parameter passing             : ACCA (value to be converted)
; Registers used internally     : ACCA

chr2hex             proc
                    suba      #'0'
                    cmpa      #9
                    bls       Done@@
                    suba      #7
Done@@              rts

;*******************************************************************************
; DCHR2HEX: convert 2 ASCII bytes into 8-bit hex
; X points to a buffer containing two ASCII chars
; A holds return value
;
; Input                         : INDX -> X:Y (2 ASCII chars)
; Output                        : ACCA = XY   (1 hex byte)
; Parameter passing             : INDX (pointer to buffer)
;                               : ACCA (return value)
; Registers used internally     : ACCA, ACCB, INDX

dchr2hex            proc
                    pshb                          ; preserve ACCB
                    lda       ,x                  ; ACCA = aaaa aaaa (ASC)
                    bsr       chr2hex             ; ACCA = 0000 hhhh (HEX)
                    tab                           ; ACCB = 0000 hhhh (HEX)
                    lda       1,x                 ; ACCA = bbbb bbbb (ASC)
                    bsr       chr2hex             ; ACCA = 0000 iiii (HEX)
                    lslb:4                        ; ACCB = hhhh 0000
                    aba                           ; ACCA = hhhh iiii (HEX)
                    pulb                          ; restore ACCB
                    rts

;*******************************************************************************
; QCHR2HEX: convert 4 ASCII bytes into 2 hex bytes
; X points to a buffer containing 4 ASCII chars
; X holds 2-byte hex return value
;**
; Input                         : INDX -> X:Y:Z:W (4 ASCII chars)
; Output                        : INDX =  XYZW    (1 hex byte)
; Parameter Passing             : INDX (pointer to buffer)
; Registers used internally     : ACCA, ACCB, INDX, INDY

qchr2hex            proc
                    pshy                          ; save INDY
                    pshd
                    pshx                          ; copy X into Y
                    puly                          ; Y = buffer base address
                    bsr       dchr2hex            ; ACCA = XY (HEX)
                    tab                           ; ACCB = XY (HEX)
                    pshy                          ; copy Y into X
                    pulx                          ; X = buffer base address
                    inx:2                         ; point to next two chars
                    bsr       dchr2hex            ; ACCA = ZW (HEX)
                    psha                          ; swap ACCA & ACCB
                    pshb                          ; since ACCD = ACCA:ACCB
                    puld                          ; ACCA = XY, ACCB = ZW
                    xgdx                          ; INDX=ACCD=A:B=XYZW (HEX)
                    puld                          ; put everything back
                    puly
                    rts

;*******************************************************************************
; Interrupt Service Routines
;*******************************************************************************

;*******************************************************************************
; RTI interrupt: real time clock
; updates the Time data structure on each interrupt
; Currently set up for 16.384ms RTI rate
; Accuracy: 1s = 0.999424s, or: slow by 49.7664s per 24h period

RTI_Handler         proc
                    lda       #$40
                    sta       TFLG2               ; clear interrupt flag
          ;-------------------------------------- ; update ticks
                    lda       ticks               ; get ticks
                    inca                          ; increment
                    sta       ticks               ; store
                    cmpa      #61                 ; compare with 61 (16.384ms * 61 = 1s)
                    blo       Done@@              ; if lower, exit
          ;-------------------------------------- ; ticks rollover and seconds update
                    clr       ticks               ; if not, set ticks=0
                    lda       seconds             ; get seconds
                    inca                          ; increment
                    sta       seconds             ; store
                    cmpa      #60                 ; compare with 60 seconds
                    blo       Done@@              ; if lower, exit
          ;-------------------------------------- ; seconds rollover and minutes update
                    clr       seconds             ; set seconds=0
                    lda       minutes             ; get minutes
                    inca                          ; increment
                    sta       minutes             ; store
                    cmpa      #60                 ; compare with 60 minutes
                    blo       Done@@              ; if lower, exit
          ;-------------------------------------- ; minutes rollover and seconds update
                    clr       minutes             ; minutes=0
                    lda       hours               ; get hours
                    inca                          ; increment
                    sta       hours               ; store
                    cmpa      #24                 ; compare with 24 hours
                    blo       Done@@              ; if lower, exit
          ;-------------------------------------- ; hours rollover
                    clr       hours               ; hours=0
Done@@              rti                           ; return from interrupt

;** Constants & Strings
; Kernel strings
done                fcs       'Done.',CR,LF
wrm_strt            fcs       'MONITOR 01'
mem_free            fcs       ' bytes free.'
y32                 fcs       'mx1 detected.',CR,LF
error               fcs       'Error.',CR,LF
help                fcc       'Commands: [d x h s t m j l]'
newline             fcs       CR,LF
prompt              fcs       '[monitor]> '
reset               fcs       'Reset.',CR,LF
lmesg               fcs       'Done.'
l_begin             fcs       'Begin Loading.'
l_bytes             fcs       ' bytes loaded.'

;** On-line Documentation
; Explains CI commands.

no_help             fcs       'No help entry.'
d_help              fcs       'd [xxxx] [yy] : Dump. [x]=hex start addx, [y]=# of 16-byte blocks.'
s_help              fcs       's : Set clock in hex. 24-h format [HH:MM:SS]. :SS opt.'
x_help              fcs       'x [xxxx] : Xamine. Enter byte at addx [x].'
h_help              fcs       'h [x] : Help on command [x].'
t_help              fcs       't : Display time in 24-h format. SEE s to set.'
m_help              fcs       'm : show free system RAM.'
j_help              fcs       'j [xxxx] : Jump. Load specified address into PC.'
usage               fcs       'usage: h [d x h s t m j l].'

;** Temporary Jump Table to display subroutine hex addresses
; useful for writing programs using 'x' command
                    org       $FF00

j_crlf              dw        crlf
j_getchar           dw        getchar
j_outchar           dw        outchar
j_h2a               dw        h2a
j_chr2hex           dw        chr2hex             ;same as j_a2h
j_getbyt            dw        getbyt
j_outbyt            dw        outbyt
j_h2bcd             dw        h2bcd

;*******************************************************************************
;** Warm Start Jump instruction
; User programs include a JMP $FFD3 to return control to kernel

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

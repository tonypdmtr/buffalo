; MONITOR
; 06.08.2013 T Schaer
; Based on KERNEL1E.ASM Nov 5 2004

; Some useful I/O registers
SCSR                equ       $102E
SCDR                equ       $102F
SCCR2               equ       $102D
BAUD                equ       $102B
DDRD                equ       $1009
TMSK2               equ       $1024
TFLG2               equ       $1025
PACTL               equ       $1026
BUFSIZE             equ       $10                 ; keyboard buffer size = 16 chars

; ----- Variables

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
arg_ptr             rmb       2
; Used by "shell"
yes_mx1             rmb       1                   ; MX1 installed. 1=yes, 0=no
faa_ptr             rmb       2                   ; first available RAM address

; ----- Program
                    org       $F800
strt                sei
                    lds       #$7F                ; put stack in bottom half of RAM
; SCI initialization
                    lda       #$02                ; make TxD pin an output
                    sta       DDRD
                    lda       #$30                ; 4800 baud
                    sta       BAUD
                    lda       #$0C                ; TE=1, RE=1, no interrupts
                    sta       SCCR2
; RTI Initialization
                    lda       #$02                ; RTI rate = 16.384ms
                    sta       PACTL
                    lda       #$40                ; Enable RTI interrupt
                    sta       TMSK2
                    lda       #$40                ; Start RTI-ing
                    sta       TFLG2
; Initialize Time data structure: initial time 23:59:55:00
                    lda       #23                 ; init hours to 23
                    sta       hours
                    lda       #59                 ; init minutes to 59
                    sta       minutes
                    lda       #55                 ; init seconds to 55
                    sta       seconds
                    clra                          ; start ticks at zero
                    sta       ticks
                    cli                           ; enable interrupts
; Miscellaneous initializations
                    ldx       #reset              ; print reset message
                    jsr       outstr
                    ldx       #help               ; show available commands
                    jsr       outstr
                    lda       #$42
                    sta       $200                ; attempt to store to expansion RAM
                    lda       $200                ; read it
                    cmpa      #$42                ; if $42 retrieved, RAM present
                    beq       yes32K
no_32K              clr       yes_mx1             ; yes_mx1=false
                    ldx       #faa_ptr            ; load address of faa ptr
                    inx
                    inx
                    stx       faa_ptr             ; store first available RAM addx
                    lda       faa_ptr             ; display upper byte of faa
                    jsr       outbyt
                    lda       faa_ptr+1           ; display lower byte of faa
                    jsr       outbyt
                    bra       warm_start

yes32K              lda       #$01
                    sta       yes_mx1             ; yes_mx1=true
                    ldx       #y32                ; indicate mx1 detected
                    jsr       outstr

; Command Interpreter loop
warm_start          jsr       crlf
                    ldx       #wrm_strt           ; display warm start message
                    jsr       outstr
main                jsr       crlf
                    ldx       #prompt
                    jsr       outstr              ; display prompt
                    jsr       edit                ; get command line string
                    jsr       crlf
                    ldx       #buffer             ; look at command
                    lda       0,x
                    beq       main                ; ignore empty buffer
; Command interpreter
m_d                 cmpa      #'d'
                    bne       m_x
                    bra       d_ump

m_x                 cmpa      #'x'
                    bne       m_h
                    jmp       x_amine

m_h                 cmpa      #'h'
                    bne       m_s
                    jmp       h_elp

m_s                 cmpa      #'s'
                    bne       m_t
                    jmp       set_time

m_t                 cmpa      #'t'
                    bne       m_m
                    jmp       display_time

m_m                 cmpa      #'m'
                    bne       m_j
                    jmp       free_ram

m_j                 cmpa      #'j'
                    bne       m_l
                    jmp       j_ump

m_l                 cmpa      #'l'
                    bne       m_q
                    jmp       l_oad

m_q                 cmpa      #'?'
                    bne       m__
                    ldx       #error              ; unrecognized command, error
                    bra       m_e

m__                 ldx       #help
m_e                 jsr       outstr
                    bra       main

;** Sub - Programs
; The commands that are "shell" commands

; D_UMP: Dump memory contents to screen in hex & ASCII format
; displays paragraphs of 16 memory locations, also shows ASCII
; representation of memory location if its value is greater
; than $30 and less than $7F inclusive.
; ARGUMENTS: 4-digit hex starting address, optional 2-digit number
; of paragraps to display, also in hex.
; Re-written to use BUFFER2HEX and the keyboard buffer to pass args
; Optimized Jan 11 2001 to use arg_ptr
d_ump               ldx       #buffer             ; point to keyboard buffer
                    jsr       buffer2hex          ; convert arguments to hex
                    ldy       arg_ptr             ; point to location in buffer
                    ldx       0,y                 ; get argument [xxxx]
                    stx       top8                ; save as dump start address
                    iny                           ; skip upper byte of start address
                    iny                           ; skip lower byte of start address
                    lda       0,y                 ; examine next char in buffer
                    cmpa      #$20                ; is it a space?
                    beq       args                ; if so, there may be more args
no_arg              lda       #$01                ; set bytes = 1 * 16
                    sta       bytes
                    bra       loop1

args                iny                           ; skip space after [xxxx]
                    lda       0,y                 ; read [yy]
                    beq       no_arg              ; if it is NULL, there are no args
                    sta       bytes               ; if not, save as # of paragraphs
loop1               lda       bytes
                    bne       ok
                    bra       x_optd              ; if bytes = 0, exit

ok                  jsr       crlf                ; start new line

loop                lda       top8
                    jsr       outbyt              ; display top 8 bits of address
                    lda       low8
                    jsr       outbyt              ; display low 8 bits of address
                    lda       #$20
                    jsr       outchar             ; display space after address
                    lda       #$10                ; init loop counter to 16
                    sta       sixteen
                    ldx       top8                ; load starting address of dump
                    ldy       #dump               ; pointer to temp. storage
read                lda       0,x                 ; get byte from memory
                    sta       0,y                 ; save in temp. storage
                    jsr       outbyt              ; display it
                    lda       #$20
                    jsr       outchar             ; space between entries
                    inx                           ; point to next source byte
                    iny                           ; point to next temp. byte
                    dec       sixteen             ; decrement loop counter
                    bne       read                ; quit when sixteen = 0
                    stx       top8                ; save updated address
                    dec       bytes               ; decrement # of 16 byte paragraphs
                    lda       #$20
                    jsr       outchar
                    ldy       #dump               ; prepaer to output ASCII rendition
                    lda       #$10
                    sta       sixteen             ; re init loop counter
a_dump              lda       0,y
                    cmpa      #$20                ; lower than $20?
                    bhs       no_ctrl             ; branch if not a control char.
                    lda       #'.'                ; if yes replace with '.'
no_ctrl             cmpa      #$7F
                    bls       ok_asc              ; $7F or lower is ok
                    lda       #'.'                ; if over $7F replace with '.'
ok_asc              jsr       outchar             ; display ASCII memory byte
                    iny                           ; point to next source byte
                    dec       sixteen             ; decrement loop counter
                    bne       a_dump              ; loop while sixteen !=0
                    bra       loop1               ; display another 16-byte paragraph

x_optd              jmp       main

; X_AMINE: view/change memory byte
; Useful for setting registers, flags, etc.
; Status: stub function written Jan 09 2001
; First version Jan 11 2001
x_amine             ldx       #buffer
                    jsr       buffer2hex          ; hex-ify arguments
                    ldy       arg_ptr
                    ldx       0,y
x_more              stx       top8                ; use same vars as d_ump
                    lda       top8                ; display upper 8 bits of address
                    jsr       outbyt
                    lda       low8                ; display lower 8 bits of address
                    jsr       outbyt
                    lda       #$20
                    jsr       outchar             ; display space
                    lda       0,x                 ; get current memory value
                    jsr       outbyt
                    lda       #$20
                    jsr       outchar
                    jsr       getbyt              ; wait for user input
                    bcs       x_abort             ; quit if nothing entered
                    sta       0,x                 ; save byte in location
                    inx                           ; increment memory pointer
                    lda       #$0D                ; display new line
                    jsr       outchar             ; cannot use CRLF() b/c it fucks X
                    lda       #$0A
                    jsr       outchar
                    bra       x_more              ; do it all again

x_abort             jmp       main

; J_UMP: begin executing program code at a user-specified address
; The last link in creating a complete ROM monitor
; jump address is supplied as command-line argument
; IMPORTANT: do not use jsr to call this function!!
j_ump               ldx       #buffer             ; point to keyboard buffer
                    jsr       buffer2hex          ; convert args to hexadecimal
                    ldy       arg_ptr             ; get location of first argument
                    beq       abort_jump          ; no address supplied, abort
                    ldx       0,y                 ; load jump address into X
                    jmp       0,x                 ; jump to address in X

abort_jump          jmp       main                ; return on error

; SET_TIME: alter value of Time data structure to something other
; than the power-on test values
; option to quit without entering seconds value by pressing enter
; data-format conversion routine still missing (BCD2H)
; altered Jan 11 2001 to use revised GETBYT
set_time            clra                          ; lda  #$0
                    sta       TMSK2               ; turn off RTI
                    jsr       getbyt              ; get the hours digits (echo automatically)
                    bcs       x_time              ; if no data entered, quit
;       jsr     bcd2h          ; convert to hex from user-input BCD
                    sta       hours               ; store in data structure
                    lda       #':'                ; display the colon separator
                    jsr       outchar
                    jsr       getbyt              ; get minutes
;       jsr     bcd2h           ; convert to hex
                    sta       minutes
                    lda       #':'                ; display the colon separator
                    jsr       outchar
                    jsr       getchar             ; get single char (10's of seconds)
                    cmpa      #$0D                ; check if it's the return character
                    beq       x_time              ; if so, job is done
                    jsr       outchar             ; echo character
                    jsr       a2h                 ; convert to hex
                    lsla
                    lsla
                    lsla
                    lsla
                    tab                           ; copy ACCA to ACCB
                    jsr       getchar             ; get 1's of seconds
                    jsr       outchar             ; echo
                    jsr       a2h                 ; convert to hex
                    aba                           ; combine digits in ACCA
                    sta       seconds             ; finally store it in data structure
x_time              lda       #$40
                    sta       TMSK2               ; turn RTI back on
                    jmp       main

; DISPLAY_TIME: prints the current time to the screen.
; no variables passed or returned. ACCA altered.
; Displays time in decimal format.
display_time
                    lda       hours               ; get hours
                    jsr       h2bcd               ; convert to BCD
                    jsr       outbyt              ; ascii-fy and output
                    lda       #':'                ; colon separator
                    jsr       outchar             ; display it
                    lda       minutes             ; get minutes
                    jsr       h2bcd               ; convert to BCD
                    jsr       outbyt
                    lda       #':'                ; colon separator
                    jsr       outchar             ; display it
                    lda       seconds             ; get seconds
                    jsr       h2bcd               ; convert to BCD
                    jsr       outbyt
                    jmp       main

; H_ELP: Command help for various "shell" commands
; Temporary function, may assign help function to each subprogram
; Very inefficient code, could be fixed up using a jump table
h_elp               ldx       #buffer             ; point to keyboard buffer
h_1                 lda       0,x
                    beq       h_usage             ; quit if NULL hit before space found
                    cmpa      #$20                ; search for space
                    beq       h_args              ; space found
                    inx
                    bra       h_1                 ; keep looking

h_args              inx                           ; point to entry after space
                    lda       0,x                 ; get command "word"
                    cmpa      #'d'                ; is it 'd'?
                    bne       h_2                 ; if not, skip around it
                    ldx       #d_help             ; load help string for 'd'
                    bra       x_h_elp             ; display string

h_2                 cmpa      #'x'                ; is it 'x'?
                    bne       h_3                 ; if not, skip around it
                    ldx       #x_help             ; load string
                    bra       x_h_elp             ; display it

h_3                 cmpa      #'s'                ; is it 's'?
                    bne       h_4                 ; if not, skip around it
                    ldx       #s_help             ; load string
                    bra       x_h_elp             ; display it

h_4                 cmpa      #'h'                ; is it 'h'?
                    bne       h_5                 ; if not, skip around it
                    ldx       #h_help             ; load string
                    bra       x_h_elp             ; display it

h_5                 cmpa      #'t'                ; : t?
                    bne       h_6                 ; if not, skip around it
                    ldx       #t_help             ; load string
                    bra       x_h_elp             ; display

h_6                 cmpa      #'j'                ; j?
                    bne       nvc                 ; if not, skip around it
                    ldx       #j_help
                    bra       x_h_elp

nvc                 ldx       #no_help            ; no help entry
                    bra       x_h_elp

h_usage             ldx       #usage              ; display usage help
x_h_elp             jsr       outstr
                    jsr       crlf
                    jmp       main

; *L_OAD: downline loader for S19 files generated by AS11
; hacking-around first version, Jan 17 2001
; completely re-written Nov. 5 2004
l_oad               jsr       crlf                ; display "begin loading"
                    ldx       #l_begin
                    jsr       outstr
                    clr       bytes               ; init program length counter

l_strt              jsr       getchar             ; get the S
                    cmpa      #'S'
                    beq       s_ok                ; if yes, start new S19 record
                    bra       l_strt              ; if no, keep trying

s_ok                jsr       getchar
                    cmpa      #'1'                ; S1 record
                    beq       one
                    cmpa      #'9'                ; S9 record: S9030000FC always
                    beq       nine
                    bra       load_x              ; if neither, quit

one                 ldx       #buffer             ;** Start processing an S1 record**
                    bsr       getchar             ; ACCA=first byte of length field
                    sta       0,x                 ; buffer[0]=ACCA
                    bsr       getchar             ; ACCA=second byte of length field
                    sta       $01,x               ; buffer[1]=ACCA
                    jsr       dchr2hex            ; ACCA=hex length field value
                    tab                           ; ACCB=hex length (loop ctr)
                    subb      #$03                ; correct length for addx & CRC fields
addx                ldx       #buffer             ; x -> buffer[0]
                    bsr       getchar             ; address byte 1
                    sta       0,x                 ; buffer[0]=ASCII address byte 1
                    bsr       getchar             ; address byte 2
                    sta       $01,x               ; buffer[1]=ASCII address byte 2
                    bsr       getchar             ; address byte 3
                    sta       $02,x               ; buffer[2]=ASCII address byte 3
                    bsr       getchar             ; address byte 4
                    sta       $03,x               ; buffer[3]=ASCII address byte 4
                    jsr       qchr2hex            ; x=program load address
                    pshx                          ; copy X into Y
                    puly                          ; y=program load address
                    ldx       #buffer             ; x -> buffer[0]
l_loop              bsr       getchar             ; ASCII program byte 1
                    sta       0,x                 ; buffer[0]=ASCII program byte 1
                    bsr       getchar             ; ASCII program byte 2
                    sta       $01,x               ; buffer[1]=ASCII program byte 2
                    jsr       dchr2hex            ; ACCA=object code byte
                    sta       0,y                 ; store code byte in RAM
                    inc       bytes               ; advance the program length counter
                    iny                           ; point to next code byte location
                    decb                          ; loop ctr --
                    beq       l_strt              ; loop ctr=0, get next S19 record
                    bra       l_loop              ; else finish this record

nine                bsr       getchar             ; get the 03
                    bsr       getchar             ; get the 00
                    bsr       getchar             ; get the 00
                    bsr       getchar             ; get the FC
                    bsr       crlf
                    ldx       #lmesg              ; display "Done."
                    bsr       outstr
                    bsr       crlf
                    lda       faa_ptr             ; display upper byte of faa
                    jsr       outbyt
                    lda       faa_ptr+1           ; display lower byte of faa
                    jsr       outbyt
                    bsr       crlf                ; new line
                    lda       bytes               ; ACCA = loaded program length
                    jsr       h2bcd               ; HUNDREDS, ACCA=tens:ones
                    tab                           ; ACCB = tens:ones
                    lda       hundreds            ; ACCA = 100's digit
                    bsr       outbyt
                    tba                           ; ACCA = 10's:1's digits
                    bsr       outbyt
                    ldx       #l_bytes            ; display " bytes loaded."
                    bsr       outstr
                    bsr       crlf
load_x              jmp       main


;** Subroutines
; For writing sub-programs and useful user functions

; CRLF: print a new line to the screen
; occurs so often it warrants its own subroutine
crlf                ldx       #newline
                    bsr       outstr
                    rts

; GETCHAR: a polling loop that gets a character from the serial port
; returns char in ACCA
getchar             lda       SCSR                ; get serial status register
                    anda      #$20                ; mask off all bits but RDRF flag
                    beq       getchar             ; keep checking if RDRF=0
                    lda       SCDR                ; load received character
                    rts

; OUTCHAR: sends one character to serial port
; receives character in ACCA
outchar             pshb                          ; save B
oc                  ldb       SCSR                ; read serial status register
                    bpl       oc                  ; loop until TDRE = 1
                    sta       SCDR                ; send char
                    pulb                          ; restore B
                    rts

; OUTSTR: sends a string of characters to serial port
; address of string is passed in X
outstr              lda       0,x                 ; get char in string
                    beq       x_out               ; exit if null terminator
                    bsr       outchar             ; output char
                    inx                           ; point to next char
                    bra       outstr

x_out               rts

; H2A: Converts an 8-bit hex number into two ASCII characters
; Hex number passed in ACCA, lower ASCII returned in ACCB
h2a                 tab                           ; copy hex number into ACCB
                    lsra                          ; shift ACCA right 4 times
                    lsra
                    lsra
                    lsra                          ; higher hex digit now in LSB's of ACCA
                    adda      #$30                ; ASCII-fy both registers in one shot
                    cmpa      #$3A                ; is it $3A?
                    blo       do_B                ; if lower, conversion correct
                    adda      #$7                 ; if not, adjust for A-F
do_B                andb      #$0F                ; mask off upper 4 bits
                    addb      #$30
                    cmpb      #$3A
                    blo       x_h2a               ; quit if conversion correct
                    addb      #$7                 ; if not, adjust for A-F
x_h2a               rts

; A2H: Converts an ASCII character to its 4-bit hex number
; ASCII passed in ACCA, hex returned in ACCA
a2h                 suba      #$30
                    cmpa      #$0A
                    blo       notchr
                    suba      #$07
notchr              rts

; GETBYT: Gets two ASCII chars from serial port and converts to 8-bit hex
; Gets its own chars from the serial port, returns hex in CONV & ACCA
; Carry flag set if nothing entered
getbyt              bsr       getchar             ; get char from serial port
                    cmpa      #$0D                ; is it enter?
                    beq       abort               ; user is aborting
                    bsr       outchar             ; echo it
                    bsr       a2h                 ; convert to 4-bit hex
                    lsla                          ; shift it left 4 times
                    lsla
                    lsla
                    lsla
                    sta       conv                ; save it
                    bsr       getchar             ; get second char
                    cmpa      #$0D                ; is it enter?
                    beq       abort
                    bsr       outchar             ; echo it
                    bsr       a2h                 ; convert to 4-bit hex
                    adda      conv                ; add first char to it
                    sta       conv                ; save full 8-bit hex
                    bra       x_getb

abort               sec                           ; return error code
x_getb              rts

; OUTBYT: Converts a hex number to ASCII chars and outputs it
; Hex is passed in ACCA. Returns nothing.
; fixed Jan 10 2001 to take advantage of OUTCHAR pushing B to stack
outbyt              bsr       h2a
                    bsr       outchar
                    tba
                    bsr       outchar
                    rts

; H2BCD: converts 8-bit hex to 12-bit BCD
; call this routine just before OUTBYT to display decimal instead of hex
; hex is passed in ACCA, hundreds digit is stored in HUNDREDS
; tens and ones are stored in packed format in DECIMAL
; ACCA also returns tens and ones
h2bcd               tab                           ; put into lower byte of D
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
                    lslb                          ; shift 10's into upper 4 bits
                    lslb
                    lslb
                    lslb
                    addb      decimal             ; combine 10's and 1's in ACCB
                    stb       decimal             ; store BCD
                    tba                           ; make decimal available in ACCA
                    rts

; EDIT: the input routine for the command interpreter
; One line of 15 chars max (to change, alter BUFFER length)
; Last entry in string is reserved for the NULL ($00)
; ENTER terminates routine, BACKSPACE allows editing
edit                ldx       #buffer             ; X points to command-line buffer
                    ldb       #BUFSIZE-1          ; set loop counter=15 chars max
get_c               jsr       getchar             ; get a character from keyboard
                    cmpa      #$0D                ; check for enter
                    beq       x_edit              ; quit if it is
                    cmpa      #$08                ; check for backspace character
                    beq       bksp                ; do the thing if it is
                    sta       0,x                 ; store character in buffer
                    jsr       outchar
                    inx                           ; point to next location in buffer
                    decb                          ; decrement loop counter
                    beq       x_edit              ; exit on loop counter=0
                    bra       get_c               ; otherwise repeat

bksp                cmpb      #BUFSIZE-1          ; is backspace the first char typed?
                    beq       get_c               ; ignore it
                    jsr       outchar             ; try and move cursor back one
                    lda       #$20                ; clear that letter
                    jsr       outchar
                    lda       #$08                ; try and move cursor back one
                    jsr       outchar
                    dex                           ; move buffer pointer back by one
                    bra       get_c               ; get next char

x_edit              clra                          ; make null char=$00
                    sta       0,x                 ; terminate string with NULL
                    rts

; BUFFER2HEX: Converts any command-line arguments in the keyboard
; buffer to hexadecimal
; from ASCII. Starts converting after the first space is found in
; the buffer - this allows future commands of more than a single letter
; leaves spaces ($20) as separators for the arguments. ARG_PTR points to
; the first converted item. To call: address of buffer must be in X
; Written in Florida December 2000
; revised Jan 11 2001 to add arg_ptr
buffer2hex
                    lda       0,x                 ; get char in buffer
                    beq       x_b2h               ; exit if end of buffer ($00) hit
                    cmpa      #$20                ; look for space character
                    beq       spc                 ; start converting if space found
                    inx                           ; point to next char
                    bra       buffer2hex

spc                 inx                           ; skip space character
                    stx       arg_ptr             ; save location of first argument
                    ldy       arg_ptr             ; Copy X into Y: X=src ptr, Y=dest ptr
hexify              lda       0,x                 ; get first char after space
                    beq       x_b2h               ; exit if null detected
                    cmpa      #$20                ; is it space character?
                    beq       skip_spc            ; if so, preserve it as a separator
                    jsr       a2h                 ; convert char to hex
                    sta       0,y                 ; save in buffer
                    inx                           ; point to next source char
                    lda       0,x                 ; get next source char
                    beq       x_b2h               ; quit if null detected
                    jsr       a2h                 ; convert this to hex also
                    ldb       0,y                 ; get previous converted char
                    lslb                          ; combine the two chars
                    lslb
                    lslb
                    lslb
                    aba                           ; full 8-bit hex number
skip_spc            sta       0,y                 ; store in buffer
                    inx                           ; next source entry
                    iny                           ; next dest entry
                    bra       hexify              ; continue converting

x_b2h               sta       0,x                 ; place NULL at end of converted buffer
                    rts

; FREE_RAM: displays the free RAM in the system. Takes no arguments
; Works only for unexpanded system
; Uses the first available address pointer FAA_PTR as bottom of RAM
; and $FF as top of RAM - change this if expanded
; Written Jan. 13 2001
free_ram            jsr       crlf                ; new line
                    lda       #$FF                ; put top of RAM in ACCA
                    suba      faa_ptr+1           ; subtract highest free RAM address
                    jsr       h2bcd               ; convert to decimal
                    jsr       outbyt              ; display free memory bytes
                    ldx       #mem_free
                    jsr       outstr
                    jmp       main

; CHR2HEX: convert one ASCII byte into 4-bit hex
; ASCII passed in A, value returned in A
; Input                         : ACCA=aaaa aaaa (ASC)
; Output                        : ACCA=0000 hhhh (HEX)
; Parameter passing             : ACCA (value to be converted)
; Registers used internally     : ACCA
chr2hex             suba      #$30
                    cmpa      #$09
                    ble       c2h_x
                    suba      #$07
c2h_x               rts


; DCHR2HEX: convert 2 ASCII bytes into 8-bit hex
; X points to a buffer containing two ASCII chars
; A holds return value
;**
; Input                         : INDX -> X:Y (2 ASCII chars)
; Output                        : ACCA = XY   (1 hex byte)
; Parameter passing             : INDX (pointer to buffer)
;                               : ACCA (return value)
; Registers used internally     : ACCA, ACCB, INDX
dchr2hex            pshb                          ; preserve ACCB
                    lda       0,x                 ; ACCA = aaaa aaaa (ASC)
                    bsr       chr2hex             ; ACCA = 0000 hhhh (HEX)
                    tab                           ; ACCB = 0000 hhhh (HEX)
                    lda       $01,x               ; ACCA = bbbb bbbb (ASC)
                    bsr       chr2hex             ; ACCA = 0000 iiii (HEX)
                    lslb                          ; ACCB = 000h hhh0
                    lslb                          ; ACCB = 00hh hh00
                    lslb                          ; ACCB = 0hhh h000
                    lslb                          ; ACCB = hhhh 0000
                    aba                           ; ACCA = hhhh iiii (HEX)
                    pulb                          ; restore ACCB
dc2h_x              rts

; QCHR2HEX: convert 4 ASCII bytes into 2 hex bytes
; X points to a buffer containing 4 ASCII chars
; X holds 2-byte hex return value
;**
; Input                         : INDX -> X:Y:Z:W (4 ASCII chars)
; Output                        : INDX =  XYZW    (1 hex byte)
; Parameter Passing             : INDX (pointer to buffer)
; Registers used internally     : ACCA, ACCB, INDX, INDY
qchr2hex            pshy                          ; save INDY
                    pshb
                    psha
                    pshx                          ; copy X into Y
                    puly                          ; Y = buffer base address
                    bsr       dchr2hex            ; ACCA = XY (HEX)
                    tab                           ; ACCB = XY (HEX)
                    pshy                          ; copy Y into X
                    pulx                          ; X = buffer base address
                    inx                           ; point to next two chars
                    inx
                    bsr       dchr2hex            ; ACCA = ZW (HEX)
                    psha                          ; swap ACCA & ACCB
                    pshb                          ; since ACCD = ACCA:ACCB
                    pula                          ; ACCA = XY
                    pulb                          ; ACCB = ZW
                    xgdx                          ; INDX=ACCD=A:B=XYZW (HEX)
                    pula                          ; put everything back
                    pulb
                    puly
qc2h_x              rts






;** Interrupt Service Routines

; RTI interrupt: real time clock
; updates the Time data structure on each interrupt
; Currently set up for 16.384ms RTI rate
; Accuracy: 1s = 0.999424s, or: slow by 49.7664s per 24h period
rti_isr             sei                           ; disable interrupts
                    psha                          ; save ACCA
                    lda       #$40
                    sta       TFLG2               ; clear interrupt flag
;** update ticks
                    lda       ticks               ; get ticks
                    inca                          ; increment
                    sta       ticks               ; store
                    cmpa      #$3D                ; compare with 61 (16.384ms * 61 = 1s)
                    blo       x_isr               ; if lower, exit
;** ticks rollover and seconds update
                    clra
                    sta       ticks               ; if not, set ticks=0
                    lda       seconds             ; get seconds
                    inca                          ; increment
                    sta       seconds             ; store
                    cmpa      #$3C                ; compare with 60 seconds
                    blo       x_isr               ; if lower, exit
;** seconds rollover and minutes update
                    clra
                    sta       seconds             ; set seconds=0
                    lda       minutes             ; get minutes
                    inca                          ; increment
                    sta       minutes             ; store
                    cmpa      #$3C                ; compare with 60 minutes
                    blo       x_isr               ; if lower, exit
;** minutes rollover and seconds update
                    clra
                    sta       minutes             ; minutes=0
                    lda       hours               ; get hours
                    inca                          ; increment
                    sta       hours               ; store
                    cmpa      #$18                ; compare with 24 hours
                    blo       x_isr               ; if lower, exit
;** hours rollover
                    clra
                    sta       hours               ; hours=0
x_isr               pula                          ; restore ACCA
                    cli                           ; re-enable interrupts
                    rti                           ; return from interrupt

;** Constants & Strings
; Kernel strings
done                fcc       "Done."
                    fcb       $0D,$0A,$00
wrm_strt            fcc       "MONITOR 01"
                    fcb       $00
mem_free            fcc       " bytes free."
                    fcb       $00
y32                 fcc       "mx1 detected."
                    fcb       $0D,$0A,$00
error               fcc       "Error."
                    fcb       $0D,$0A,$00
help                fcc       "Commands: [d x h s t m j l]"
newline             fcb       $0D,$0A,$00
prompt              fcc       "[monitor]> "
                    fcb       $00
reset               fcc       "Reset."
                    fcb       $0D,$0A,$00
lmesg               fcc       "Done."
                    fcb       $00
l_begin             fcc       "Begin Loading."
                    fcb       $00
l_bytes             fcc       " bytes loaded."
                    fcb       $00

;** On-line Documentation
; Explains CI commands.

no_help             fcc       "No help entry."
                    fcb       $00
d_help              fcc       "d [xxxx] [yy] : Dump. [x]=hex start addx, [y]=# of 16-byte blocks."
                    fcb       $00
s_help              fcc       "s : Set clock in hex. 24-h format [HH:MM:SS]. :SS opt."
                    fcb       $00
x_help              fcc       "x [xxxx] : Xamine. Enter byte at addx [x]."
                    fcb       $00
h_help              fcc       "h [x] : Help on command [x]."
                    fcb       $00
t_help              fcc       "t : Display time in 24-h format. SEE s to set."
                    fcb       $00
m_help              fcc       "m : show free system RAM."
                    fcb       $00
j_help              fcc       "j [xxxx] : Jump. Load specified address into PC."
                    fcb       $00
usage               fcc       "usage: h [d x h s t m j l]."
                    fcb       $00

;** Temporary Jump Table to display subroutine hex addresses
; useful for writing programs using 'x' command
                    org       $FF00
j_crlf              fdb       crlf
j_getchar           fdb       getchar
j_outchar           fdb       outchar
j_h2a               fdb       h2a
j_a2h               fdb       a2h
j_getbyt            fdb       getbyt
j_outbyt            fdb       outbyt
j_h2bcd             fdb       h2bcd

;** Warm Start Jump instruction
; User programs include a JMP $FFD3 to return control to kernel
                    org       $FFD3
                    jmp       warm_start

;** Interrupt Vector Table
; only RTI interrupt enabled
; all others cause a restart
                    org       $FFD6

SCI_VECT            fdb       strt
SPI_VECT            fdb       strt
PAI_VECT            fdb       strt
PAO_VECT            fdb       strt
TOF_VECT            fdb       strt

TOC5_VECT           fdb       strt
TOC4_VECT           fdb       strt
TOC3_VECT           fdb       strt
TOC2_VECT           fdb       strt
TOC1_VECT           fdb       strt
TIC3_VECT           fdb       strt
TIC2_VECT           fdb       strt
TIC1_VECT           fdb       strt

RTI_VECT            fdb       rti_isr
IRQ_VECT            fdb       strt
XIRQ_VECT           fdb       strt
SWI_VECT            fdb       strt
TRAP_VECT           fdb       strt
COP_FAIL_VECT       fdb       strt
COP_CMF_VECT        fdb       strt
RESET_VECT          fdb       strt

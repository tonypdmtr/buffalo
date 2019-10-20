;*******************************************************************************
;  e9bug      EEPROM-based S19 loader for the 68hc11e9 MCU
;
;  This program accepts S1-S9 records from the SCI and stores them at
;  the proper addresses in memory.  It also accepts a minimum number of
;  commands from the serial port, to allow the user to execute the
;  downloaded commands from a comm program.
;
;  This code is designed to reside in the EEPROM from $b600 to $b7ff.
;  Unlike 811bug, it does not take over the vector area, and it does
;  not use any on-chip RAM for RAM vectors.
;*******************************************************************************

;
;  Declare a few I/O register addresses
;

REGS                equ       $1000               ; i/o regs start here
PORTE               equ       $0a
BAUD                equ       $2b
SCCR2               equ       $2d
SCDR                equ       $2f
SCSR                equ       $2e
HPRIO               equ       $3c

CR                  equ       13
LF                  equ       10

;*******************************************************************************
;  Define the stack address and a few vital RAM locations.
;
;  This program configures the low-area RAM ($00 - $1ff) as follows:
;
;  $1ff    +--------+       <- Top of low-area RAM
;          |        |       e9bug variables reside here.
;          |        |
;  stkbeg  +--------+       <- Top of e9bug stack area
;          |        |       The stack grows downward from here.
;          |        |       Reserve at least $10 bytes for stack.
;          |        |
;          |        |
;          |        |
;  $00     +--------+       <- Bottom of low-area RAM
;*******************************************************************************

stkbeg              equ       $1ef                ; top of stack

;*******************************************************************************
                    #RAM
;*******************************************************************************
                    org       stkbeg+1            ; addr of variables

knt                 rmb       1                   ; byte variable
addr                rmb       2                   ; word variable
chk                 rmb       1                   ; byte variable
xqtaddr             rmb       2                   ; word variable
flag9               rmb       1                   ; byte variable

;*******************************************************************************
                    #ROM
;*******************************************************************************
                    org       $b600               ; start of code
;
;  Start of e9bug code.
;

;  Following powerup or reset, e9bug checks the state of port E, bit 7.
;  If that bit is high, control passes to the main loop in e9bug.  If
;  that bit is low, control automatically jumps to address $8000.
;
;  The jump to $8000 is blind; no check is made to ensure that code actually
;  exists there.  This leaves plenty of time for the code at $8000 to alter
;  the time-sensitive I/O registers, if needed.
;
;  Note that e9bug always turns on the MDA bit in the HPRIO register
;  before jumping to $8000.  This ensures that external memory is active
;  when the jump occurs.

Start               proc
                    ldx       #REGS               ; point to io regs
                    brclr     PORTE,x,#$80,Restart; branch if pe7 is low
                    bset      HPRIO,x,#$20        ; force expanded mode
xxqt                jmp       $8000               ; force start of user's prog

;*******************************************************************************
;  This is the main entry point to e9bug.  Each time control reaches
;  this point, the code resets the stack pointer and the SCI.
;  It also loads the e9bug variable xqtaddr with the address Restart.
;  This effectively makes e9bug the default program for use with the
;  'x' command.
;
;  This code then prints out a simple hello message.

Restart             proc
                    ldx       #REGS               ; point to i/o
                    lds       #stkbeg             ; always reset stack
                    bset      HPRIO,x,#$20        ; force expanded mode
                    lda       #$30                ; set baud = 9600
                    sta       BAUD,x
                    lda       #$0c                ; turn on SCI
                    sta       SCCR2,x

_1@@                lda       SCDR,x              ; dummy read of SCI data reg
                    clra                          ; get a null
                    jsr       OutCh               ; and send it
                    ldy       #4096               ; get a counter
_2@@                brset     SCSR,x,#$20,_1@@    ; branch if char arrived
                    dey                           ; no char, count this test
                    bne       _2@@                ; keep trying until timeout

                    ldd       #Restart            ; get restart addr
                    std       xqtaddr             ; use as default S9 addr
                    ldy       #HelloMsg           ; point to string
                    jsr       PrintStr            ; print it
;                   bra       MainLoop

;*******************************************************************************
;  This is the main loop for accepting input from the host and processing
;  commands.  Commands are determined by the first letter following a CR.
;  LFs are always ignored when e9bug is looking for a command.
;
;  Legal e9bug commands are:
;
;  S    marks the start of an S19 object record.  The entire line is
;       treated as an S19 record, and is parsed and loaded into memory.
;
;  x    transfers control to the last legal execution address, as passed
;       in the most recent S9 record, or as set by the j command below.
;
;  X    transfers control to $8000, regardless of any S19 object records
;       previously sent.
;
;  j    assigns a jump address for use with a subsequent 'x' command as above.
;       The j must be followed immediately by a four-digit hex address, followed
;       immediately by a CR.  The input cannot contain backspaces or other
;       cursor control characters, and it cannot contain embedded spaces or
;       tabs.

MainLoop            proc
                    lds       #stkbeg             ; always reset stack
                    ldx       #REGS               ; always point to i/o
                    jsr       CrLf                ; make it pretty
Loop@@              jsr       GetChar             ; get first char
                    cmpa      #LF                 ; LF?
                    beq       Loop@@              ; always ignore
                    cmpa      #'S'                ; load S19 record?
                    jeq       LoadS19             ; if so, do it
                    cmpa      #'x'                ; execute prog?
                    beq       xqt                 ; branch if so
                    cmpa      #'X'                ; execute at $8000?
                    beq       xxqt                ; branch if so
                    cmpa      #'j'                ; set jump addr?
                    beq       Jmp                 ; branch if so
                    cmpa      #'d'                ; display 16 bytes?
                    beq       Display             ; branch if so
                    cmpa      #'c'                ; change memory?
                    beq       Change              ; branch if so
                    cmpa      #CR                 ; empty line?
                    beq       MainLoop            ; if so, ignore it
;                   bra       EatErr

;*******************************************************************************
;  This code eats all incoming characters until a CR is detected.  It then
;  displays a '?' as an error indicator, then returns to the top of the
;  loop for the next command.

EatErr              proc
                    bsr       EatLine             ; error, waste the line
                    lda       #'?'                ; get error flag
                    jsr       OutCh               ; send it
                    bra       MainLoop            ; and leave

;*******************************************************************************
;  EatLine -- eat all characters, without echo, until a CR
;
;  This routine absorbs all incoming characters until a CR is reached.
;  This effectively ignores all text after an error has been detected.

EatLine             proc
Loop@@              jsr       GetCh               ; grab a char (no echo)
                    cmpa      #CR                 ; wait for CR
                    beq       Done@@              ; loop until hit it
                    jsr       OutCh               ; echo char
                    bra       Loop@@              ; do some more
Done@@              rts

;*******************************************************************************
;  xqt -- process the 'x' command
;
;  This routine jumps to the address stored in e9bug variable xqtaddr.

xqt                 proc
                    ldy       xqtaddr             ; get execution addr
                    jmp       ,y                  ; and do it

;*******************************************************************************
;  Jmp -- process the 'j' command
;
;  This routine collects a four-digit hex address and a terminating CR.
;  It then stores the address into the e9bug variable xqtaddr, for use
;  with a later 'x' command.

Jmp                 proc
                    jsr       getaddr             ; get the addr
                    jsr       GetCh               ; get last char
                    cmpa      #CR                 ; better be cr
                    bne       EatErr              ; oops, that's bad
                    ldd       addr                ; OK, get real addr
                    std       xqtaddr             ; and save it
                    bra       MainLoop            ; do another one

;*******************************************************************************

Display             proc
                    jsr       getaddr             ; get the addr
                    jsr       GetCh               ; get last char
                    cmpa      #CR                 ; better be cr
                    bne       EatErr              ; oops, that's bad
                    jsr       CrLf                ; new line
                    lda       addr                ; get msb
                    jsr       OutByte             ; show it
                    lda       addr+1              ; get lsb
                    jsr       OutByte             ; show it
                    ldy       addr                ; set up pointer
                    ldb       #16                 ; zero a counter
Loop@@              jsr       Space               ; pretty
                    lda       ,y                  ; get byte
                    jsr       OutByte             ; show it
                    iny                           ; next byte
                    decb                          ; count this byte
                    bne       Loop@@              ; loop until done
                    jmp       MainLoop            ; all done

;*******************************************************************************

Change              proc
                    jsr       getaddr             ; get the addr
                    jsr       byte                ; get data
                    sta       flag9               ; temp storage
                    jsr       GetCh               ; get last char
                    cmpa      #CR                 ; better be cr
                    bne       EatErr              ; oops, that's bad
                    ldy       addr                ; point to addr
                    lda       flag9               ; get data
                    sta       ,y                  ; write to memory
                    jmp       MainLoop            ; all done

;*******************************************************************************
;  LoadS19 -- process an S19 record
;
;  This routine reads the rest of an S-record, processing the characters
;  as needed.
;
;  This routine supports the following S-records:
;
;  S0     header record; ignored by e9bug.
;
;  S1     16-bit data record; e9bug stores data into assigned addresses.
;
;  S9     execution record; e9bug records the execution address in e9bug
;         variable xqtaddr for later use with the 'x' command.
;
;  This routine automatically calculates and tests the checksum for each
;  record.  Illegal records and any record with a bad checksum are flagged
;  to the user with an error indication.
;
;  NOTE:  e9bug does not currently verify that data written to a memory
;  address actually got there.

LoadS19             proc
                    clr       flag9               ; assume S1 record
                    jsr       GetChar             ; grab next char
                    cmpa      #'0'                ; S0 record?
                    bne       _1@@                ; branch if not
;_0@@
                    bsr       EatLine             ; waste whole line
                    jmp       MainLoop            ; get next line

_1@@                cmpa      #'9'                ; S9 record?
                    bne       _2@@                ; no, try next one
                    inc       flag9               ; show S9 record
                    bra       _4@@                ; continue processing

_2@@                cmpa      #'1'                ; S1 record?
                    jne       EatErr              ; if not, error

_4@@                bsr       byte                ; get byte count
                    sta       knt                 ; save for now
                    dec       knt                 ; count this byte
                    sta       chk                 ; start checksum
                    bsr       byte                ; get MSB of addr
                    sta       addr                ; save addr
                    adda      chk                 ; add to checksum
                    sta       chk                 ; save it back
                    dec       knt                 ; count this byte
                    bsr       byte                ; get LSB of addr
                    sta       addr+1              ; save addr
                    adda      chk                 ; add to checksum
                    sta       chk                 ; save it back
                    ldy       addr                ; get addr
                    dec       knt                 ; count this byte
_3@@                beq       _5@@                ; if 0, all done
                    bsr       byte                ; get data byte
                    sta       ,y                  ; write to addr
                    iny                           ; point to next addr
                    adda      chk                 ; add to checksum
                    sta       chk                 ; save it back
                    dec       knt                 ; count this byte
                    bra       _3@@                ; go test knt

_5@@                bsr       byte                ; get checksum
                    coma                          ; get one's comp
                    cmpa      chk                 ; does it match?
                    bne       Fail@@              ; branch if fail
                    jsr       EatLine             ; eat the rest
                    tst       flag9               ; was this S9 record?
                    beq       Done@@              ; branch if S1
                    ldd       addr                ; get start address
                    std       xqtaddr             ; save for execution
Done@@              jmp       MainLoop            ; do next command
Fail@@              jmp       EatErr              ; show bad record

;*******************************************************************************
;  HexBin -- convert an ASCII character in AR to its binary value
;
;  This routine expects an ASCII character in the A register.  It converts
;  the character to uppercase, then converts it to a binary value from 0
;  to $f.
;
;  If this routine detects an illegal (non-hex) character, it automatically
;  jumps to EatErr to handle the error condition.

HexBin              proc
                    bsr       Upcase              ; make it uppercase
                    cmpa      #'0'                ; test low range
                    blt       Fail@@              ; branch if fail
                    cmpa      #'9'                ; test decimal
                    ble       Number@@            ; found hex number
                    cmpa      #'A'                ; test low hex
                    blt       Fail@@              ; branch if fail
                    cmpa      #'F'                ; test high hex
                    bgt       Fail@@              ; branch if fail
                    adda      #9                  ; set up for conversion
Number@@            anda      #$0f                ; get binary value
                    rts
Fail@@              jmp       EatErr              ; fail, show error

;*******************************************************************************
;  Upcase -- convert an ASCII character in AR to uppercase

Upcase              proc
                    cmpa      #'a'                ; check for low end
                    blt       Done@@              ; leave if too low
                    cmpa      #'z'                ; check for high end
                    bgt       Done@@              ; leave if too high
                    suba      #'a'-'A'            ; make it uppercase
Done@@              rts

;*******************************************************************************
;  byte -- collect two ASCII hex characters and convert them to a byte

byte                proc
                    pshb                          ; save b
                    bsr       GetChar             ; get a char
                    bsr       HexBin              ; convert it
                    asla:4                        ; move to MSB
                    tab                           ; now save in b
                    bsr       GetChar             ; get 2nd char
                    bsr       HexBin              ; convert it
                    aba                           ; make a byte of them
                    pulb                          ; restore b
                    rts

;*******************************************************************************
;  getaddr -- get two bytes, save in addr and addr+1

getaddr             proc
                    bsr       byte                ; get msb
                    sta       addr                ; and save it
                    bsr       byte                ; get lsb
                    sta       addr+1              ; save it
                    rts

;*******************************************************************************
;  GetCh -- get a character from the SCI; polling loop.

GetCh               proc
                    brclr     SCSR,x,#$20,*       ; wait until char arrives
                    lda       SCDR,x              ; get the data
                    rts

;*******************************************************************************
;  GetChar -- get a character, with echo, from the SCI; polling loop.

GetChar             proc
                    bsr       GetCh               ; get a char
                    bra       OutCh               ; and echo it

;*******************************************************************************
;  CrLf -- send a CR/LF sequence to the SCI

CrLf                proc
                    lda       #CR                 ; get cr
                    bsr       OutCh               ; send it
                    lda       #LF                 ; get lf
                    bra       OutCh               ; send it, then exit

;*******************************************************************************
;  space -- send a space to the SCI

Space               proc
                    lda       #' '                ; get a space and fall thru
;                   bra       OutCh

;*******************************************************************************
;  OutCh -- send a character to the SCI; polling loop.

OutCh               proc
Loop@@              tst       SCSR,x              ; get status
                    bpl       Loop@@              ; loop until ready
                    sta       SCDR,x              ; send it
                    rts

;*******************************************************************************
;  OutByte -- send a byte as two hex characters to SCI

OutByte             proc
                    psha                          ; save byte
                    lsra:4                        ; move msn to lsn
                    bsr       OutNyb              ; send msn
                    pula                          ; restore byte
;                   bra       OutNyb              ; and fall thru

;*******************************************************************************
;  OutNyb -- send low nibble in A to SCI

OutNyb              proc
                    anda      #$0f                ; leave only low nibble
                    adda      #'0'                ; make it ASCII
                    cmpa      #'9'                ; is it decimal?
                    ble       Done@@              ; branch if so
                    adda      #39                 ; make it lowercase ASCII
Done@@              bra       OutCh               ; send char and return

;*******************************************************************************
;  PrintStr -- print a string to the SCI
;
;  This routine prints a text string to the SCI.  The address of the
;  string is passed in the Y register, which is destroyed.  The string
;  must be null-terminated.

PrintStr            proc
Loop@@              lda       ,y                  ; get next char in string
                    beq       Done@@              ; leave if done
                    bsr       OutCh               ; send it
                    iny                           ; bump pointer
                    bra       Loop@@              ; do another
Done@@              equ       :AnRTS

;*******************************************************************************

HelloMsg            fcs       LF,CR,'e9bug  v1.0'

                    end       Start

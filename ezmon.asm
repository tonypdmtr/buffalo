;
; EZ-MON for the 6811
; by Randy Sargent (rsargent@media.mit.edu)
;
;*********************************************************************
; Control Register Definitions
;

BASE                equ       $1000

PORTA               equ       $1000               ; Port A data register
RESV1               equ       $1001               ; Reserved
PIOC                equ       $1002               ; Parallel I/O Control register
PORTC               equ       $1003               ; Port C latched data register
PORTB               equ       $1004               ; Port B data register
PORTCL              equ       $1005
DDRC                equ       $1007               ; Data Direction register for port C
PORTD               equ       $1008               ; Port D data register
DDRD                equ       $1009               ; Data Direction register for port D
PORTE               equ       $100A               ; Port E data register
CFORC               equ       $100B               ; Timer Compare Force Register
OC1M                equ       $100C               ; Output Compare 1 Mask register
OC1D                equ       $100D               ; Output Compare 1 Data register

; Two-Byte Registers (High,Low -- Use Load & Store Double to access)
TCNT                equ       $100E               ; Timer Count Register
TIC1                equ       $1010               ; Timer Input Capture register 1
TIC2                equ       $1012               ; Timer Input Capture register 2
TIC3                equ       $1014               ; Timer Input Capture register 3
TOC1                equ       $1016               ; Timer Output Compare register 1
TOC2                equ       $1018               ; Timer Output Compare register 2
TOC3                equ       $101A               ; Timer Output Compare register 3
TOC4                equ       $101C               ; Timer Output Compare register 4
TI4O5               equ       $101E               ; Timer Input compare 4 or Output compare 5 register

TCTL1               equ       $1020               ; Timer Control register 1
TCTL2               equ       $1021               ; Timer Control register 2
TMSK1               equ       $1022               ; main Timer interrupt Mask register 1
TFLG1               equ       $1023               ; main Timer interrupt Flag register 1
TMSK2               equ       $1024               ; misc Timer interrupt Mask register 2
TFLG2               equ       $1025               ; misc Timer interrupt Flag register 2
PACTL               equ       $1026               ; Pulse Accumulator Control register
PACNT               equ       $1027               ; Pulse Accumulator Count register
SPCR                equ       $1028               ; SPI Control Register
SPSR                equ       $1029               ; SPI Status Register
SPDR                equ       $102A               ; SPI Data Register
BAUD                equ       $102B               ; SCI Baud Rate Control Register
SCCR1               equ       $102C               ; SCI Control Register 1
SCCR2               equ       $102D               ; SCI Control Register 2
SCSR                equ       $102E               ; SCI Status Register
SCDR                equ       $102F               ; SCI Data Register
ADCTL               equ       $1030               ; A/D Control/status Register
ADR1                equ       $1031               ; A/D Result Register 1
ADR2                equ       $1032               ; A/D Result Register 2
ADR3                equ       $1033               ; A/D Result Register 3
ADR4                equ       $1034               ; A/D Result Register 4
BPROT               equ       $1035               ; Block Protect register
RESV2               equ       $1036               ; Reserved
RESV3               equ       $1037               ; Reserved
RESV4               equ       $1038               ; Reserved
OPTION              equ       $1039               ; system configuration Options
COPRST              equ       $103A               ; Arm/Reset COP timer circuitry
PPROG               equ       $103B               ; EEPROM Programming register
HPRIO               equ       $103C               ; Highest Priority Interrupt and misc.
INIT                equ       $103D               ; RAM and I/O Mapping Register
TEST1               equ       $103E               ; factory Test register
CONFIG              equ       $103F               ; Configuration Control Register

; Interrupt Vector locations

; This is for 6.270 board (in special mode)
; VECTOR_PAGE    EQU $BF00

; This is for 6811E2 (in normal single-chip mode)
VECTOR_PAGE         equ       $FF00

SCIVEC              equ       $D6+VECTOR_PAGE     ; SCI serial system
SPIVEC              equ       $D8+VECTOR_PAGE     ; SPI serial system
PAIVEC              equ       $DA+VECTOR_PAGE     ; Pulse Accumulator Input Edge
PAOVVEC             equ       $DC+VECTOR_PAGE     ; Pulse Accumulator Overflow
TOVEC               equ       $DE+VECTOR_PAGE     ; Timer Overflow
TOC5VEC             equ       $E0+VECTOR_PAGE     ; Timer Output Compare 5
TOC4VEC             equ       $E2+VECTOR_PAGE     ; Timer Output Compare 4
TOC3VEC             equ       $E4+VECTOR_PAGE     ; Timer Output Compare 3
TOC2VEC             equ       $E6+VECTOR_PAGE     ; Timer Output Compare 2
TOC1VEC             equ       $E8+VECTOR_PAGE     ; Timer Output Compare 1
TIC3VEC             equ       $EA+VECTOR_PAGE     ; Timer Input Capture 3
TIC2VEC             equ       $EC+VECTOR_PAGE     ; Timer Input Capture 2
TIC1VEC             equ       $EE+VECTOR_PAGE     ; Timer Input Capture 1
RTIVEC              equ       $F0+VECTOR_PAGE     ; Real Time Interrupt
IRQVEC              equ       $F2+VECTOR_PAGE     ; IRQ External Interrupt
XIRQVEC             equ       $F4+VECTOR_PAGE     ; XIRQ External Interrupt
SWIVEC              equ       $F6+VECTOR_PAGE     ; Software Interrupt
BADOPVEC            equ       $F8+VECTOR_PAGE     ; Illegal Opcode Trap Interrupt
NOCOPVEC            equ       $FA+VECTOR_PAGE     ; COP Failure (Reset)
CMEVEC              equ       $FC+VECTOR_PAGE     ; COP Clock Monitor Fail (Reset)
RESETVEC            equ       $FE+VECTOR_PAGE     ; RESET Interrupt

; Serial port

PORTD_WOM           equ       $20
BAUD1200            equ       $B3                 ; Assumes 8 MHz crystal
BAUD9600            equ       $B0                 ; Assumes 8 MHz crystal
TRENA               equ       $0C                 ; Transmit, Receive ENAble
RDRF                equ       $20                 ; Receive Data Register Full
TDRE                equ       $80                 ; Transmit Data Register Empty

; ASCII characters

SPACE               equ       32
CR                  equ       13
LF                  equ       10
DEL                 equ       127
BS                  equ       8
BELL                equ       7

;
;
;**********************************************************

;**********************************************************
;
; Start of program
;

                    org       0                   ; Start of internal RAM

input_buf           rmb       130                 ; Input buffer length
input_buf_end       rmb       1                   ; plus one for null termination
tempword1           rmb       2                   ; Temporary word of storage (2 byte value)

half_period         rmb       2
cycle_count_end
                    rmb       2                   ; Stores end of cycles
cycle_count_buf
                    rmb       70

                    org       $FF
stack_high          rmb       1                   ; High point of stack (grows downward)

                    org       $F800               ; Start of EEPROM

startup
                    lds       #stack_high         ; Set stack pointer to top of ram
                    jsr       init_serial
                    jsr       init_analogs

                    ldx       #boot_message       ; Write the boot message
                    jsr       puts

interaction_loop
                    ldx       #prompt
                    jsr       puts

                    jsr       gets
                    ldx       #input_buf

                    bsr       execute
                    bra       interaction_loop

execute
                    jsr       skip_spaces         ; Skip over spaces in input
                    ldab      0,X

                    beq       execute_done        ; Hit null termination, done
                    inx                           ; Skip to next char

                    subb      #'a'                ; Change a-z to 0-25
                    blo       error_handler       ; if underflow, error
                    cmpb      #25
                    bhi       error_handler       ; if larger than 25, error

                    aslb                          ; Multiply B by 2
                    ldy       #command_table
                    aby                           ; X has command_table + 2*(command#)

                    ldy       0,y                 ; get vector for command
                    jsr       0,y                 ; go there
                    bra       execute

execute_done
                    rts

;**************************************
;
; error_handler
;
; If there is an error reading input, go here.
; Gives help message, restarts interaction loop
;
; It is OK to jump to error_handler from
; anywhere -- even inside a subroutine.
; Error_hander sets the stack pointer
; back to the beginning before it restarts
; the interaction loop
;

error_handler
                    ldx       #error_message
                    jsr       puts                ; print error message
                    ldx       #help_message
                    jsr       puts                ; print help message
                    lds       #stack_high         ; reset stack (sort of like C longjmp)
                    bra       interaction_loop

;**************************************
;
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
; (see "help_message:")
;
; Be sure not to delete or add lines inadvertently.  The command
; dispatch code simply does an address offset from `command_table'.
; The `command_a' through `command_z' labels are ignored;  if you
; get them out of order
;

command_table

command_a           fdb       analog_command
command_b           fdb       error_handler
command_c           fdb       clear_command
command_d           fdb       error_handler
command_e           fdb       echo_command
command_f           fdb       error_handler
command_g           fdb       error_handler
command_h           fdb       error_handler
command_i           fdb       error_handler
command_j           fdb       error_handler
command_k           fdb       error_handler
command_l           fdb       loop_command
command_m           fdb       error_handler
command_n           fdb       error_handler
command_o           fdb       timer_output_command
command_p           fdb       error_handler
command_q           fdb       error_handler
command_r           fdb       read_command
command_s           fdb       set_command
command_t           fdb       error_handler
command_u           fdb       error_handler
command_v           fdb       error_handler
command_w           fdb       write_command
command_x           fdb       error_handler
command_y           fdb       error_handler
command_z           fdb       sleep_command

loop_command
                    jsr       read_y
                    pshx                          ; Save pointer to rest of line
loop_command_again
                    pulx                          ; Point back to rest of line
                    pshx
                    pshy                          ; Save Y

                    bsr       execute             ; execute rest of line

                    puly                          ; Restore Y
                    dey
                    bne       loop_command_again  ; Keep going?
                    puly                          ; Get rid of stored pointer
                    rts

sleep_command
                    jsr       read_y
                    pshx
outer_sleep_loop
                    ldx       #333                ; # of inner loops in 1 ms
inner_sleep_loop
                    dex                           ; 3 cycles
                    bne       inner_sleep_loop    ; 3 cycles
                    dey
                    bne       outer_sleep_loop
                    pulx
                    rts

echo_command
                    jsr       read_y
                    jsr       put_y
                    jsr       put_crlf
                    rts

read_command
                    jsr       read_y
                    ldab      0,y
                    jsr       put_b
                    jsr       put_crlf
                    rts

write_command
                    jsr       read_y
                    jsr       read_b
                    stab      0,y
                    rts

set_command
                    jsr       read_y
                    jsr       read_b
                    orab      0,y
                    stab      0,y                 ; mem = mem | val
                    rts

clear_command
                    jsr       read_y
                    jsr       read_b
                    comb                          ; 1's complement
                    andb      0,y
                    stab      0,y                 ; mem = mem & ~val
                    rts

analog_command
                    jsr       read_b
                    jsr       measure_analog
                    bsr       put_b
                    jsr       put_crlf
                    rts

timer_output_command


; First get the port number.
; For now we ignore this and just output everything to PORTA:6

                    jsr       read_b

; Next, get number of E clocks per half cycle

                    jsr       read_y
                    sty       half_period

; Next, store up all the cycle counts
                    ldy       #cycle_count_buf

get_cycle_count_loop

                    jsr       skip_spaces
                    ldab      0,X
                    cmpb      #'$'
                    beq       get_cycle_count
                    jsr       decode_decimal_digit
                    bcs       got_all_cycle_counts
get_cycle_count

                    xgdy                          ; D now has ptr
                    jsr       read_y
                    xgdy                          ; Y has ptr, D has data
                    std       0,Y
                    iny
                    iny
                    bra       get_cycle_count_loop

got_all_cycle_counts

                    sty       cycle_count_end

                    pshx                          ; Save X
                    ldx       #cycle_count_buf
                    bra       ol_20

;
; For now this code only deals with OC2,
; which controls bit 6 of PORTA
;

                    clr       TCTL1

output_loop
                    ldy       0,X

;       jsr     put_y
;       jsr     put_space

                    ldaa      #%01000000          ; Toggle on OC2
                    eora      TCTL1
                    staa      TCTL1               ; Swap from toggle pin to leave pin alone

                    pshx
                    ldx       #$1000
                    ldd       TCNT
                    addd      half_period         ; 5

;
; This is the tightest code I've thought of so far.
; It handles half_period of 27 and higher, which
; gives freqeuecies up to 37 KHz.  I believe this should
; be OK for most remote control devices
;

toggle_again
wait_toc2
                    cpd       $ff&TCNT,X          ; 6+
                    bpl       wait_toc2           ; 3+
                    addd      half_period         ; 5
                    std       $ff&TOC2,X          ; 5
                    dey                           ; 4
                    bne       toggle_again        ; 3

;
; The following implementation is a fair bit slower,
; which is why it is commented out.
;

; toggle_again:
;       bset    TFLG1,X #%01000000      ; 7 Clear compare
;       addd    half_period             ; 5
;       std     0xff&TOC2,X             ; 5
;
; wait_toc2:
;       brclr   TFLG1,X #%01000000 wait_toc2 ; 7+ Wait until TOC2 has compared
;
;       dey                             ; 4
;       bne     toggle_again            ; 3

                    bclr      [PORTA,X,#%01000000  ; Clear PORTA:6 (OC2)

                    pulx
                    inx
                    inx
ol_20
                    cpx       cycle_count_end
                    bne       output_loop

                    clr       TCTL1

                    pulx                          ; Restore X
                    rts


;**************************************
;
; put_b
;
; Puts 8-bit unsigned value in b out serial
; port (in decimal)
;

put_b
                    psha                          ; Save A
                    pshb                          ; Save B
                    pshy                          ; Save Y
                    clra
                    xgdy                          ; Y = [0:B]
                    bsr       put_y
                    puly                          ; Restore Y
                    pulb                          ; Restore B
                    pula                          ; Restore A
                    rts

;**************************************
;
; put_y
;
; Puts 16-bit unsigend value in Y out serial
; port (in decimal)
;

put_y

                    pshx                          ; Save X
                    pshy                          ; Save Y
                    psha                          ; Save A
                    pshb                          ; Save B
; Decode 5 digits, place on stack
                    xgdy
                    ldy       #5

py_decode_loop
                    ldx       #10
                    idiv                          ; X=X/10, D=X%10 (D=[A:B])
                    pshb                          ; Push 1-byte remainder
                    xgdx
                    dey
                    bne       py_decode_loop

; Print 5 digits, but truncate leading zeros
;   A =  0, truncating leading zeros
;   A != 0, have printed non-zero digit, so must print zeros

                    ldy       #5
                    clra

py_print_loop
                    pulb
                    tsta
                    bne       py_print_it         ; Seen non-zero digit, must print this one
                    tstb
                    beq       py_skip_it          ; This is zero, skip it
py_print_it
                    addb      #'0'                ; Add ascii `0' (48)
                    jsr       putchar
                    tba                           ; Give A non-zero value
py_skip_it
                    dey
                    bne       py_print_loop

; Done with digits.  If A is still zero, then
; we skipped all digits.  We had better print
; one `0' digit!

                    tsta
                    bne       py_rts
                    ldab      #'0'                ; Print 0
                    jsr       putchar

py_rts
                    pulb                          ; : Restore B
                    pula                          ; Restore A
                    puly                          ; Restore Y
                    pulx                          ; : Restore X
                    rts

;**************************************
;
; read_b
;
; Read 8 bit unsigned number from ascii string
; String pointed to by X
; Output in B
;
; X is modified to point to first char after the last
; digit of the number
;
; Default base is decimal;  if preceded
; by $, will be in hexadecimal
;
; Jumps to "error_handler" if error parsing
; number

read_b
                    pshy                          ; Save Y
                    psha                          ; Save B
                    bsr       read_y
                    xgdy                          ; Swap Y and [A:B] (a= msb, b= lsb)
                    tsta                          ; Are there high order bits?
                    bne       rb_error            ; If so, # was too high
                    pula                          ; Restore B
                    puly                          ; Restore Y
                    rts

rb_error
                    jmp       error_handler


;**************************************
;
; read_y
;
; Read 16 bit unsigend number from ascii string
; String pointed to by X
; Output in Y
; X is modified to point to first char after the last
; digit of the number
;
; Default base is decimal;  if preceded
; by $, will be in hexadecimal
;
; Jumps to "error_handler" if error parsing
; number

read_y
                    pshb                          ; Save B
                    jsr       skip_spaces
                    ldab      0,X
                    cmpb      #'$'
                    beq       read_y_hexadecimal
                    cmpb      #'p'
                    beq       read_y_symbol

read_y_decimal
                    ldy       #0
                    ldab      0,X
                    jsr       decode_decimal_digit
                    bcs       ry_error            ; If first char not digit, error

ryd_loop
                    ldab      0,X
                    bsr       decode_decimal_digit
                    bcs       ry_end              ; If char not digit, at end of number

                    pshb                          ; Save decoded digit

;
; Multiply Y by 10 by computing (2*Y)+(8*Y)
                    xgdy                          ; Exchange Y and D
                    lsld                          ; D=Y*2
                    std       tempword1           ; tempword1= Y*2
                    lsld                          ; D=Y*4
                    lsld                          ; D=Y*8
                    addd      tempword1           ; D=Y*2+Y*8
                    xgdy                          ; Exchange Y and D
; End of multiply Y by 10
;

                    pulb                          ; Restore decoded digit
                    aby                           ; Add B to Y

                    inx
                    bra       ryd_loop            ; Next digit

ry_end
                    pulb                          ; Restore B
                    rts

read_y_hexadecimal
                    ldy       #0
                    inx                           ; Skip $
                    ldab      0,X
                    bsr       decode_hexadecimal_digit
                    bcs       ry_error            ; If first char not digit, error

ryh_loop
                    ldab      0,X
                    bsr       decode_hexadecimal_digit
                    bcs       ry_end              ; If char not digit, at end of number

                    pshb                          ; Save decoded digit

;
; Multiply Y by 16
                    xgdy
                    lsld
                    lsld
                    lsld
                    lsld
                    xgdy
; End of multiply Y by 16
;

                    pulb                          ; Restore decoded digit
                    aby                           ; Add B to Y

                    inx
                    bra       ryh_loop            ; Next digit



ry_error
                    jmp       error_handler

read_y_symbol
                    inx                           ; Skip 'p'
                    ldab      0,X
                    inx
                    cmpb      #'a'
                    beq       read_y_porta
                    cmpb      #'b'
                    beq       read_y_portb
                    cmpb      #'c'
                    beq       read_y_portc
                    cmpb      #'d'
                    beq       read_y_portd
                    cmpb      #'e'
                    beq       read_y_porte
                    bra       ry_error

read_y_porta
                    ldy       #PORTA
                    bra       ry_end

read_y_portb
                    ldy       #PORTB
                    bra       ry_end

read_y_portc
                    ldy       #PORTC
                    bra       ry_end

read_y_portd
                    ldy       #PORTD
                    bra       ry_end

read_y_porte
                    ldy       #PORTE
                    bra       ry_end


;**************************************
;
; skip_spaces
;
; If X points to an ascii `space', increment
; X until it no longer points at one.

skip_spaces
                    pshb                          ; Save B
ss_loop
                    ldab      0,X
                    cmpb      #SPACE              ; (SPACE ascii for `space', 32)
                    bne       ss_done             ; If not a space, we're done
                    inx                           ; Skip to next char
                    bra       ss_loop

ss_done
                    pulb                          ; Restore B
                    rts

;**************************************
;
; decode_decimal_digit
;
; Decodes decimal digit in B.
;
; If B is an ascii digit `0'-`9',
; subtract ascii `0' and leave 0-9 in B
; Clear carry
;
; If B is not in range `0'-`9', set carry

decode_decimal_digit

                    subb      #'0'                ; Subtract ascii `0' (48)
                    blo       dd_error
                    cmpb      #9
                    bhi       dd_error
                    clc                           ; Successful, so clear carry
                    rts

dd_error
                    sec                           ; Digit not in range, so set carry
                    rts

;**************************************
;
; decode_hexadecimal_digit
;
; Decodes hexdecimal digit in B.
;
; If B is a hexadecimal digit `0'-`9'
; `a'-`f', or `A'-`F', convert it to
; 0-15 and put in B.  Clear carry.
;
; If B is not a valid hexadecimal digit,
; set carry

decode_hexadecimal_digit

                    cmpb      #'9'                ; If `9' or below, decode as decimal
                    bls       decode_decimal_digit
                    cmpb      #'a'                ; Lowercase letter?
                    blo       dhd_10
                    subb      #32                 ; Yes, convert lowercase to uppercase
dhd_10
                    subb      #'A'
                    blo       dd_error            ; Letter < `A'?  If so, it's invalid.
                    cmpb      #5
                    bhi       dd_error            ; Letter > `F'? If so, it's invalid.

                    addb      #10                 ; Add 10 so in range 10-15
                    clc                           ; Successful, so clear carry
                    rts

;**************************************
;
; getchar
;
; Get a character from the serial port.
; Returns character in B

getchar
                    ldab      SCSR
                    andb      #RDRF               ; Receive register full?
                    beq       getchar             ; If not, loop
                    ldab      SCDR                ; Get received character
                    rts

;**************************************
;
; putchar
;
; Puts character to serial port.
; Character passed in B

putchar
                    psha                          ; Save A
putchar_wait
                    ldaa      SCSR
                    anda      #TDRE               ; Transmit register empty?
                    beq       putchar_wait        ; If not, loop
                    stab      SCDR                ; Transmit B
                    pula                          ; Restore A
                    rts

;**************************************
;
; gets
;
; Gets line from serial port
; Places into "input_buf"
; Allows backspace (8 or 127)
; Waits for carriage return (13)
; Kills line on ctrl-x (24)

gets
                    pshb                          ; save B
                    pshx                          ; save X
                    pshy                          ; save Y
gets_cancel_line
                    ldx       #input_buf
gets_loop
                    bsr       getchar
                    andb      #$7f                ; strip off high bit
                    cmpb      #BS                 ; backspace key? (8)
                    beq       gets_backspace
                    cmpb      #DEL                ; delete key? (127)
                    beq       gets_backspace
                    cmpb      #CR                 ; carriage return key? (13)
                    beq       gets_cr
                    cmpb      #24                 ; cancel line key (ctrl-x)
                    beq       gets_cancel_line
                    cpx       #input_buf_end
                    beq       gets_error          ; input buffer full!
                    cmpb      #SPACE
                    blo       gets_error          ; no control characters

                    stab      0,x                 ; character was OK. add it
                    bsr       putchar             ; echo char
                    inx
                    bra       gets_loop

gets_backspace
                    cpx       #input_buf
                    bls       gets_error
                    ldab      #BS                 ; back up cursor
                    bsr       putchar
                    ldab      #SPACE              ; erase char by sending a space
                    bsr       putchar
                    ldab      #BS                 ; back up cursor
                    bsr       putchar
                    dex
                    bra       gets_loop

gets_error
                    ldab      #BELL               ; bell
                    bsr       putchar
                    bra       gets_loop

gets_cr
                    clrb
                    stab      0,X                 ; null terminate input

                    bsr       put_crlf
                    puly                          ; restore Y
                    pulx                          ; restore X
                    pulb                          ; restore B
                    rts

;**************************************
;
; puts
;
; Puts null-terminated string to serial port
; String pointed to by X

puts
                    pshx                          ; Save X
                    pshb                          ; Save B
puts_loop
                    ldab      0,X
                    beq       puts_done           ; If at null termination, exit
                    bsr       putchar
                    inx
                    bra       puts_loop

puts_done
                    pulb                          ; Restore B
                    pulx                          ; Restore X
                    rts

put_space
                    pshb
                    ldab      #SPACE
                    bsr       putchar
                    pulb
                    rts

put_crlf
                    pshb
                    ldab      #CR
                    bsr       putchar
                    ldab      #LF
                    bsr       putchar
                    pulb
                    rts

debug1
                    pshb
                    ldab      #'D'
                    jsr       putchar
                    ldab      #'1'
                    jsr       putchar
                    pulb
                    rts

debug2
                    pshb
                    ldab      #'D'
                    jsr       putchar
                    ldab      #'2'
                    jsr       putchar
                    pulb
                    rts

debug3
                    pshb
                    ldab      #'D'
                    jsr       putchar
                    ldab      #'3'
                    jsr       putchar
                    pulb
                    rts

debug4
                    pshb
                    ldab      #'D'
                    jsr       putchar
                    ldab      #'4'
                    jsr       putchar
                    pulb
                    rts


;**************************************
;
; init_serial
;
; Turns on serial port and initializes
; to reasonable values, speed 9600 baud
;
; Unfortunately, given an 8 MHz crystal, we
; can't get accurate divisors for
; 19.2K, 38.4K, 57.6K, 115.2K baud

init_serial
                    psha                          ; Save A

                    ldaa      SPCR                ; Turn off wired-or serial output
                    anda      #$ff^PORTD_WOM
                    staa      SPCR

                    ldaa      #BAUD9600           ; Set baud rate to 9600
                    staa      BAUD

                    ldaa      #TRENA              ; Enable transmit and receive
                    staa      SCCR2

                    pula                          ; Restore A
                    rts

;**************************************
;
; measure_analog
;
; Analog channel 0-7 input in B
; Output analog value 0-255 in B
;
; Be sure to turn on analog subsystem
; first with "init_analogs"

measure_analog
                    andb      #15                 ; Mask off top 4 bits
                    stab      ADCTL               ; Start conversion

; Note that the completion flag is set only after _4_ conversions
; take place.  You could be more efficient by waiting the exact #
; of clocks required for the first reading to be good.
;
; On the other hand, 4 conversions gives the internal capacitance of
; the A/D time to charge up in case you have an input that's has
; an impedance that's a bit too high.

ma_wait
                    ldab      ADCTL
                    andb      #$80                ; CCF = conversion complete flag
                    beq       ma_wait             ; Not yet complete

                    ldab      ADR1                ; Load result
                    rts

;**************************************
;
; init_analogs
;
; Turns on analog ports

init_analogs
                    ldaa      OPTION
                    oraa      $80                 ; ADPU = AD Power Up
                    staa      OPTION
                    rts

;**********************
;
; Strings
;

prompt
                    fcc       'ez-mon> '
                    fcb       0                   ; null termination

boot_message
                    fcb       CR,LF
                    fcc       'Welcome to EZ-MON 0.2     10/9/95    Randy Sargent'
                    fcb       CR,LF
                    fcc       '   http://www.ai.mit.edu/people/rsargent/ez-mon.html'
                    fcb       CR,LF
                    fcb       0                   ; null termination

error_message
                    fcc       'Error in input.'
                    fcb       CR,LF
                    fcb       0                   ; null termination

help_message
                    fcb       CR,LF
                    fcc       'EZ-MON help:'
                    fcb       CR,LF
                    fcc       'r <addr>        Read memory byte'
                    fcb       CR,LF
                    fcc       'w <addr> <data> Write memory byte'
                    fcb       CR,LF
                    fcc       's <addr> <data> Set bits (*addr = *addr | data)'
                    fcb       CR,LF
                    fcc       'c <addr> <data> Clear bits (*addr = *addr & ~data)'
                    fcb       CR,LF
                    fcc       'a <num>         Read analog channel'
                    fcb       CR,LF
                    fcc       'e <num>         Echo number'
                    fcb       CR,LF
                    fcc       'o <chan> <clocks/cycle> <cycles-on> (<cycs-off> <cycs-on> ...)'
                    fcb       CR,LF
                    fcc       '                "Beep" PORTA:6 (<chan> currently ignored)'
                    fcb       CR,LF
                    fcc       'l <num> ...     Loop over ... <num> times'
                    fcb       CR,LF
                    fcc       'z <num>         Sleep <num> milliseconds'
                    fcb       CR,LF
                    fcc       'Numbers can be entered in decimal (default), hexadecimal (start with $)'
                    fcb       CR,LF
                    fcc       'or symbolically (pa, pb, pc, pd, pe for ports a-e)'
                    fcb       CR,LF

                    fcc       'Examples:'
                    fcb       CR,LF
                    fcc       'r $1000         Reads byte from port A'
                    fcb       CR,LF
                    fcc       'r 4096          Reads byte from port A'
                    fcb       CR,LF
                    fcc       'r pa            Reads byte from port A'
                    fcb       CR,LF
                    fcb       CR,LF
                    fcb       0                   ; null termination

;**************************************
;
; Jump Vectors
;
; These are typically located in the
; $FF00 page (unless the chip is running
; in special mode, in which case they
; should be in the $BF00 page)

                    org       RESETVEC
                    fdb       startup             ; reset -> jump to startup

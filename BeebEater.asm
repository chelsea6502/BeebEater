; BeebEater v0.1 - BBC BASIC for the Ben Eater 6502.
; by Chelsea Wilkinson (chelsea6502)
; https://github.com/chelsea6502/BeebEater

; First, let's set some addresses...
BASIC = $8000 ; the entry point for the BBC BASIC rom.
START = $C000 ; the entry point for BeebEater

; Next, let's define the 'OS Calls'. These calls are how BBC BASIC interacts with hardware.
OSASCI = $FFE3
OSNEWL = $FFE7
OSWRCH = $FFEE
OSWORD = $FFF1
OSBYTE = $FFF4

; 6502-specific addresses
NMI = $FFFA

; Set ACIA labels
ACIA_DATA = $5000
ACIA_STATUS = $5001
ACIA_CMD = $5002
ACIA_CTRL = $5003
    
    .org BASIC  ; Set the start of the rom at $8000.
    incbin "Basic4r32.rom"  ; Import the binary file for BBC BASIC version 4r32. 
                                ; Sourced from: https://mdfs.net/Software/BBCBasic/6502/ 
    ; incbin "Basic2.rom" ; Running an old-school 6502 instead of the WDC 65C02? You'll have to use BBC BASIC II instead.

    .org START ; set the start of BeebEater at $C000.

; Let's hard-code in the boot messages. 
bootMessage: ; The first part of the first line.
    .byte $0D ; This is the "Carriage Return" (CR) ASCII character. 
    .text "BBC Computer " ; Default value as it was on the original BBC Micro. Feel free to change this to whatever you want
    .byte $00 ; All strings must end with an ASCII NUL character.

bootMessageRAM: ; The second part of the first line.
    .text "16K" ; 16k for 16 kilobytes of RAM available. If you have more RAM, you'll have to update this manually.
    .byte $07 ; $07 is the ASCII 'Bell' character. This plays a sound in most serial monitors.
    .byte $00 ; End with NUL

; Setup BeebEater. The reset addresses of $FFFC and $FFFD point to here.
reset:
    lda #$00 ; Soft reset the 6551 ACIA
    sta ACIA_STATUS

    ; Set the command register
    lda #$0B ; no parity, no echo, no interrupts.
    sta ACIA_CMD

    ; Set the control register
    lda #$10 ; 1 stop bit, 8 bits, 16x baud (Baud rate of 115200)
    sta ACIA_CTRL

    ; BBC BASIC stores the address of the 'write character' routine at $020E for quick access.
    ; Let's tell BBC BASIC where it is.
    lda #>OSWRCH ; Get the upper two bytes of OSWRCH (FF)
    sta $020F ; Store it in $020F
    lda #<OSWRCH ; Load the lower two bytes of OSWRCH (EE)
    sta $020E ; Store it in $020F
    ; fall through to 'boot'
boot: ; Setup and enter BBC BASIC
    LDA #>bootMessage ; Get the start address of where the first line of the boot message is.
    JSR printMessage ; Display the boot message.
    LDA #>bootMessageRAM ; Get the second line that describes how much RAM there is.
    JSR printMessage ; Display the RAM boot message.

    ; Print two line breaks to have a one line gap from the next line
    JSR OSNEWL
    JSR OSNEWL

    ; Get the 'ROM Title' at address $8009. By default this will display "BASIC".
    LDA #>($8009 - 1) ; Load the location of the ROM title into the accumulator 
    LDY #<($8009 - 1) ; [????]
    JSR printMessage ; Display the ROM title.

    ; Print two line breaks to have a one line gap from the command prompt (The '>')
    JSR OSNEWL
    JSR OSNEWL

    LDA #$01 ; Load '1' into the accumulator to tell BBC BASIC we want to enter the start of the ROM.
    JMP BASIC ; Enter BBC BASIC!


; Routine/Function to print a string.
; printMessage will start reading at the address stored in A, plus the raw value stored in Y.
; e.g. if A is "09" and Y is "4", printMessage will read the ASCII character stored at $8009 + 4 bytes (which is $800C).
printMessage:
    STA $FE ; Store the location of the address we want to print in $FE, which tells BBC BASIC where to start reading from.
    LDA #00 ; Load 0 into A
    STA $FD ; Store 0 into $FD, which tells BBC BASIC how much characters we have printed so far.
printMessageLoop:
    INY ; Read the next character. If we just entered the loop, this initialises Y to 0.
    LDA ($FD),Y ; Read the character at $FD, offset by the value of Y.
    JSR OSASCI ; Send the character to the ACIA to transmit out of the 'Tx' pin.
    CMP #$00 ; A '0' lets BBC Basic know when to stop reading. Let's check if that's the case.
    BNE printMessageLoop ;  If A is not 0, read the next character.
    RTS ; Return to where we were before 'printMessage' was called.

OSRDCH:
    lda ACIA_STATUS ; Get the status of the ACIA
    and #$08 ; Check if the 'Reciever Data Register Full' bit is 'Full'
    beq OSRDCH ; If not, keep waiting.
    lda ACIA_DATA ; Otherwise, get the character and store it into A.
    rts

OSWRCHV:
    sta ACIA_DATA ; send the character
WAIT_SETUP: ; OSWRCHV_WAIT is only needed for the Western Design Center (WDC) version of the 6551 ACIA, thanks to the famous UART bug.
; Assuming 1Mhz clock speed and 115200 baud rate, we need to loop WAIT_LOOP 18 times (#$12 in hex) before we can send the next character.
; At 115200 baud, wait times are <0.01 milliseconds!
    PHX ; 3 clock cycles
    LDX #$12 ; Number of WAIT_LOOPs. Calculated by: ((1 / (baud rate)) * ((Clock rate in Hz) * 10) - 18) / 4
WAIT_LOOP:
    DEX ; 2 clock cycles for every loop,
    BNE WAIT_LOOP ; 2 clock cycles for every loop, plus an extra 1 to leave the loop.
    PLX ; 4 cycles. Called only once.
OSWRCHV_RETURN: ; make sure this is still included if have commented WAIT_SETUP and WAIT_LOOP out
    RTS ; 6 cycles

OSBYTEV: 
    cmp #$84 ; Is it the 'read top of memory' system call?
    beq OSBYTE84 ; Put address '$4000' in YX registers.
    cmp #$83 ; Is it the 'read bottom of memory' system call?
    beq OSBYTE83 ; Put address '$0080' in YX registers.
    rts ; Otherwise, return with nothing. 
    ; There are much more OSBYTE system calls, but we don't need to implement these for now.

OSBYTE84: ; Routine to return the highest address of free RAM space.
    ldy #$40 ; Put address '$4000' in YX registers.
    ldx #$00
    rts

OSBYTE83: ; Routine to return the lowest address of free RAM space.
    ldy #$08 ; Put address '$0800' in YX registers. Anything below $0800 is memory space reserved by BBC Basic.
    ldx #$00
    rts

OSWORDV:
	sei	            ; disable interrupts 
	
    ; Load A, X, and Y registers by storing them into short-term memory.
	sta	$EF	
	stx	$F0				
	sty	$F1				

    cmp #$00        ; Is it the 'Read Line' system call?
    beq OSWORD0V    ; If yes, start reading input from the user.
    rts             ; Otherwise, return with no change.
OSWORD0V:
    ; TODO: explain what a 'control block' is
    LDY #4
osword0setup:
    ; store max/min ASCII codes, and max line length from zero page memory to main memory
    LDA ($F0),Y
    STA $02B3-2,Y
    DEY
    CPY #2 ; loop until it's 1
    BCS osword0setup

    ; store input buffer addresses into memory (temporary buffer)
    LDA ($F0),Y ; Get value (high byte) from zero-page. Y is 1 right now.
    STA $E9 ; Store into temporary buffer (high byte)

    LDY #$00
    STY $0269 ; store 0 in 'paged mode counter'?

    LDA ($F0),Y ; Get value (low byte) from zero-page
    STA $E8 ; Store into temporary buffer (low byte)

    CLI ; enable interrupts
    BCC readInputCharacter

readLineInputBufferFull:
    LDA #07 ; send a 'bell character'
retryWithoutIncrement:
    DEY ; Decrement Y. We are essentially 'cancelling out' the next instruction.
retryWithIncrement:
    INY ; Decrement Y. Y is currently holding the current position in the input.
outputAndReadAgain:
    JSR OSWRCH ; Print the character. Fall through to 'readInputCharacter'
readInputCharacter:
    JSR OSRDCH ; Get the next character from ACIA

    CMP #$08 ; Is it a backspace? Let's delete the last character.
    BEQ delete
    CMP #$7F ; Is it a delete? Let's delete the last character.
    BEQ delete

    BNE checkLowercase ; otherwise, move on
delete:
    CPY #0 ; are we at the first character?
    BEQ readInputCharacter ; then do nothing
    DEY ; otherwise, go back 1
    BCS outputAndReadAgain ; Write the delete character. Go bback to the start.

checkLowercase: 
    CMP #$61        ; Compare with 'a'
    BCC notLower    ; If less than 'a', it's not a lowercase letter
    CMP #$7A        ; Compare with 'z'
    BCS notLower    ; If greater than 'z', it's not a lowercase letter
    AND #$DF        ; Clear the 5th bit to convert to uppercase

notLower:
    STA ($E8),Y ; store character into the buffer
    CMP #$0D ; is it the newline character?
    BEQ newLineAndExit ; then finish

    CMP #$1B ; is it the escape key?
    BEQ Escape ; end early

    CPY $02B3 ; check current length against max word length
    BCS readLineInputBufferFull ; send a bell character if full

    CMP $02B4 ; check minimum ASCII character
    BCC retryWithoutIncrement ; less than minimum? retry

    CMP $02B5 ; check maximum ASCII character
    BEQ retryWithIncrement ; equal to maximum? accept and retry
    BCC retryWithIncrement ; less than maxmimum? accept and retry

    BCS retryWithoutIncrement

newLineAndExit:
    JSR OSNEWL
    LDA $FF ; set escape flag
    ROL ; "put bit 7 into carry"
Escape:
    RTS

interrupt:
    STA $FC ; save A
    PLA
    PHA ; Load the status flags into A so we can check them.
    AND #$10 ; Check if it's a BRK or an IRQ.
    BNE BRKV ; If it's BRK, that's an error. Go to the BRK vector.
IRQV: ; otherwise, it's an IRQ. Do nothing.
    RTS

BRKV:
    TXA             ; }
    PHA             ; } save X on stack

    TSX             ; get stack pointer
    LDA $0100 + 3,X ; get program counter low

    CLD                                                 
    SEC             ; set carry
    SBC #1          ; subtract 1
    STA $FD         ; and store
    
    LDA $0100 + 4,X ; get high byte
    SBC #0          ; subtract 1 if necessary
    STA $FE         ; and store
    STX $F0
    PLA             ;get back original value of X
    TAX                                                 
    JMP ($0202)     ; Call BBC BASIC's error handler. It's address is stored in $0202
    rts


    ; System Call vectors
    .org OSASCI
    CMP #$0D ; Is it the 'enter' key? Jump to OSNEWL, otherwise fall through to OSWRCH.
    BNE OSWRCH
   .org OSNEWL ; OSNEWL is essentially OSWRCH, but with a line break (CR+LF)
    LDA #$0A ; Send 'Carriage Return' character.
    JSR OSWRCH
    LDA #$0D ; Send a 'Line Feed' character. CR+LF make up a complete line break.
    .org OSWRCH
    JMP OSWRCHV ; At address 'OSWRCH', jump to the 'OSWRCH' routine (AKA a 'vector').

    .org OSWORD
    jmp OSWORDV

    .org OSBYTE
    jmp OSBYTEV

    .org NMI
    .word interrupt ; at NMI, go to interrupt handler
    .word reset ; at RESET address, go to reset label
    .word interrupt ; When IRQ goes low or BRK is called, go to the interrupt handler
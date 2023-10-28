; BeebEater v0.2 - BBC BASIC for the Ben Eater 6502.
; by Chelsea Wilkinson (chelsea6502)
; https://github.com/chelsea6502/BeebEater

; First, let's set some addresses...
BASIC = $8000 ; the entry point for the BBC BASIC rom.
START = $C000 ; the entry point for BeebEater

; BeebEater-specific memory addresses. $00-5F is reserved for BBC BASIC!
READBUFFER = $60 ; this stores the latest ASCII character that was sent into the ACIA

; Next, let's define the 'OS Calls'. These calls are how BBC BASIC interacts with hardware.
OSASCI = $FFE3 ; "OS ASCII" - Print an ASCII character stored in Register A (Accumulator)
OSNEWL = $FFE7 ; "OS New Line" - Print the 'CR' ASCII character, followed by the 'LF' character. These two characters make up a new line.
OSWRCH = $FFEE ; "OS Write Character" - Print a byte stored in the Accumulator. This doesn't necessarily have to be an ASCII one.
OSWORD = $FFF1 ; "OS Word" - This one is actually several system calls wrapped together. These system calls involve inputs more than one byte: a "word".
OSBYTE = $FFF4 ; "OS Byte" - A group of system calls that have inputs of only one byte. This one is much simpler than OSWORD.

; These are BBC BASIC-specific locations in memory. These locations are defined by BBC BASIC.
TICKS = $0292 ; A 5-byte memory location ($0292-$0296) that counts the number of 'centiseconds' since booting up. We use this this for the TIME command.

; 6502-specific addresses
NMI = $FFFA ; This is the entry point for when we trigger a 'Non-Maskable Interupt'. 
; RESET = $FFFC ; We don't need to define this because it comes directly after NMI.
; IRQ = $FFFD ; We don't need to define this because it comes directly after IRQ.

; Next, let's define where our ACIA is. $5000 is the default for Ben Eater.
ACIA_DATA = $5000
ACIA_STATUS = $5001 ; Status register
ACIA_CMD = $5002 ; Command register
ACIA_CTRL = $5003 ; Control register

; Next, let's define where the VIA is. $6000 is the default for Ben Eater.
; Right now we only use the VIA for timers. We'll use the ports for future versions.
;PORTB = $6000
;PORTA = $6001
;DDRB = $6002 ; "Data Direction Register B"
;DDRA = $6003 ; "Data Direction Register A"
T1CL = $6004 ; "Timer 1 Counter Low"
T1CH = $6005 ; "Timer 1 Counter High"
ACR = $600B ; "Auxiliary Control Register"
;IFR = $600D ; "Interrupt Flag Register"
IER = $600E ; "Interrupt Enable Register"
    
    .org BASIC  ; Set the start of the rom at $8000.
    incbin "Basic4r32.rom"  ; Import the binary file for BBC BASIC version 4r32. 
                                ; Sourced from: https://mdfs.net/Software/BBCBasic/6502/
                                ; Download the one from the "Acorn BBC Master" section.
    ; incbin "Basic2.rom" ; Running an old-school 6502 instead of the WDC 65C02? You'll have to use BBC BASIC II instead.
                                ; Sourced from: https://mdfs.net/Software/BBCBasic/6502/
                                ; Download the one from the "Acorn BBC Microcomputer" section.

    .org START ; set the start of BeebEater at $C000.

; Define the boot message. By default, you should see this at boot:
;;; BBC Computer 16k
;;;
;;; BASIC
;;;
;;; >
bootMessage: ; The first part of the first line.
    .byte $0D ; $OD is the "Carriage Return" (CR) ASCII character. 
    .text "BBC Computer " ; Default value as it was on the original BBC Micro. Feel free to change this to whatever you want.
    .byte $00 ; All strings must end with an ASCII NUL character.

bootMessageRAM: ; The second part of the first line.
    .text "16K" ; 16k for 16 kilobytes of RAM available. Feel free to change it if you change your RAM capacity.
    .byte $07 ; $07 is the "Bell" ASCII character. This plays a sound in most serial monitors.
    .byte $00 ; End with NUL

; Setup BeebEater. The reset addresses of $FFFC and $FFFD point to here.
; Let's set any hardware-specific things here.
reset:
    ; Clear registers
    LDA #0
    PHA ; Push A onto the stack
    PLP ; PLP = "Pull status from stack". This essentially resets the status flags to 0.
    LDX #0
    LDY #0
    JSR wipe_ram ; Let's clear all the RAM and start fresh

    ; --- ACIA 6551 Initialisation ---

    LDA #$00 ; Soft reset the 6551 ACIA by writing 0 to the status register.
    STA ACIA_STATUS

    ; Intialise ACIA the command register
    LDA #$09 ; No parity, no echo, with interrupts after every time we recieve a byte.
    STA ACIA_CMD

    ; Initialise the ACIA control register
    LDA #$10 ; 1 stop bit, 8 bits, 16x baud. ('16x' means 115200 on a 1.8432Mhz clock)
    STA ACIA_CTRL


    ; --- VIA 6522 Initialisation ---

    ; Initialise the 'Auxiliary Control Register (ACR)'.
    ; Set the VIA timer to trigger an interrupt every 10 milliseconds (1 centisecond)
    LDA #%01000000 ; Set the VIA to send continuous interrupts, spaced apart by every time Timer 1 runs out.
    STA ACR
    ; Store the hex equivalent of '10,000 - 2' into the timer. 
    ; We subtract 2 because it takes two clock cycles to send an interrupt and reset the timer.
    ; At 1mhz clock, the VIA ticks every 0.001 milliseconds. 0.0001 x 10000 = 10 milliseconds.
    LDA #$0E 
    STA T1CL
    LDA #$27
    STA T1CH

    ; Initialise the 'Interrupt Enable Register (IER)'.
    ; WARNING: Ben Eater wires the ACIA IRQ to the 6502 IRQ. This means that timers will stop counting if interrupts are disabled with 'SEI' at any point!
    ; If you don't want this to happen, sent the ACIA IRQ to 6502's NMI instead. That way 'SEI' can't pause the timers.
    LDA #%11000000 ; Trigger an IRQ interrupt every time Timer 1 runs out.
    STA IER
    ; fall through to 'boot' label
boot: ; Setup and enter BBC BASIC

    ; Reset the part in memory that stores the time elapsed since boot.
    LDA #$00
    STA TICKS
    STA TICKS + 1
    STA TICKS + 2
    STA TICKS + 3
    STA TICKS + 4

    ; BBC BASIC stores the address of the 'write character' routine at $020E for quick access.
    ; If we want to print characters, we also need to tell BBC BASIC where it is.
    LDA #>OSWRCH ; Get the upper two bytes of OSWRCH (FF)
    STA $020F ; Store it in $020F. This is where BBC BASIC looks for the OSWRCH instruction.
    LDA #<OSWRCH ; Load the lower two bytes of OSWRCH (EE)
    STA $020E ; Store it in $020F

    ; Send a "Form Feed" ASCII character. This clears the screen on CoolTerm.
    LDA #$0C 
    JSR OSWRCH

    ; Let's print the boot message!
    LDA #>bootMessage ; Get the start address of where the first line of the boot message is.
    JSR printMessage ; Display the boot message.
    LDA #>bootMessageRAM ; Get the second line that describes how much RAM there is.
    JSR printMessage ; Display the RAM boot message.

    ; Print two line breaks to have a one line gap between the previous line and the next line.
    JSR OSNEWL
    JSR OSNEWL

    ; Get the 'ROM Title' that BBC BASIC stores at address $8009. By default this will display "BASIC".
    ; TODO: Remember and explain why we need to subtract 1 from $8009
    LDA #>($8009 - 1) ; High byte of '$8009 - 1' contains the address of first character of the string. Let's store that in A.
    LDY #<($8009 - 1) ; Low byte of '$8009 - 1' contains the address containing length of the string. Let's store that in Y.
    JSR printMessage ; Display the ROM title.

    ; Print two line breaks to have a one line gap from the command prompt (The '>')
    JSR OSNEWL
    JSR OSNEWL

    CLI ; enable interrupts

    LDA #$01 ; Load '1' into the accumulator to tell BBC BASIC we want to enter the start of the ROM.
    JMP BASIC ; Enter BBC BASIC!


; Subroutine to print a string.
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

; OSRDCH: 'OS Read Character'
; This subroutine waits for a character to arrive from the ACIA, then stores it into the A register for another subroutine to read.
; We use this to send input from your keyboard to BBC BASIC.
; It also checks if the escape key has been pressed. If it has, it lets BBC BASIC know that it needs to leave whatever it's running.
OSRDCH:
    ; First, check for escape flag
    LDA #0
    BIT $FF ; if the escape flag set?
    BMI escapeCondition

    ; If there's no escape flag set, let's check the READBUFFER to see if it's full.
    ; We don't read the ACIA directly here. We use the IRQ interrupt handler to read the character and place it into READBUFFER.
    ; A full READBUFFER essentially means that there's a character that's been sent to the ACIA that hasn't been displayed yet.
    LDA READBUFFER ; Is there something in the buffer?
    CMP #0 ; Is the buffer empty?
    BEQ OSRDCH ; If it's still empty, keep waiting.
    PHA ; Otherwise, it's full. Let's save A to the stack so we can use it for later.
    LDA #0
    STA READBUFFER ; Clear the character buffer
    PLA ; Restore A from the stack
    RTS ; Return to the main routine
escapeCondition:
    ; If we're here, that means that the user just pressed the escape key. This should signal to BBC BASIC to stop whatever it's doing.
    LDA #0
    STA READBUFFER ; clear the character buffer
    SEC ; Set the carry bit, which BASIC reads as the escape condition is set.
    LDA #$1B ; Load the 'ESC' ASCII character into A.
    RTS

; OSWRCH: 'OS Write Character'
; System call that displays whatever character is in A. This doesn't necessarily have to be an ASCII character.
; The 'V' in "OSWRCHV" means "Vector". When BBC BASIC jumps to the OSWRCH address, it jumps straight to here.
OSWRCHV:
    STA ACIA_DATA ; Send the character to the ACIA to transmit through Tx.
WAIT_SETUP: ; This is only needed for the Western Design Center (WDC) version of the 6551 ACIA, thanks to the famous bug where the 'transmit register full' bit doesn't work properly.
; The bug means that we have to wait enough clock cycles to be sure that the byte has been transmitted fully before sending the next character.
; Assuming 1Mhz clock speed and 115200 baud rate, we need to loop WAIT_LOOP 18 times (#$12 in hex) before we can send the next character.
; Don't worry! At 115200 baud, the wait time is around 0.01 millisecond.
    PHX ; 3 clock cycles
    LDX #$12 ; Number of WAIT_LOOPs. Calculated by: ((1 / (baud rate)) * ((Clock rate in Hz) * 10) - 18) / 4
WAIT_LOOP:
    DEX ; 2 clock cycles for every loop,
    BNE WAIT_LOOP ; 2 clock cycles for every loop, plus an extra 1 to leave the loop.
    PLX ; 4 cycles. Called only once.
OSWRCHV_RETURN: ; make sure this is still included if have commented WAIT_SETUP and WAIT_LOOP out
    RTS ; 6 cycles

; OSBYTE: 'OS Byte'
; A group of system calls that only involve up to two bytes into the X and Y registers.
; Which system call to do is determined by whatever value is currently in the A register.
; There are much more OSBYTE system calls, but we only need three for the time being:
    ; OSBYTE $7E: "Acknowledge Escape" - Handles how BBC BASIC leaves what it's doing when the user presses the escape key.
    ; OSBYTE $84: "Read HIMEM" - This tells BBC BASIC the maximum memory address we can use for BASIC programs. $4000 by default
    ; OSBYTE $83: "Read OSHWM" - This tells BBC BASIC the minimum memory address we can use for BASIC programs (A.K.A the start of 'PAGE' memory). 
        ; $0800 by default, because we need to reserve $0000-$0800 for BBC BASIC.
OSBYTEV: 
    CMP #$7E ; is it the 'acknowledge escape' system call?
    BEQ OSBYTE7E
    CMP #$84 ; Is it the 'read top of memory' system call?
    BEQ OSBYTE84 ; Put address '$4000' in YX registers.
    CMP #$83 ; Is it the 'read bottom of memory' system call?
    BEQ OSBYTE83 ; Put address '$0080' in YX registers.
    RTS ; Otherwise, return with nothing. 

OSBYTE7E: ; Routine that 'acknowledges' the escape key has been pressed.
    LDA #0 ; Reset A
    LDX #0 ; Reset X
    BIT $FF   ; check for the ESCAPE flag. 'BIT' just checks bit 7 and 6.
    BPL osbyte124  ; if there's no ESCAPE flag then branch. Just clear the ESCAPE condition.
    LDA $0276   ; Get ESCAPE effects. TODO: Explain ESCAPE effects.
    BNE noEscapeEffects  ; No escape effects? 
    CLI    ; Allow interrupts
noEscapeEffects:
    LDX #$FF   ; X=$FF to indicate ESCAPE has been acknowledged
osbyte124:
    CLC    ; Clear the carry bit to let BBC BASIC know there's no more escape key to process.
osbyte125:
    ROR $FF ; set/clear bit 7 of ESCAPE flag
    RTS 

OSBYTE84: ; Routine to return the highest address of free RAM space.
    ; Put address '$4000' in YX registers.  
    LDY #$40 ; High byte goes into Y
    LDX #$00  ; Low byte goes into X
    RTS

OSBYTE83: ; Routine to return the lowest address of free RAM space.
    ; Put address '$0800' in YX registers. Anything below $0800 is memory space reserved by BBC Basic.
    LDY #$08 ; High byte goes into Y
    LDX #$00  ; Low byte goes into X
    RTS

; OSWORD: 'OS Word'
; A group of system calls that involves more than just a couple of bytes, but an area in RAM.
; BBC Basic uses 'Control Blocks' to define a sequence of bytes. They're a bit hard to explain, but all you need to know right now is that it's an area in RAM.
; There are much more OSWORD system calls, but we only need three for the time being:
    ; OSWORD 0: "Read line from current input" - This is how BBC BASIC lets you input a command, and how it processes your command.
    ; OSWORD 1: "Read system clock" - Get the number of 'centiseconds' since boot. This is called by the TIME command in BASIC.
    ; OSWORD 2: "Write system clock" - Set the number of 'centiseconds' since boot to a certain value. This is called by "TIME=[value]" in BASIC.
OSWORDV:
	SEI	 ; Disable interrupts for now, so we don't change a character to print while it's in the middle of printing one.
	
    ; Load A, X, and Y registers by storing them into short-term memory.
	STA	$EF	
	STX	$F0				
	STY	$F1				

    CMP #$00        ; Is it the 'Read Line' system call?
    BEQ OSWORD0V    ; If yes, start reading input from the user.
    CMP #$01        ; Is it the 'Read Clock' system call?
    BEQ OSWORD1V    ; Jump to it if yes
    CMP #$02        ; Is it the 'Write Clock' system call?
    BEQ OSWORD2V    ; Jump to it if yes
    RTS             ; Otherwise, return with no change.
OSWORD0V:
    STA READBUFFER ; Write a '0' to the character buffer, in case there's an escape character currently in there. Kind of a hacky solution, but it works!

    ; An OSWORD 0 control block has a couple of bytes of metadata to help us:
    ; 4th byte: Maximum ASCII code allowed
    ; 3rd byte: Minimum ASCII code allowed
    ; 2nd byte: Maximum line length allowed
    ; 1st byte: Address of the first character (I think?)
    LDY #4 ; Load 
osword0setup:
    ; Store max/min ASCII codes, and max line length from zero page memory to main memory
    LDA ($F0),Y
    STA $02B3-2,Y ; TODO: Remember and explain why we need to subtract 2.
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
    JSR OSRDCH ; Read the next character from ACIA

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
    BEQ Escape
continueRead:
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

; OSWORD 1: Read System Timer
; The variable TIME is a 5-byte variable starting at address 'TICKS'.
; To read the timer, let's loop through the 5 bytes and store them in $F0-$F4, which BBC BASIC reads from.
OSWORD1V:
    LDX #0                              
readTimer:
    LDY #4 ; Use this to read the 5 bytes. This will run down from 4 to 0.
readTimerLoop:
    LDA TICKS,X ; Load the TICKS byte, offset by X. X will be either 0, 1, 2, 3, or 4.
    STA ($F0),Y ; Store into $F0, offset by Y. Y will be either 4, 3, 2, 1, or 0.               
    INX                                                
    DEY                 
    BPL readTimerLoop ; Loop while Y is still greater than 0. BPL = "Branch on PLus"
    RTS

; OSWORD 2: Write System Timer
; To write the timer, let's essentially do the opposeite of 'Read System Timer'
; Let's loop through the 5 bytes in $F0-$F4, and store them in the 5-byte variable starting at address 'TICKS'.
OSWORD2V:
    LDX #0
    LDY #4
writeTimerLoop:
    LDA ($F0),Y ; Same principle as 'readTimerLoop'.
    STA TICKS, X
    INX
    DEY
    BPL writeTimerLoop ; ; Loop while Y is still greater than 0. BPL = "Branch on PLus"
    RTS

; Subroutine called after every NMI or IRQ in hardware, or the BRK instruction in software.
interrupt:
    STA $FC ; Save A for later.
    PLA ; get status register. it's on the stack at this point
    PHA ; put the status flags back on the stack
    AND #$10 ; Check if it's a BRK or an IRQ.
    BNE BRKV ; If it's BRK, that's an error. Go to the BRK vector.
IRQV: ; Otherwise, it's an IRQ. Let's check what caused the interrupt, starting with the ACIA.
    LDA ACIA_STATUS
    AND #$88 ; Check the ACIA status register to find out if the ACIA is asking to read a character.
    BEQ IRQ_VIA_TICK ; If there's nothing to read, then the VIA timer probably caused it.
IRQ_ACIA:
    LDA ACIA_DATA ; Read the ACIA. Because reading the ACIA clears the data, this is the only place allowed to read it directly!
    STA READBUFFER ; For everywhere else that needs to access the character, we will store in memory.
    CMP #$1B ; check if an escape key was pressed
    BNE end_irq ; If it's not an escape key, we've done everything we need. Skip to the end.
IRQ_ACIA_ESCAPE: ; If an escape key was pressed, let's set the escape flag.
    LDA #$FF
    STA $FF ; set the 'escape flag' address at $FF to the value #$FF.
    JMP end_irq ; Skip to the end.
IRQ_VIA_TICK: ; If we've ruled out the ACIA, then let's assume it was the VIA timer.
    LDA T1CL ; Clear the interrupt by reading the timer.
    INC TICKS + 4 ; Increment the 4th byte, which holds the lowest byte.
    BNE end_irq ; If the byte didn't overflow from FF to 00, then we've done all we need. Skip to the end.
    INC TICKS + 3 ; If it DID overflow, then let's carry the 1 to the next register.
    BNE end_irq ; If the byte didn't overflow from FF to 00, then we've done all we need. Skip to the end.
    INC TICKS + 2 ; If it DID overflow, then let's carry the 1 to the next register.
    BNE end_irq ; etc etc
    INC TICKS + 1
    BNE end_irq
    INC TICKS
end_irq:
    LDA $FC ; Restore what was in the A register before we were so rudely interrupted
    RTI ; "ReTurn from Interrupt"

; Handler for interrupts that we know were called by the BRK instruction. This likely means that there was an error called by BBC BASIC.
; BBC BASIC has a way of telling us the error message. To get the message, we need to store the location of the error message into addresses $FD and $FE. 
BRKV:
    TXA ; Save X to the stack, so we can restore it later
    PHA

    TSX             ; Get the value of the stack pointer and store it in X
    LDA $0100 + 3,X ; Get the low byte of the error message. This is stored in the stack.
    CLD             ; "Clear Decimal Mode". I don't know what this does, but the original BBC Micro does this so I left it in.                    
    SEC             ; Set the carry bit in the 6502 status register.
    SBC #1          ; Subtract 1. SBC = "SuBtract if Carry set". This will always subtract 1 because we just set it. TODO: Investigate why we need to subtract 1.
    STA $FD         ; Store it into $FD
    
    LDA $0100 + 4,X ; Get the high byte of the error message. This is stored in the stack.
    SBC #0          ; Subtract 1 if the carry bit is set. TODO: Investigate why we need to subtract 1.
    STA $FE         ; Store high byte into $FE
    STX $F0         ; Store the return address into $F0.

    PLA             ; Restore the original value of X
    TAX                 

    ; Call BBC BASIC's error handler, which takes it from there. It's stored in $0202.
    JMP ($0202)     
    RTS ; The error handler will return us to here. From there, let's return to where we were before the BRK.

wipe_ram:
        PHA ; Save A register (Accumulator)
        PHX ; Save X register
        PHY ; Save Y register
        LDA #$00
        LDX #$00       ; Start with the low byte of the address at $00
        LDY #$00       ; Start with the high byte of the address at $00
wipe_ram_loop:
        STA ($00,X)    ; Store the accumulator (which is already 0) to the effective address
        INX            ; Increment the low byte
        BNE no_incy    ; If the low byte is not 0 (no overflow), skip the Y increment
        INY            ; Increment the high byte
no_incy:
        CPY #$40       ; Check if we've reached past $3FFF
        BNE wipe_ram_loop ; If we haven't, continue the loop
        PLY ; Restore Y register
        PLX ; Restore X register
        PLA ; Restore A register
        RTS

    ; BBC BASIC system calls. BBC BASIC calls these by jumping to their place in memory.
    ; Most of them jump to a 'vector' that properly handles the system call.
    .org OSASCI
    CMP #$0D ; Is it the 'Enter' key? Jump to OSNEWL, otherwise fall through to OSWRCH.
    BNE OSWRCH
   .org OSNEWL ; OSNEWL is essentially OSWRCH, but with a line break (CR+LF)
    LDA #$0A ; Send 'Carriage Return' character.
    JSR OSWRCH
    LDA #$0D ; Send a 'Line Feed' character. CR+LF make up a complete line break.
    .org OSWRCH
    JMP OSWRCHV ; At address 'OSWRCH', jump to the 'OSWRCH' routine (AKA a 'vector').
    .org OSWORD
    JMP OSWORDV
    .org OSBYTE
    JMP OSBYTEV

    ; 6502-specific calls, such as interrupts and resets.
    .org NMI
    .word interrupt ; at NMI, go to interrupt handler
    .word reset ; at RESET address, go to reset label
    .word interrupt ; When IRQ goes low or BRK is called, go to the interrupt handler

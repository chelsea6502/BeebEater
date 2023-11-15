; BeebEater v0.4.2 - BBC BASIC port for the Ben Eater 6502.
; by Chelsea Wilkinson (chelsea6502)
; https://github.com/chelsea6502/BeebEater

; -- Constants --

; First, let's set some addresses...
BASIC = $8000 ; the entry point for language rom.
START = $C000 ; the entry point for BeebEater.

; We're going to use the same constant names from the original BBC Micro/Master OS (a.k.a 'BBC MOS'). 
; See https://mdfs.net/Docs/Comp/BBC/AllMem for details.
OSVDU  =$D0
OSKBD1 =$EC
OSKBD2 =OSKBD1+1
OSKBD3 =OSKBD1+2
OSAREG =$EF
OSXREG =$F0
OSYREG =$F1
OSINTA =$FC
OSFAULT=$FD
OSESC  =$FF
TIME   =$0292 ; A 5-byte memory location ($0292-$0296) that counts the number of 'centiseconds' since booting up. We use this for the TIME function.
OSVDUWS=$0300

; For some, we'll set some aliases so it's easier to understand their purpose.
READBUFFER      = OSKBD1  ; this stores the latest ASCII character that was sent into the ACIA
KEYBOARD_FLAGS  = OSKBD2  ; This byte helps us keep track of the state of a key presses on the keyboard. See below.
LCDREADBUFFER   = OSVDU+2 ; For storing the last printed ASCII character in the LCD.
LCDCURSORBUFFER = OSVDU+3 ; For storing the current LCD cursor position.
LCDBUFFER       = OSVDUWS ; For storing a line of LCD characters.

; Keyboard flag constants:
RELEASE = %00000001 ; Flag for if a key has just been released.
SHIFT   = %00000010 ; Flag for if we are holding down the shift key.

; ACIA definitions. $5000 is the default.
ACIA_DATA = $5000
ACIA_STATUS = $5001 ; Status register
ACIA_CMD = $5002 ; Command register
ACIA_CTRL = $5003 ; Control register

; VIA definitions. $6000 is the default.
PORTB = $6000 ; Location of register B on the VIA. Keep this available for the future LCD update.
PORTA = $6001 ; Location of register A on the VIA.
DDRB = $6002 ; "Data Direction Register B"
DDRA = $6003 ; "Data Direction Register A"
T1CL = $6004 ; "Timer 1 Counter Low"
T1CH = $6005 ; "Timer 1 Counter High"
ACR = $600B ; "Auxiliary Control Register"
PCR = $600C ; "Peripheral Control Register"
IFR = $600D ; "Interrupt Flag Register"
IER = $600E ; "Interrupt Enable Register"'

; LCD Constants
E  = %01000000
RW = %00100000
RS = %00010000

; BBC MOS "OS Calls". These addresses point to routines that access your hardware.
OSRDCH = $FFE0 ; "OS Read Character" - Transfers the characters read from the 6551 ACIA into Register A (Accumulator)
OSASCI = $FFE3 ; "OS ASCII" - Print an ASCII character stored in Register A (Accumulator)
OSNEWL = $FFE7 ; "OS New Line" - Print the 'CR' ASCII character, followed by the 'LF' character. These two characters make up a new line.
OSWRCH = $FFEE ; "OS Write Character" - Print a byte stored in the Accumulator. This doesn't necessarily have to be an ASCII one.
OSWORD = $FFF1 ; "OS Word" - A group of system calls that have parameters passed in a control block pointed to by the XY registers.
OSBYTE = $FFF4 ; "OS Byte" - A group of system calls that have byte parameters in the registers. This one is much simpler than OSWORD.

; Hardware 6502-specific addresses
NMI = $FFFA ; This is the entry point for when we trigger a 'Non-Maskable Interupt'. 
RST = $FFFC ; RESET
IRQ = $FFFE ; Maskable interupts

; -- Entry Points --

    .org BASIC  ; Set the start of the rom at $8000.
    incbin "Basic4r32.rom"  ; Import the binary file for BBC BASIC version 4r32. 
                                ; Sourced from: https://mdfs.net/Software/BBCBasic/6502/
                                ; Download the one from the "Acorn BBC Master" section.

    .org START ; set the start of BeebEater at $C000.

; -- ROM Constants. Unlike the constants before, these are actually stored in the EEPROM. --

; Define the boot message. By default, you should see this at boot:
;;; BeebEater Computer 16k
;;;
;;; BASIC
;;;
;;; >
bootMessage: ; The first line of the boot message.
    .byte $0C ; Start with a 'form feed' ASCII character. This clears the screen.
    .text "BeebEater Computer " ; Describes the computer system.
    .text "16K" ; 16k for 16 kilobytes of RAM available. Feel free to change it if you change your RAM capacity.
    .byte $0A ; Give a one-line gap.
    .byte $0D
    .text "BASIC"
    .byte $0A ; Give a one-line gap.
    .byte $0D
    .byte $07 ; Send a bell character
    .byte $00 ; End with NUL

; -- Start of Program --

; Set up BeebEater. The reset addresses of $FFFC and $FFFD point to here.
; Let's set any hardware-specific things here.
reset:
    ; -- ACIA 6551 Initialisation --

    ; Soft reset the 6551 ACIA by writing 0 to the status register.
    LDA #0
    STA ACIA_STATUS 

    ; Intialise ACIA the command register
    LDA #%00001001 ; No parity, no echo, with interrupts after every time we recieve a byte.
    STA ACIA_CMD

    ; Initialise the ACIA control register
    LDA #%00010000 ; 1 stop bit, 8 bits, 16x baud. ('16x' means 115200 on a 1.8432Mhz clock)
    STA ACIA_CTRL

    ; --- VIA 6522 Initialisation ---

    LDA #0 ; Set PORTA (for the keyboard) to input.
    STA DDRA
    LDA #%11111111 ; Set PORTB (for the LCD) to output.
    STA DDRB

    ; Initialise the 'Auxiliary Control Register (ACR)'.
    ; Set the VIA timer to trigger an interrupt every 0.1 milliseconds (1 centisecond)
    LDA #%01000000 ; Set the VIA to send continuous interrupts, spaced apart by every time Timer 1 runs out.
    STA ACR

    ; Store the hex equivalent of '10,000 - 2' into the timer. 
    ; We subtract 2 because it takes two clock cycles to send an interrupt and reset the timer.
    ; At 1mhz clock, the VIA ticks every 0.001 milliseconds. 0.001 x 10000 = 1 millisecond.
    LDA #$0E 
    STA T1CL
    LDA #$27
    STA T1CH

    ; Set two interrupt triggers on the VIA:
    ; 1. When the timer goes to 0.
    ; 2. When the 'CA1' pin has a rising edge (for the PS/2 Keyboard).
    LDA #$01
    STA PCR
    LDA #$C2
    STA IER

    ; Initialise the 'Interrupt Enable Register (IER)'.
    LDA #%11000000 ; Trigger an IRQ interrupt every time Timer 1 runs out.
    STA IER

    ; --- LCD Reset Sequence ---
    ; We will now go through the LCD reset sequence, as instructed in page 47 of the Hitachi 44780U LCD controller datasheet.

    ; Step 1: Wait 15ms after LCD gets power.
    JSR lcd_init_delay ; This routine waits about 16 milliseconds when at a 1mhz clock.
    
    ; Step 2: Send the '00000011' instruction to the LCD. 
    ; We can't use 'lcd_instruction' right now because the LCD 'busy' flag isn't available yet.
    LDA #%00000011
    STA PORTB
    ORA #E ; Send the same thing, but with the 'enable' bit set. This is how we send an instruction to the LCD.
    STA PORTB
    EOR #E ; Send the same thing, but with the 'enable' bit cleared.
    STA PORTB
    JSR lcd_init_delay  ; Wait at least 4.1 milliseconds

    ; Step 3: Send the same instruction again to the LCD.
    LDA #%00000011
    STA PORTB
    ORA #E ; Set the 'enable' bit
    STA PORTB
    EOR #E ; Clear the 'enable' bit
    STA PORTB
    JSR lcd_init_delay ; Wait at least 100 microseconds (0.1 milliseconds)

    ; Step 4: Send a third and final '00000011'
    ; At this point, we can now use 'lcd_instruction' to help us send an instruction.
    LDA #%00000011
    JSR lcd_instruction

    ; Step 5: Send '00000010' to indicate that we want to use 4-bit mode instead of 8-bit mode.
    LDA #%00000010 ; Send the instruction to set 4-bit mode. 
    JSR lcd_instruction

    ; --- LCD Initialisation ---
    
    ; Let's now send a series of options that will set it to the configuration we want.
    ; For more details of the options, see page 24 of the Hitachi HD44780U datasheet.
    LDA #%00101000 ; Function set: 2-line display; 5x8 font
    JSR lcd_instruction
    LDA #%00001111 ; Display on/off control: display on, cursor on, cursor blinking.
    JSR lcd_instruction
    LDA #%00000110 ; Entry mode set: Increment, display shift.
    JSR lcd_instruction
    LDA #%00000001 ; Clear the display.
    JSR lcd_instruction
    LDA #%11000000 ; put cursor at position 40.
    JSR lcd_instruction

    ; Reset the part in memory that stores the time elapsed (in 'centiseconds') since boot.
    LDA #0
    STA TIME
    STA TIME + 1
    STA TIME + 2
    STA TIME + 3
    STA TIME + 4

    ; To print characters, BBC BASIC uses the address stored in $020F-$020E. We need to load those addresses with our OSWRCH routine.
    LDA #>OSWRCHV ; Get the high byte of the write character routine.
    STA $020F ; Store it in $020F.
    LDA #<OSWRCHV ; Get the low byte of the write character routine.
    STA $020E ; Store it in $020E

    ; -- Print the boot message --

    LDY #<bootMessage ; Store the lower 4 bits of the first part into the Y register.
    LDA #>bootMessage ; Store the upper 4 bits into A register.
    STA $FE ; Store the high byte of the source address.
    LDA #0 ; Load 0 into A
    STA $FD ; Use 0 as the low byte of the address, using offset from Y for source.
printBootMessageLoop:
    LDA ($FD),Y ; Read the character at $FD, offset by the value of Y.
    JSR OSASCI ; Send the character to the ACIA to transmit out of the 'Tx' pin.
    INY ; Step to the next character.
    CMP #$0 ; A '0' lets BBC Basic know when to stop reading. Let's check if that's the case.
    BNE printBootMessageLoop ;  If A is not 0, read the next character.

    ; -- Enter BBC BASIC --

    CLC ; Clear the carry bit to tell the BBC BASIC we are entering from RESET.
    LDA #$01 ; Load '1' into the accumulator to tell BBC BASIC we are starting up.
    CLI ; Enable interrupts, now that we're done initialising all our memory and peripherals.
    JMP BASIC ; Enter BBC BASIC! 
    ; This is the end of the reset sequence.


; -- OS Call Routines --

; OSRDCH: 'OS Read Character'
; This subroutine waits for a character to arrive from the ACIA, then returns it in A. Cy=Esc pressed.
; We use this to receive input from your keyboard to the the caller.
; It also checks if the escape key has been pressed. If it has, it lets the caller know so it needs to leave whatever it's running.
OSRDCHV:
    ; First, check for escape flag
    BIT OSESC ; if the escape flag set?
    BMI escapeCondition ; Skip reading and jump to escape handling.

    ; If there's no escape flag set, let's check the READBUFFER to see if it's full.
    ; We don't read the ACIA directly here. We use the IRQ interrupt handler to read the character and place it into READBUFFER.
    ; A full READBUFFER essentially means that there's a character that's been received by the ACIA that hasn't been read yet.
    LDA READBUFFER ; Is there something in the buffer?
    BEQ OSRDCHV ; If it's still empty, keep waiting.
    CMP #$1B
    BEQ escapeCondition ; Escape key pressed
    PHA ; Otherwise, it's full. Let's save A to the stack so we can use it for later.
    LDA #0
    STA READBUFFER ; Clear the character buffer
    PLA ; Restore A from the stack
    CLC ; CC=Not Escape
    RTS ; Return to the main routine
escapeCondition:
    ; If we're here, that means that the user just pressed the escape key. This should signal to BBC BASIC to stop whatever it's doing.
    LDA #0
    STA READBUFFER ; clear the character buffer by writing '0' to it
    SEC ; CS=Escape key pressed
    LDA #$1B ; Load the 'ESC' ASCII character into A.
    RTS

; OSWRCH: 'OS Write Character'
; System call that displays whatever character is in A. This doesn't necessarily have to be an ASCII character.
; The 'V' in "OSWRCHV" means "Vector". When BBC BASIC jumps to the OSWRCH address, it jumps straight to here.
OSWRCHV:
    STA ACIA_DATA ; Send the character to the ACIA where it will immediately try to transmit it through 'Tx'.
    PHP ; Save caller's interupt state
    SEI ; Disable interrupts while we are printing a character.
    JSR print_char ; Also print the same character to the LCD.
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
    PLP ; Restore caller's interupt state
    CLI ; Enable interrupts again
    RTS ; 6 cycles

; OSBYTE: 'OS Byte'
; A group of system calls that only involve up to two bytes into the X and Y registers.
; Which system call to do is determined by whatever value is currently in the A register.
; There are much more OSBYTE system calls, but we only need three for the time being:
; On exit: A=preserved, X=any return value, Y=any return value for calls >$7F else preserved, Cy=any return value for calls >$7F
    ; OSBYTE $7E: "Acknowledge Escape" - Handles how BBC BASIC leaves what it's doing when the user presses the escape key.
    ; OSBYTE $84: "Read HIMEM" - This tells the caller the maximum memory address we can use for BASIC programs. $4000 by default
    ; OSBYTE $83: "Read OSHWM" - This tells the caller the minimum memory address we can use for BASIC programs (A.K.A the start of 'PAGE' memory). 
        ; $0800 by default, because we need to reserve $0100-$03FF for the MOS, and $400-$7FF for fixed space for the language.
OSBYTEV: 
    CMP #$7E ; is it the 'acknowledge escape' system call?
    BEQ OSBYTE7E ; Jump to the 'acknowledge escape' routine.
    CMP #$84 ; Is it the 'read top of memory' system call?
    BEQ OSBYTE84 ; Put address '$4000' in YX registers.
    CMP #$83 ; Is it the 'read bottom of memory' system call?
    BEQ OSBYTE83 ; Put address '$0080' in YX registers.
    RTS ; Otherwise, return with nothing. 

OSBYTE7E: ; Routine that 'acknowledges' the escape key has been pressed.
    LDX #0 ; Reset X
    BIT $FF   ; check for the ESCAPE flag. 'BIT' just checks bit 7 and 6.
    BPL clearEscape  ; if there's no ESCAPE flag then just clear the ESCAPE condition.
    LDX #$FF   ; If escape HAS been pressed, set X=$FF to indicate ESCAPE has been acknowledged
clearEscape:
    CLC    ; Clear the carry bit to let caller know there's no more escape state to process.
    ROR $FF ; set/clear bit 7 of ESCAPE flag
    RTS 

OSBYTE84: ; Routine to return the highest address of free RAM space.
    ; Put address '$4000' in YX registers.  
    LDY #$40 ; High byte goes into Y
    LDX #$00  ; Low byte goes into X
    RTS

OSBYTE83: ; Routine to return the lowest address of free RAM space.
    ; Put address '$0800' in YX registers. Anything below $0800 is memory space reserved by BBC MOS.
    LDY #$08 ; High byte goes into Y
    LDX #$00  ; Low byte goes into X
    RTS

; OSWORD: 'OS Word'
; A group of system calls that involves more than just a couple of bytes, but an area in RAM.
; BBC MOS uses 'Control Blocks' to define a sequence of bytes. They're a bit hard to explain, but all you need to know right now is that it's an area in RAM.
; There are much more OSWORD system calls, but we only need three for the time being:
    ; OSWORD 0: "Read line from current input" - This is how BBC MOS lets you input a line of text.
    ; OSWORD 1: "Read system clock" - Get the number of 'centiseconds' since boot. This is called by the TIME function in BASIC.
    ; OSWORD 2: "Write system clock" - Set the number of 'centiseconds' since boot to a certain value. This is called by "TIME=[value]" in BASIC.
OSWORDV:
    PHP  ; Preserve caller's IRQ state.
	SEI	 ; Disable interrupts for now, so we don't change a character mid-print.
	
    ; Store A, X, and Y registers in MOS API workspace.
	STA	OSAREG
	STX	OSXREG			
	STY	OSYREG				

    CMP #$00        ; Is it the 'Read Line' system call?
    BEQ OSWORD0V    ; If yes, start reading input from the user.
    CMP #$01        ; Is it the 'Read Clock' system call?
    BEQ OSWORD1V    ; Jump to it if yes
    CMP #$02        ; Is it the 'Write Clock' system call?
    BEQ OSWORD2V    ; Jump to it if yes
    PLP             ; Restore caller's IRQs
    RTS             ; Otherwise, return with no change.

OSWORD0V:
    STA READBUFFER ; Write a '0' to the character buffer, in case there's an escape character currently in there. Kind of a hacky solution, but it works!

    ; An OSWORD 0 control block has a couple of bytes of metadata to help us:
    ; 4th byte: Maximum ASCII code allowed
    ; 3rd byte: Minimum ASCII code allowed
    ; 2nd byte: Maximum line length allowed
    ; 1st byte: Address of the first character [I think?]
    LDY #4 ; Load 
osword0setup:
    ; Store max/min ASCII codes, and max line length from zero page memory to main memory
    LDA (OSXREG),Y
    STA $02B3-2,Y ; TODO: Remember and explain why we need to subtract 2.
    DEY
    CPY #2 ; loop until it's 1
    BCS osword0setup

    ; store input buffer addresses into memory (temporary buffer)
    LDA (OSXREG),Y ; Get value (high byte) from zero-page. Y is 1 right now.
    STA $E9 ; Store into temporary buffer (high byte)

    LDY #$00
    STY $0269 ; [store 0 in 'paged mode counter'? This needs be here, otherwise we get 'Syntax Error' for everything]

    LDA (OSXREG),Y ; Get value (low byte) from zero-page
    STA $E8 ; Store into temporary buffer (low byte)

    CLI ; Explicitly enable interrupts to allow background keypress processing
    BCC readInputCharacter

readLineInputBufferFull:
    LDA #07 ; Send a 'bell character'
retryWithoutIncrement:
    DEY ; Decrement Y. We are essentially 'cancelling out' the next instruction.
retryWithIncrement:
    INY ; Decrement Y. Y is currently holding the current position in the input.
outputAndReadAgain:
    JSR OSWRCH ; Print the character. Fall through to 'readInputCharacter'
readInputCharacter:
    JSR OSRDCH ; Read the next character from ACIA
    BCS Escape

    CMP #$08 ; Is it a backspace? Let's delete the last character.
    BEQ delete
    CMP #$7F ; Is it a delete? Let's delete the last character.
    BEQ delete

    BNE checkLowercase ; Otherwise, move on
delete:
    CPY #0 ; Are we at the first character?
    BEQ readInputCharacter ; Then do nothing
    DEY ; Otherwise, go back 1
    BCS outputAndReadAgain ; Write the delete character. Go back to the start.
checkLowercase: 
    CMP #$61        ; Compare with 'a'
    BCC notLower    ; If less than 'a', it's not a lowercase letter
    CMP #$7B        ; Compare with 'z'
    BCS notLower    ; If greater than 'z', it's not a lowercase letter
    AND #$DF        ; Clear the 5th bit to convert to uppercase
notLower:
    STA ($E8),Y ; store character into the buffer
    CMP #$0D ; is it the newline character?
    BEQ newLineAndExit ; then finish

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
    PLP ; Restore flags
    LDA $FF ; Get escape flag
    ROL ; Put bit 7 into the carry bit in the status register
    RTS
Escape:
    PLP
    SEC
    RTS

; OSWORD 1: Read System Timer
; The variable TIME is a 5-byte variable starting at address 'TIME'.
; To read the timer, let's loop through the 5 bytes and store them in the control block
OSWORD1V:
    LDX #0                              
readTimer:
    LDY #4 ; Use this to read the 5 bytes. This will run down from 4 to 0.
readTimerLoop:
    LDA TIME,X ; Load the TIME byte, offset by X. X will be either 0, 1, 2, 3, or 4.
    STA (OSXREG),Y ; Store into control block offset by Y. Y will be either 4, 3, 2, 1, or 0.               
    INX                                                
    DEY                 
    BPL readTimerLoop ; Loop while Y is still greater than 0. BPL = "Branch on PLus"
    PLP ; Restore caller's IRQ state
    RTS

; OSWORD 2: Write System Timer
; To write the timer, let's essentially do the opposite of 'Read System Timer'
; Let's loop through the 5 bytes in control block, and store them in the 5-byte variable starting at address 'TIME'.
OSWORD2V:
    LDX #0
    LDY #4
writeTimerLoop:
    LDA (OSXREG),Y ; Same principle as 'readTimerLoop'.
    STA TIME, X
    INX
    DEY
    BPL writeTimerLoop
    PLP ; Restore caller's IRQ state
    RTS

; -- Keyboard Interrupt Routines --

keyboard_interrupt:
    PHA ; Save A
    TXA
    PHA ; Save X

    ; First, check if we are releasing a key
    LDA KEYBOARD_FLAGS
    AND #RELEASE
    BEQ read_key ; Skip ahead if we are not releasing a key

    ; If we ARE releasing a key, let's clear the release flag
    LDA KEYBOARD_FLAGS
    EOR #RELEASE ; Flip the release bit. 
    STA KEYBOARD_FLAGS
    LDA PORTA ; read to clear the interrupt
    
    ; Check if we are releasing the shift key:
    CMP #$12 ; is it the left shift?
    BEQ shift_up ; clear the shift flag
    CMP #$59 ; is it the left shift?
    BEQ shift_up ; clear the shift flag
    
    JMP keyboard_interrupt_exit ; We've processed a released key. Skip to the end.

; Routine to clear the 'Shift' flag when we release the Shift key.
shift_up:
    LDA KEYBOARD_FLAGS
    EOR #SHIFT
    STA KEYBOARD_FLAGS
    JMP keyboard_interrupt_exit

; Routine to set the 'Shift' flag when we press and hold the Shift key.
shift_down:
    LDA KEYBOARD_FLAGS
    ora #SHIFT
    STA KEYBOARD_FLAGS
    jmp keyboard_interrupt_exit

; Routine to process what's in PORTA, and store it into READBUFFER for reading later.
read_key:
    LDA PORTA
    CMP #$F0 ; If we've read $F0, that means the keyboard is signalling a key was released.
    BEQ key_release ; Jump ahead to setting the 'release' flag.

    CMP #$12 ; Left shift was pressed?
    BEQ shift_down ; Set the shift flag [TODO: investigate why my build starts with the shift flag set by default]
    CMP #$59 ; Right shift was pressed?
    BEQ shift_down ; Set the  shift flag

    ; Convert the PS/2 scancode to an ASCII code.
    TAX ; Transfer the scancode to X register.
    LDA KEYBOARD_FLAGS
    AND #SHIFT
    BNE shifted_key; If yes, convert it to a capitalised ASCII set.

unshifted_key:
    LDA keymap,X ; Use the 'keymap' to convert the scancode. Scancode is in X, which will convert to an ASCII stored in A.
    JMP push_key ; Move ahead to store the ASCII for processing.

shifted_key:
    LDA keymap_shifted,X
    JMP push_key

; Now that we have the ASCII character stored in A, let's store it in READBUFFER for processing later.
push_key:
    STA READBUFFER ; Store the ASCII into READBUFFER
    CMP #$1B ; Is the character an escape character?
    BNE keyboard_interrupt_exit ; If not, we are done.
    LDA #$FF ; If it IS the escape character, we need to signal that an escape state is active. 
    STA $FF ; set the 'escape flag' address at $FF to the value #$FF.
    JMP keyboard_interrupt_exit ; We are done.

; Routine to set the 'Release' flag when we release any key.
key_release:
    LDA KEYBOARD_FLAGS
    ORA #RELEASE
    STA KEYBOARD_FLAGS
    JMP keyboard_interrupt_exit ; We are done.

keyboard_interrupt_exit:
    PLA ; Restore X
    TAX
    PLA ; Restore A
    RTS ; Return back to the interrupt handler


; -- LCD Routines --

; High-level overview on how to send an instruction to the LCD in 4-bit mode:
; 1. Send the high 4 data bits of the instruction to PORTB.
;    'RS' and 'RW' will be depending on the type of instruction. Most of them have RS and RW set to 0.
; 2. Send the same 4 bits with the 'Enable' bit set (Bit 7 by default), and then send with it cleared.
; 3. Now the LCD is expecting the next 4 bits. Send the low 4 data bits of the instruction.
; 4. Again, send the same thing with the 'E' bit set, and then with it cleared. 

; Before you send any instruction, you need to make sure the LCD isn't busy with the last instruction.
; You can check this by checking the 'busy flag' of the LCD. See the 'lcd_wait' routine for details.
lcd_instruction:
    JSR lcd_wait ; Wait until the LCD is ready for another instruction.
    PHA ; Save the original value of A, because we're going to be messing with it next
    LSR ; LSR = 'Logical Shift Right'. 
    LSR
    LSR
    LSR ; Four of these instructions will shuffle the high 4 bits down the low 4 bits.
    STA PORTB ; Send it to the LCD
    ORA #E ; Set E bit to send instruction
    STA PORTB
    EOR #E ; Clear E bit
    STA PORTB
    PLA ; Get back the original A
    AND #%00001111 ; Mask the high 4 bits we already sent
    STA PORTB ; Send the low four bits to the LCD.
    ORA #E ; Set E bit to send instruction
    STA PORTB
    EOR #E ; Clear E bit
    STA PORTB
    RTS ; Return to where we were before.

; Routine to keep the 6502 waiting until the LCD isn't busy anymore.
lcd_wait:
    PHA ; Save the original value of A.
    LDA #%11110000 ; We need to set the lower four bits of PORTB to 'input' to read the busy flag.
    STA DDRB
lcdbusy:
    LDA #RW ; Checking the busy flag is like sending an instruction, but we have the 'RW' bit enabled at all times.
    STA PORTB
    LDA #(RW | E)
    STA PORTB
    LDA PORTB ; Read the high four bits. The first bit will have the busy flag.
    PHA  ; Save the value for processing later.
    LDA #RW ; We aren't interested in the lower four bits, but we need to read them anyway so we can finish the instruction properly.
    STA PORTB
    LDA #(RW | E)
    STA PORTB
    LDA PORTB
    PLA ; Get back the value of the busy flag.
    AND #%00001000 ; Isolate it to just the busy flag and nothing else.
    BNE lcdbusy ; Is the busy flag set? Let's loop and try again.

    LDA #RW ; Otherwise, let's finish the instruction and return
    STA PORTB
    LDA #%11111111  ; Set PORTB data direction back to the default: all output.
    STA DDRB
    PLA ; Restore the original value of A
    RTS

; LCD Routine to read the current position of the cursor.
; This routine is especially useful for the 'Backspace' and 'Enter' routines.
lcd_instruction_read_cursor:
    JSR lcd_wait ; As always, make sure the LCD isn't bust with the previous instruction.
    LDA #%11110000  ; LCD data is input
    STA DDRB
    LDA #RW ; Send the instruction to read the cursor counter.
    STA PORTB
    LDA #(RW | E)
    STA PORTB

    LDA #0
    STA LCDCURSORBUFFER ; Clear the 'LCD Cursor Buffer', because we're going to be using it.

    LDA PORTB ; read high nibble
    AND #%00000111 ; mask out the busy flag
    ROL ; ROL = 'ROtate Left' 
    ROL
    ROL
    ROL ; Four of these will shuffle the low 4 bits to the high 4 bits.
    STA LCDCURSORBUFFER ; Store the high 4 bits into memory.
    LDA #RW ; Now we need to read the low 4 bits and save that too.
    STA PORTB
    LDA #(RW | E)
    STA PORTB
    LDA PORTB ; Read the low 4 bits
    AND #%00001111 ; Make sure its only the low 4 bits
    ORA LCDCURSORBUFFER ; Connect the high 4 bits from before with the low 4 bits now to form a complete cursor read.
    STA LCDCURSORBUFFER ; Store it into the 'LCD Cursor Buffer' for reading later.

    LDA #RW ; Finish the instruction by clearing the 'Enable' bit.
    STA PORTB

    LDA #%11111111  ; Set data direction back to default.
    STA DDRB
    LDA LCDCURSORBUFFER ; Leave the routine with the cursor value stored in A.
    RTS 

; LCD Routine to read the last printed ASCII character.
; Right now this routine is only used for the 'Enter' LCD sequence.
lcd_instruction_read:
    JSR lcd_wait ; Make sure the LCD isn't busy
    LDA #%11110000  ; LCD data is input
    STA DDRB
    LDA #(RS | RW) ; send instruction
    STA PORTB
    LDA #(RS | RW | E)
    STA PORTB

    LDA #0
    STA LCDREADBUFFER ; Clear the 'LCD Read Buffer' since we are about to use it.
    LDA PORTB ; Read the high nibble
    AND #%00001111 ; Isolate to just 4 bits.
    ROL
    ROL
    ROL
    ROL ; Shuffle the bits from the lower four to the upper four.
    STA LCDREADBUFFER ; Store it for later.
    LDA #(RS | RW)
    STA PORTB
    LDA #(RS | RW | E)
    STA PORTB
    LDA PORTB       ; Read low nibble
    AND #%00001111 ; Isolate to just 4 bits.
    ORA LCDREADBUFFER ; Connect the high 4 bits from before with the low 4 bits now to form a complete ASCII character read.
    STA LCDREADBUFFER ; Store it in memory

    LDA #(RS | RW)
    STA PORTB

    LDA #%11111111  ; Set data direction back to default.
    STA DDRB
    LDA LCDREADBUFFER ; Leave the routine with the ASCII character value stored in A.
    RTS

; LCD routine to print the character you've stored in the A register.
; This also handles things like backspace, escape, and enter.
print_char:
    PHA ; Save the original value of A

    CMP #$1B ; is the escape character?
    BEQ lcd_print_escape ; Go to the escape hander.
    CMP #$08 ; is it backspace?
    BEQ lcd_print_backspace ; Go to the backspace handler.
    CMP #$0C ; is it the 'Form Feed' character? This is how we clear the screen.
    BEQ lcd_clear_screen
    CMP #$0D ; is it a carriage return?
    BEQ lcd_print_enter  ; Go to the enter handler.

    CMP #$20 ; Check minimum ASCII character ($20 = Space character)
    BCC print_char_exit

    JMP print_ascii ; Otherwise, let's print it.
    
print_char_exit:
    JMP exit_lcd ; This is a JMP instead of a branch because the exit_lcd routine address is too far away from here to branch.
lcd_print_escape:
    LDA #%00000001 ; If the escape character was pressed, let's clear the display.
    JSR lcd_instruction
    JMP exit_lcd ; Leave early.
lcd_clear_screen:
    LDA #%00000001 ; If the 'CLS' command was sent, let's clear the display.
    JSR lcd_instruction
    LDA #%11000000 ; Put cursor at the start of the second line (Position 40)
    JSR lcd_instruction
    JMP exit_lcd ; Leave early.

; Enter handling is a little tricky: What we want to do is transfer the second line to the first line.
; Here's a high level overview:
; 1. Reset the cursor to the start of the second line.
; 2. Read each character of the second line, and store it into memory.
; 3. Clear the display. This sets the cursor to the start of the first line.
; 4. Read each character in memory and write them one by one to the second line.
; 5. Finish by placing the cursor back to the start of the second line.
lcd_print_enter:
    LDA #%11000000 ; Put cursor at the start of the second line (Position 40)
    JSR lcd_instruction
    LDX #$0 ; Reset X and A for later use.
    LDA #0
lcd_print_enter_read_line_loop:
    JSR lcd_instruction_read ; For each character in the LCD line, read it
    STA LCDBUFFER,X ; Store it into memory.
    INX
    CPX #$27
    BCC lcd_print_enter_read_line_loop ; Keep going while we haven't hit the end of line 2 (Position 40 + $27).
lcd_print_enter_clear:
    LDA #%00000001 ; Clear the display
    JSR lcd_instruction
    LDX #$0 ; Reset X in preparation for the next loop.
lcd_print_enter_write_line_loop:
    LDA LCDBUFFER,X ; Load the next character from memory
    JSR print_char ; Print it to the LCD
    LDA #0
    STA LCDBUFFER,X ; Clear that space in memory, since we have already read it.
    INX
    CPX #$27 
    BCC lcd_print_enter_write_line_loop ; Keep going while we haven't hit the end of line 1 (Position 0 + $27).

    LDA #%11000000 ; Now that we're finished writing, let's set the cursor to the start of line 2.
    JSR lcd_instruction

    JMP exit_lcd ; We're done!

; Overview of the backspace handler:
; 1. Move the cursor back one
; 2. Print a space ASCII character, essentially clearing the last printed character.
; 3. Move cursor left again to the cleared character space.
; 4. Decide if we need to shift the display view or not (More details below)
lcd_print_backspace:
    ; Shift cursor left
    LDA #%00010000
    JSR lcd_instruction
    LDA #' ' ; Print a space
    JSR print_char

    ; Shift cursor left again
    LDA #%00010000
    JSR lcd_instruction

    ; If the cursor position is greater than $50, that means we've travelled beyond the view of the 16x2 LCD display.
    ; If this happens, we need to 'shift' the display so we can still see what we are doing.
    JSR lcd_instruction_read_cursor ; Get the current cursor position
    CMP #$4F ; 40 + 16 (-1 to cancel out the space we printed)
    BCC exit_lcd ; If the cursor is within the frame, no need to do anything.

    JSR lcd_instruction_read_cursor
    CMP #$6F ; if we are at the very end of the 2nd line's buffer, we should not be shifting.
    BEQ exit_lcd

    ; If we are here, this means we moved backspace while out of the main view. We need to shuffle the display back.

    ; Shift display right to compensate for the backspace.
    LDA #%00011100
    JSR lcd_instruction

    ; Shift the display right again to cancel out the space we printed.
    LDA #%00011100
    JSR lcd_instruction

    JMP exit_lcd ; We are now done. Leave early.


print_ascii:
    JSR lcd_wait
    PHA ; Save the original value of A
    LSR ; Shuffle the high 4 bits down to the low 4 bits.
    LSR
    LSR
    LSR ; Send the high 4 bits
    ORA #RS ; Set 'RS' to signal we want to print a character.
    STA PORTB
    ORA #E ; Set E bit to send instruction
    STA PORTB
    EOR #E ; Clear E bit
    STA PORTB
    PLA ; Restore A to send the low four bits.
    AND #%00001111 ; Send low 4 bits
    ORA #RS ; Set RS
    STA PORTB
    ORA #E ; Set E bit to send instruction
    STA PORTB
    EOR #E ; Clear E bit, but still preserving RS.
    STA PORTB

    ; If we've used up all of the main view (16 characters), we need to shift the display along so we can still read what we are doing.
    JSR lcd_instruction_read_cursor ; Get the current position of the cursor.
    CMP #$50 ; 40 + 16 (-1 to cancel out the space we printed)
    BCC exit_lcd ; If we're inside the frame, no need to do anything.

    ; If we're here, that means we're outside the main view. 
    LDA #%00011000 ; Shift display down so we can see the newly printed character.
    JSR lcd_instruction
exit_lcd:
    PLA ; Get back the original value of A
    ; We just used the carry bit for conditional branching. 
    ; We need to clear the carry bit afterwards because the carry bit indicates escape handling.
    CLC ; CLC = "CLear Carry"
    RTS ; Return to where we were before.


lcd_init_delay:
    LDX #$0F        ; Load X register with the high byte of 4000 (4000 in hexadecimal is 0x0FA0)
    LDY #$A0        ; Load Y register with the low byte of 4000
lcd_init_delay_loop:
    DEY             ; Decrement Y
    BNE lcd_init_delay_loop  ; Branch if not zero (Y != 0) to delay_loop
    DEX             ; Decrement X
    BNE lcd_init_delay_loop  ; Branch if not zero (X != 0) to delay_loop
    RTS

; -- Interrupt Handling --

; Subroutine called after every NMI or IRQ in hardware, or the BRK instruction in software.
interrupt:
    STA OSINTA ; Save A for later.
    CLD ; Ensure we are operating in binary.                    
    PLA ; get status register. it's on the stack at this point
    PHA ; put the status flags back on the stack
    AND #$10 ; Check if it's a BRK or an IRQ.
    BEQ irqv 
    JMP BRKV ; If it's BRK, that's an error. Go to the BRK vector.
irqv: ; Otherwise, it's an IRQ. Let's check what caused the interrupt, starting with the ACIA.
    LDA ACIA_STATUS
    AND #$88 ; Check the ACIA status register to find out if the ACIA is asking to read a character.
    BEQ irq_keyboard ; If there's nothing to read, then we'll try the keyboard
irq_acia:
    LDA ACIA_DATA ; Read the ACIA. Because reading the ACIA clears the data, this is the only place allowed to read it directly!
    STA READBUFFER ; For everywhere else that needs to access the character, we will store in memory.
    CMP #$1B ; check if an escape key was pressed
    BNE end_irq ; If it's not an escape key, we've done everything we need. Skip to the end.
irq_escape: ; If an escape key was pressed, let's set the escape flag.
    LDA #$FF
    STA OSESC ; set the 'escape flag'.
    JMP end_irq ; Skip to the end.
irq_keyboard: ; If we've ruled out the ACIA, then let's try the keyboard.
    LDA IFR ; Check the "Interrupt Flag Register" to make sure it was the keyboard that caused the interrupt.
    AND #%00000010 ; We have to check bit 2.
    BEQ irq_via_tick ; Bit 2 isn't set? Let's asume it was the timer instead.
    JSR keyboard_interrupt ; Jump to the routing that reads PORTA and stores the character into READBUFFER.
    JMP end_irq ; Finish the interrupt.
irq_via_tick: ; If we've ruled out the ACIA, then let's assume it was the VIA timer.
    LDA T1CL ; Clear the interrupt by reading the timer.
    INC TIME + 4 ; Increment the 4th byte, which holds the lowest byte.
    BNE end_irq ; If the byte didn't overflow from FF to 00, then we've done all we need. Skip to the end.
    INC TIME + 3 ; If it DID overflow, then let's carry the 1 to the next register.
    BNE end_irq ; If the byte didn't overflow from FF to 00, then we've done all we need. Skip to the end.
    INC TIME + 2 ; If it DID overflow, then let's carry the 1 to the next register.
    BNE end_irq ; etc etc
    INC TIME + 1
    BNE end_irq
    INC TIME
end_irq:
    LDA OSINTA ; Restore what was in the A register before we were so rudely interrupted
    RTI ; "ReTurn from Interrupt" Restore caller's flags, return to caller.


; -- BREAK Handler --

; Handler for interrupts that we know were called by the BRK instruction. This means an error was reported.
; The BBC MOS API defines the structure of an error message. To get the message, we need to store the location of the error message in addresses $FD and $FE. 
BRKV:
    TXA ; Save X to the stack, so we can restore it later
    PHA

    TSX             ; Get the value of the stack pointer and store it in X
    LDA $0100 + 3,X ; Get the low byte of the error message. This is stored in the stack.
    SEC             ; Set the carry bit in the 6502 status register.
    SBC #1          ; Subtract 1. SBC = "SuBtract if Carry set". This will always subtract 1 because we just set it. Need to subtract one as BRK stacks address of BRK+2 rather than the BRK+1 that we need.
    STA OSFAULT         ; Store it into $FD
    
    LDA $0100 + 4,X ; Get the high byte of the error message. This is stored in the stack.
    SBC #0          ; Subtract 1 if the carry bit is set.
    STA $FE         ; Store high byte into $FE
    STX OSXREG         ; Store the stack pointer in OSXREG.

    PLA             ; Restore the original value of X
    TAX                 

    ; Jump to current error handler, which takes it from there. It's stored in $0202.
    JMP ($0202)     


   .org $c500

; Keep this 14.2kb of EEPROM space open for the SAVE/LOAD feature in BeebEater v1.0.
; In other words, Everything above must be less than 1152 bytes. As of v0.4, we are currently at 1143 bytes. 
; This space will allow you to save/load your BBC BASIC program to ROM storage!

   .org $fd00
keymap:
    .byte "????????????? `?" ; 00-0F
    .byte "?????q1???zsaw2?" ; 10-1F
    .byte "?cxde43?? vftr5?" ; 20-2F
    .byte "?nbhgy6???mju78?" ; 30-3F
    .byte "?,kio09??./l;p-?" ; 40-4F
    .byte "??'?[=????",$0d,"]?\??" ; 50-5F
    .byte "??????",$08,"??1?47???" ; 60-6F
    .byte "0.2568",$1b,"??+3-*9??" ; 70-7F
    .byte "????????????????" ; 80-8F
    .byte "????????????????" ; 90-9F
    .byte "????????????????" ; A0-AF
    .byte "????????????????" ; B0-BF
    .byte "????????????????" ; C0-CF
    .byte "????????????????" ; D0-DF
    .byte "????????????????" ; E0-EF
    .byte "????????????????" ; F0-FF
keymap_shifted:
    .byte "????????????? ~?" ; 00-0F
    .byte "?????Q!???ZSAW@?" ; 10-1F
    .byte "?CXDE#$?? VFTR%?" ; 20-2F
    .byte "?NBHGY^???MJU&*?" ; 30-3F
    .byte "?<KIO)(??>?L:P_?" ; 40-4F
    .byte '??"?{+?????}?|??' ; 50-5F
    .byte "?????????1?47???" ; 60-6F
    .byte "0.2568???+3-*9??" ; 70-7F
    .byte "????????????????" ; 80-8F
    .byte "????????????????" ; 90-9F
    .byte "????????????????" ; A0-AF
    .byte "????????????????" ; B0-BF
    .byte "????????????????" ; C0-CF
    .byte "????????????????" ; D0-DF
    .byte "????????????????" ; E0-EF
    .byte "????????????????" ; F0-FF


    ; BBC MOS system calls. Code call these by jumping to their place in memory.
    ; Most of them jump to a 'vector' that properly handles the system call.
    
    .org $FFB6
    .byte 0,0,0 ; no vector table
    .byte $60,$60,$60 ; FFB9
    .byte $60,$60,$60 ; FFBC
    .byte $60,$60,$60 ; FFBF
    .byte $60,$60,$60 ; FFC2
    .byte $60,$60,$60 ; FFC5
    .byte $60,$60,$60 ; FFC8
    .byte $60,$60,$60 ; FFCB
    .byte $60,$60,$60 ; FFCE
    .byte $60,$60,$60 ; FFD1
    .byte $60,$60,$60 ; FFD4
    .byte $60,$60,$60 ; FFD7
    .byte $60,$60,$60 ; FFDA
    .byte $60,$60,$60 ; FFDD

    JMP OSRDCHV ; FFE0
    .org OSASCI ; FFE3
    CMP #$0D ; Is it carriage return? Jump to OSNEWL, otherwise fall through to OSWRCH.
    BNE OSWRCH
    ; If it's carriage return, fall through to OSNEWL

    .org OSNEWL ; OSNEWL is essentially OSWRCH, but with a line break (CR+LF)
    LDA #$0A ; Send 'Carriage Return' character.
    JSR OSWRCH
    LDA #$0D ; Send a 'Line Feed' character. CR+LF make up a complete line break.
    ; fall through to OSWRCH

    .org OSWRCH
    JMP OSWRCHV ; At address 'OSWRCH', jump to the 'OSWRCHV' routine (AKA a 'vector').

    .org OSWORD
    JMP OSWORDV

    .org OSBYTE
    JMP OSBYTEV
    .byte $60,$60,$60 ; FFF7

    ; 6502-specific calls, such as interrupts and resets.
    .org NMI
    .word interrupt ; at NMI, go to interrupt handler
    .word reset ; at RESET address, go to reset label
    .word interrupt ; When IRQ goes low or BRK is called, go to the interrupt handler

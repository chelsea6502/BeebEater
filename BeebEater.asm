; BeebEater v0.9.1 - BBC BASIC port for the Ben Eater 6502.
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
LCDBUFFER       = OSVDUWS ; For storing a line of LCD characters.
LCDREADBUFFER =   OSVDU
LCDWRITEBUFFER = OSVDU+1

INPUTBUFFER = $0800
INPUTBUFFERREAD = $50
; Keep $51 open
INPUTBUFFERWRITE = $52
; Keep $52 open

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

    ;incbin "Basic2.rom"  ; Want to run BASIC programs designed for the BBC Micro? Try version 2 instead.
                                ; Download the one from the "Acorn BBC Micro" section.

    .org START ; set the start of BeebEater at $C000.

; -- ROM Constants. Unlike the constants before, these are actually stored in the EEPROM. --

; Define the boot message. By default, you should see this at boot:
;;; BeebEater Computer 16k
;;;
;;; BASIC
;;;
;;; >
bootMessage:
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

    ; Intialise ACIA the command register
    LDA #%00001001 ; No parity, no echo, with interrupts after every time we recieve a byte.
    STA ACIA_CMD

    ; Initialise the ACIA control register
    LDA #%00010000 ; 1 stop bit, 8 bits, 16x baud. ('16x' means 115200 on a 1.8432Mhz clock)
    STA ACIA_CTRL

    ; --- VIA 6522 Initialisation ---

    LDA #%00000000 ; Set PORTA (for the keyboard) to input.
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

    ; --- LCD Reset Sequence ---
    ; We will now go through the LCD reset sequence, as instructed in page 47 of the Hitachi 44780U LCD controller datasheet.

    ; Step 1: Wait 15ms after LCD gets power.
    JSR delay_15ms ; This routine waits about 15 milliseconds when at a 1mhz clock.
    
    ; Step 2: Send the '00000011' instruction to the LCD. 
    ; We can't use 'lcd_instruction' right now because the LCD 'busy' flag isn't available yet.
    LDA #%00000011
    STA PORTB

    LDA #E
    TSB PORTB ; Set the 'Enable' bit on PORTB
    TRB PORTB ; Clear the 'Enable' bit on PORTB
    JSR delay_4100us  ; Wait at least 4.1 milliseconds

    ; Step 3: Send the same instruction again to the LCD.
    TSB PORTB
    TRB PORTB
    JSR delay_100us ; Wait at least 100 microseconds (0.1 milliseconds)

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
    STZ TIME
    STZ TIME + 1
    STZ TIME + 2
    STZ TIME + 3
    STZ TIME + 4

    ; Initialise KEYBOARD_FLAGS to 0
    STZ KEYBOARD_FLAGS

    LDA #$08
    STA INPUTBUFFERREAD+1
    STA INPUTBUFFERWRITE+1

    JSR flushBuffer

    ; To print characters, BBC BASIC uses the address stored in $020F-$020E. We need to load those addresses with our OSWRCH routine.
    LDA #>OSWRCHV ; Get the high byte of the write character routine.
    STA $020F ; Store it in $020F.
    LDA #<OSWRCHV ; Get the low byte of the write character routine.
    STA $020E ; Store it in $020E

    ; -- Print the boot message --

    LDY #<bootMessage ; Store the lower 4 bits of the boot message address into the Y register.
    LDA #>bootMessage ; Store the upper 4 bits of the address into the A register.
    STA $FE ; Store the high byte of the source address.
    STZ $FD ; Clear the low byte in memory.
printBootMessageLoop:
    LDA ($FD),Y ; Read the character at $FE-$FD, offset by the value of Y.
    JSR OSASCI ; Send the character to the ACIA to transmit out of the 'Tx' pin.
    INY ; Step to the next character.
    CMP #0 ; If we read a '0', that's when we stop reading the string.
    BNE printBootMessageLoop ;  If A is not 0, read the next character.

    ; -- Enter BBC BASIC --

    CLC ; Clear the carry bit to tell the BBC BASIC we are entering from RESET.
    LDA #$01 ; Load a '1' into the accumulator to tell BBC BASIC we are starting up.
    CLI ; Enable interrupts, now that we're done initialising all our memory and peripherals.
    JMP BASIC ; Enter BBC BASIC! 
    ; This is the end of the reset sequence.

; -- OS Call Routines --

; OSRDCH: 'OS Read Character'
; This subroutine waits for a character to arrive from the ACIA, then returns it in A. Cy=Esc pressed.
; We use this to receive input from your keyboard to the the caller.
; It also checks if the escape key has been pressed. If it has, it lets the caller know so it needs to leave whatever it's running.
OSRDCHV:
    BBR7 OSESC, readCharacterBuffer ; Is the escape flag set? If not, jump ahead to read the character.
    SEC ; If the escape flag IS set, set the carry bit and exit early without reading the character.
    RTS
readCharacterBuffer:
    LDA INPUTBUFFERWRITE ; Find difference between number of bytes written
    EOR INPUTBUFFERREAD ; Ends with A showing the number of bytes left to read.
    BEQ readCharacterBuffer
    LDA (INPUTBUFFERREAD)
    INC INPUTBUFFERREAD
    CLC ; Clear the carry bit. BBC BASIC uses the carry bit to track if we're in an 'escape condition' or not.
    RTS ; Return to the main routine.

; OSWRCH: 'OS Write Character'
; System call that displays whatever character is in A. This doesn't necessarily have to be an ASCII character.
; The 'V' in "OSWRCHV" means "Vector". When BBC BASIC jumps to the OSWRCH address, it jumps straight to here.
OSWRCHV:
    STA ACIA_DATA ; Send the character to the ACIA where it will immediately try to transmit it through 'Tx'.

    ; Because of the WDC 6551 ACIA transmit bug, We need around 86 microseconds between now and the end of RTS (assuming 115200 baud & 1mhz clock).
    JSR delay_100us
    ;JSR delay_100us ; Add one for each extra Mhz clock rate, in case you're running at 2+ Mhz.

    PHP ; Save caller's interupt state
    CLI ; Enable interrupts while we are printing a character.
    JSR print_char ; Also print the same character to the LCD.
    PLP ; Restore caller's interupt state.
    RTS


flushBuffer:
    LDX #0
clearBufferLoop:
    STZ INPUTBUFFER, X
    INX
    BNE clearBufferLoop 

    STZ INPUTBUFFERREAD
    STZ INPUTBUFFERWRITE

    RTS

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
    CMP #$7E ; Is it the 'acknowledge escape' system call?
    BEQ OSBYTE7E ; Jump to the 'acknowledge escape' routine.
    CMP #$84 ; Is it the 'read top of memory' system call?
    BEQ OSBYTE84 ; Put address '$4000' in YX registers.
    CMP #$83 ; Is it the 'read bottom of memory' system call?
    BEQ OSBYTE83 ; Put address '$0800' in YX registers.
    RTS ; Otherwise, return with nothing. 

OSBYTE7E: ; Routine that 'acknowledges' the escape key has been pressed.
    LDX #0 ; Reset X, in case X is currently set to #$FF aleady.
    BBR7 OSESC,clearEscape  ; if there's no ESCAPE flag, then just clear the ESCAPE condition.
    LDX #$FF   ; If escape HAS been pressed, set X=$FF to indicate ESCAPE has been acknowledged.
clearEscape:
    CLC    ; Clear the carry bit
    RMB7 OSESC ; Clear bit 7 of the ESCAPE flag.
    RTS 

OSBYTE84: ; Routine to return the highest address of free RAM space.
    ; Put address '$4000' in YX registers.  
    LDY #$40 ; High byte goes into Y
    LDX #$00  ; Low byte goes into X
    RTS

OSBYTE83: ; Routine to return the lowest address of free RAM space.
    ; Put address '$0900' in YX registers. 
    ; Anything below $0800 is memory space reserved by BBC MOS. $0900-09FF is reserved for the input buffer.
    LDY #$09 ; High byte goes into Y
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
	CLI	 ; Enable Interrupts
	
    ; Store A, X, and Y registers in MOS API workspace.
	STA	OSAREG
	STX	OSXREG			
	STY	OSYREG				

    CMP #$00        ; Is it the 'Read Line' system call?
    BEQ OSWORD0V    ; If yes, start reading input from the user.
    CMP #$01        ; Is it the 'Read Clock' system call?
    BEQ OSWORD1V_JUMP    ; Jump to it if yes
    CMP #$02        ; Is it the 'Write Clock' system call?
    BEQ OSWORD2V_JUMP    ; Jump to it if yes
    PLP             ; Restore caller's IRQs
    RTS             ; Otherwise, return with no change.

OSWORD1V_JUMP: JMP OSWORD1V ; OSWORD2V is too far away to directly jump, so we have to make a JMP here instead.
OSWORD2V_JUMP: JMP OSWORD2V ; OSWORD2V is too far away to directly jump, so we have to make a JMP here instead.

OSWORD0V:
    ; An OSWORD 0 control block has a couple of bytes of metadata to help us:
    ; byte 0: address of input buffer for result (low)
    ; byte 1: address of input buffer for result (high)
    ; byte 2: maximum line length
    ; byte 3: minimum acceptable ASCII code
    ; byte 4: maximum acceptable ASCII code
    STZ READBUFFER ; Clear the character buffer.
    LDY #4
osword0setup:
    ; Store max/min ASCII codes, and max line length from zero page memory to main memory
    LDA (OSXREG),Y
    STA $02B1,Y                 ; Copy bytes 2, 3, and 4 to memory for BBC BASIC to process.
    DEY
    CPY #1                      ; Loop until Y = 1.
    BNE osword0setup

    ; Store the input buffer addresses into a temporary buffer
    LDA (OSXREG),Y              ; Get value (high byte) from zero-page. Y is 1 right now.
    STA $E9                     ; Store into temporary buffer (high byte)
    DEY                         ; Set Y from 1 to 0.
    LDA (OSXREG),Y              ; Get value (low byte) from zero-page
    STA $E8                     ; Store into temporary buffer (low byte)

    CLI                         ; Explicitly enable interrupts to allow background keypress processing.
    JMP readInputCharacter      ; Jump ahead to process the next character.
readLineInputBufferFull:
    LDA #$07                     ; Send a 'bell character'
retryWithoutIncrement:
    DEY                         ; Decrement Y. We are essentially 'cancelling out' the next instruction.
retryWithIncrement:
    INY                         ; Decrement Y. Y is currently holding the current position in the input.
outputAndReadAgain:
    JSR OSWRCH                  ; Print the character. Fall through to 'readInputCharacter'
readInputCharacter:
    JSR OSRDCH                  ; Read the next character from ACIA
    BCS Escape                  ; If OSRDCH has set the carry bit, that means the escape key was pressed. Leave early.

    CMP #$08                    ; Is it a backspace? Let's delete the last character.
    BEQ delete
    CMP #$7F                    ; Or, is it a delete? Let's delete the last character.
    BEQ delete

    JMP convertToUppercase      ; Otherwise, move on

delete:
    CPY #0                      ; Are we at the first character?
    BEQ readInputCharacter      ; Then do nothing
    DEY                         ; Otherwise, go back 1.
    JMP outputAndReadAgain      ; Write the delete character

convertToUppercase: 
    CMP #'a'                    ; Compare with 'a'
    BCC continueRead                ; If less than 'a', it's not a lowercase letter
    CMP #'z'+1                  ; Compare with 'z'. Add 1 to include 'z' itself.
    BCS continueRead                ; If greater than 'z', it's not a lowercase letter
    AND #%11011111              ; In ASCII, you can clear the 5th bit to convert any lowercase to uppercase.
continueRead:
    STA ($E8),Y                 ; Store character into a buffer that BBC BASIC uses to process it.
    CMP #$0D                    ; Is it the newline character?
    BEQ newLineAndExit          ; ...then finish

    CPY $02B3                   ; check current length against max word length
    BCS readLineInputBufferFull ; send a bell character if full

    CMP $02B4                   ; check minimum ASCII character
    BCC retryWithoutIncrement   ; less than minimum? reject and retry

    CMP $02B5                   ; check maximum ASCII character
    BCS retryWithoutIncrement   ; If it's more than the maximum, reject and retry.
    JMP retryWithIncrement      ; Otherwise, accept and retry.

newLineAndExit:
    JSR OSNEWL
    LDA OSAREG
    LDX OSXREG
    LDY OSYREG
    PLP ; Restore flags
    CLC
    RTS
Escape:
    JSR flushBuffer
    PLP
    LDA OSESC                   ; Get escape flag
    ROL                         ; If the escape flag is set, also set the carry bit.
    PHP
    LDA OSAREG
    PLP
    CLI                         ; Re-enable interrupts
    RTS

; OSWORD 1: Read System Timer
; The variable TIME is a 5-byte variable starting at address 'TIME'.
; To read the timer, let's loop through the 5 bytes and store them in the control block
OSWORD1V:
    LDX #0 ; Use this to read the 5 bytes. This will run up from 0 to 4.                      
    LDY #4 ; Use this to write the 5 bytes. This will run down from 4 to 0.
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
    STA TIME,X
    INX
    DEY
    BPL writeTimerLoop
    PLP ; Restore caller's IRQ state
    RTS

; -- Keyboard Interrupt Routines --

keyboard_interrupt:
    PHA                         ; Save A
    PHX                         ; Save X
    BBR1 KEYBOARD_FLAGS, handle_pressed_key ; If 'release' flag is not set, skip ahead to read_key
handle_released_key:
    RMB1 KEYBOARD_FLAGS         ; If we ARE releasing a key, let's clear the release flag.
    LDA PORTA                   ; read PORTA to clear the interrupt
clear_left_shift:
    CMP #$12                    ; Left shift was pressed?
    BNE clear_right_shift       ; if not, skip ahead
    RMB2 KEYBOARD_FLAGS         ; otherwise, clear the shift flag.
clear_right_shift:
    CMP #$59                    ; Right shift was pressed?
    BNE keyboard_interrupt_exit ; if not, leave.
    RMB2 KEYBOARD_FLAGS         ; otherwise, clear the shift flag.
    JMP keyboard_interrupt_exit ; Finished processing all released keys. Exit.

handle_pressed_key:
    ; Process what's in PORTA, and store it into READBUFFER for reading later.
    LDA PORTA
    CMP #$F0                    ; If we've read $F0, that means the keyboard is signalling a key was released.
    BNE set_left_shift          ; If it's not a released key, skip ahead to shift checking
    SMB1 KEYBOARD_FLAGS         ; If it IS a released key, set the release bit in KEYBOARDS_FLAGS.
    JMP keyboard_interrupt_exit
set_left_shift:
    CMP #$12                    ; Left shift was pressed?
    BNE set_right_shift         ; if not, skip ahead
    SMB2 KEYBOARD_FLAGS         ; otherwise, set the shift flag.
    JMP keyboard_interrupt_exit
set_right_shift:
    CMP #$59                    ; Right shift was pressed?
    BNE not_shift               ; if not, skip ahead
    SMB2 KEYBOARD_FLAGS         ; otherwise, set the shift flag.
    JMP keyboard_interrupt_exit
    
not_shift:
    ; Convert the PS/2 scancode to an ASCII code.
    CMP #$7F
    BCS keyboard_interrupt_exit ; Is it outside the valid scancodes? Leave early.
    TAX                         ; Otherwise, transfer the scancode to X register.
    BBS2 KEYBOARD_FLAGS, shifted_key ; Is the shift flag set? Use the shifted keymap.
    LDA keymap,X                ; Use the 'keymap' to convert the scancode. Scancode is in X, which will convert to an ASCII stored in A.
    JMP push_key                ; Move ahead to store the ASCII for processing.
shifted_key:
    LDA keymap_shifted,X
    ; fall through...
push_key:
    ; Now that we have the ASCII character stored in A, let's store it in READBUFFER for processing later.
    STA READBUFFER              ; Store the ASCII into READBUFFER
    CMP #$1B                    ; Is the character an escape character?
    BNE keyboard_interrupt_exit ; If not, we are done.
    LDA #$FF                    ; If it IS the escape character, we need to signal that an escape state is active. 
    STA OSESC                   ; set the 'escape flag' address at $FF to the value #$FF.
keyboard_interrupt_exit:
    PLX                         ; Restore X
    PLA                         ; Restore A
    RTS                         ; Return back to the interrupt handler

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
    STA LCDWRITEBUFFER
    LDA #0
    JSR LCD_WRITE
    RTS ; Return to where we were before.

; Routine to keep the 6502 waiting until the LCD isn't busy anymore.
lcd_wait:
    PHA ; Save the original value of A.
lcdbusy:
    LDA #RW
    JSR lcd_read
    AND #%10000000 ; check busy flag
    BNE lcdbusy
    PLA ; Restore the original value of A
    RTS

lcd_read:
    PHA
    LDA #%11110000 ; We need to set the lower four bits of PORTB to 'input' to read the busy flag.
    STA DDRB
    PLA
    
    PHA
    STA PORTB
    LDA #E
    TSB PORTB

    LDA PORTB ; Read the high four bits. The first bit will have the busy flag.
    ASL
    ASL
    ASL
    ASL
    STA LCDREADBUFFER

    PLA
    PHA
    STA PORTB
    LDA #E
    TSB PORTB

    LDA PORTB
    AND #%00001111

    ORA LCDREADBUFFER
    STA LCDREADBUFFER

    PLA
    STA PORTB

    LDA #%11111111
    STA DDRB

    LDA LCDREADBUFFER
    
    RTS

LCD_WRITE:
    JSR lcd_wait

    PHA ; store the LCD flags twice
    PHA
    ; store the high and low nibs in zeropage
    LDA LCDWRITEBUFFER
    LSR LCDWRITEBUFFER 
    LSR LCDWRITEBUFFER
    LSR LCDWRITEBUFFER
    LSR LCDWRITEBUFFER ; high nib
    AND #$0F
    STA LCDWRITEBUFFER+1 ; low nib

    PLA ; get LCD flag
    ORA LCDWRITEBUFFER
    STA PORTB
    LDA #E
    TSB PORTB
    TRB PORTB

    PLA ; get LCD flag
    ORA LCDWRITEBUFFER+1
    STA PORTB
    LDA #E
    TSB PORTB
    TRB PORTB

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
    JSR lcd_wait ; Let's waste some time while the ACIA is still transmitting the character (see OSWRCH).
    JMP exit_lcd

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

lcd_print_enter:
    LDA #%11000000 ; Put cursor at the start of the second line (Position 40)
    JSR lcd_instruction
    LDX #0 ; Reset X
lcd_print_enter_read_line_loop:
    JSR lcd_wait ; Make sure the LCD isn't busy
    LDA #(RS | RW) ; send instruction to read the current cursor in memory.
    JSR lcd_read
    STA LCDBUFFER,X ; Store it into memory.
    INX
    CPX #$27
    BCC lcd_print_enter_read_line_loop ; Keep going while we haven't hit the end of line 2 (Position 40 + $27).
lcd_print_enter_clear:
    LDA #%00000001 ; Clear the display
    JSR lcd_instruction
    LDX #0 ; Reset X in preparation for the next loop.
lcd_print_enter_write_line_loop:
    LDA LCDBUFFER,X ; Load the next character from memory
    JSR print_char ; Print it to the LCD
    STZ LCDBUFFER,X ; Clear that space in memory, since we have already read it.
    INX
    CPX #$27 
    BCC lcd_print_enter_write_line_loop ; Keep going while we haven't hit the end of line 1 (Position 0 + $27).

    LDA #%11000000 ; Now that we're finished writing, let's set the cursor to the start of line 2.
    JSR lcd_instruction

    JMP exit_lcd ; We're done!

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
    JSR lcd_wait ; Make sure the LCD isn't busy with the previous instruction.
    LDA #RW ; Send the instruction to read the cursor counter.
    JSR lcd_read
    AND #%01111111 ; mask out the busy flag

    CMP #$4F ; 40 + 16 (-1 to cancel out the space we printed)
    BCC exit_lcd ; If the cursor is within the frame, no need to do anything.
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
    STA LCDWRITEBUFFER
    LDA #RS
    JSR LCD_WRITE

    ; If we've used up all of the main view (16 characters), we need to shift the display along so we can still read what we are doing.
    JSR lcd_wait ; Make sure the LCD isn't busy with the previous instruction.
    LDA #RW ; Send the instruction to read the cursor counter.
    JSR lcd_read
    AND #%01111111 ; mask out the busy flag

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

; -- LCD Delay routines ---

; Set A and Y such that microseconds = 9*(256*A+Y)+20. This is assuming a 1mhz clock.
delay_15ms:
    PHA
    PHY
    LDA #6
    LDY #129
    JMP delay_loop

delay_4100us:
    PHA
    PHY
    LDA #1
    LDY #198
    JMP delay_loop

delay_100us:
    PHA
    PHY
    LDA #0
    LDY #9
    JMP delay_loop

delay_loop:   
    CPY  #1
    DEY
    SBC  #0
    BCS  delay_loop
    PLY
    PLA
    RTS

; -- Interrupt Handling --

; Subroutine called after every NMI or IRQ in hardware, or the BRK instruction in software.
interrupt:
    STA OSINTA ; Save A for later.               
    PLA ; Get the status register. IRQ/BRK puts it on the stack.
    PHA ; Keep the status register on the stack for later.
    AND #$10 ; Check if it's a BRK or an IRQ.
    BNE BRKV ; If it's BRK, that's an error. Go to the BRK vector.
irqv: ; Otherwise, it's an IRQ. Let's check what caused the interrupt, starting with the ACIA.
    LDA ACIA_STATUS
    BIT #$08
    BEQ irq_via ; Skip ahead if bit 3 isn't set
irq_acia:
    LDA ACIA_DATA
    STA (INPUTBUFFERWRITE) ; Push to buffer
    INC INPUTBUFFERWRITE
    CMP #$1B
    BNE irq_via ; Was the escape character sent?
    SMB7 OSESC ; Set the escape flag.
irq_via:
    LDA IFR ; Check the "Interrupt Flag Register" to make sure it was the keyboard that caused the interrupt.
    AND #%00000010 ; We have to check bit 2.
    BNE irq_keyboard 
    BEQ irq_via_tick ; If we've ruled out the ACIA and Keyboard, let's assume it was the timer.
irq_keyboard: ; If we've ruled out the ACIA, then let's try the keyboard.
    JSR keyboard_interrupt ; Jump to the routine that reads PORTA and and prints the character.
    JMP end_irq ; Finish the interrupt.
irq_via_tick: ; If we've ruled out the ACIA & keyboard, then let's assume it was the VIA timer.
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
    LDA OSINTA ; Restore A
    RTI ; "ReTurn from Interrupt" Restore caller's flags, return to caller.


; -- BREAK Handler --

; Handler for interrupts that we know were called by the BRK instruction. This means an error was reported.
; The BBC MOS API defines the structure of an error message. To get the message, we need to store the location of the error message in addresses $FD and $FE. 
BRKV:
    PHX                 ; Save X
    TSX                 ; Get the stack pointer value
    LDA $0103,X         ; Get the low byte of the error message location, offset by the stack pointer.
    SEC                    
    SBC #1              ; Decrement 1. Use "SBC #1" instead of "DEC", because 'DEC' does not set the carry bit.
    STA OSFAULT         ; Store the low byte into the fault handler.
    LDA $0104,X         ; Get the high byte of the error message location.
    SBC #0              ; Did subtracting 1 from the low byte cause the carry bit to set? Subtract 1 from the high byte too.
    STA OSFAULT+1       ; Store the high byte into the fault handler.
    STX OSXREG          ; Store the location of the last break for the error handler.
    PLX                 ; Restore X
    LDA OSINTA
    CLI
    JMP ($0202)         ; Jump to BBC BASIC's error handler routine, which takes it from there. Address $0202 points to the routine.


    ; Define the mapping from PS/2 Scancode to ASCII
   .org $fe00
keymap:
    .byte "????????????? `?" ; 00-0F
    .byte "?????q1???zsaw2?" ; 10-1F
    .byte "?cxde43?? vftr5?" ; 20-2F
    .byte "?nbhgy6???mju78?" ; 30-3F
    .byte "?,kio09??./l;p-?" ; 40-4F
    .byte "??'?[=????",$0d,"]?\??" ; 50-5F
    .byte "??????",$08,"??1?47???" ; 60-6F
    .byte "0.2568",$1b,"??+3-*9??" ; 70-7F
keymap_shifted:
    .byte "????????????? ~?" ; 00-0F
    .byte "?????Q!???ZSAW@?" ; 10-1F
    .byte "?CXDE#$?? VFTR%?" ; 20-2F
    .byte "?NBHGY^???MJU&*?" ; 30-3F
    .byte "?<KIO)(??>?L:P_?" ; 40-4F
    .byte '??"?{+?????}?|??' ; 50-5F
    .byte "?????????1?47???" ; 60-6F
    .byte "0.2568???+3-*9??" ; 70-7F


    ; BBC MOS system calls. Code call these by jumping to their place in memory.
    ; Most of them jump to a 'vector' that properly handles the system call.
    
    .org $FFB9
    ; Fill the unused system calls from $FFB9 to $FFDD with the 'RTS' instruction, so we can safely return in case they are called.
    ; If you want to use BBC BASIC system calls that use these addresses, you'll have to break this '.fill' apart to make space for it.
    .fill 39, $60

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

    .byte $60,$60,$60 ; 'OSCLI' is unused, so we'll write 'RTS' to it.

    ; 6502-specific calls, such as interrupts and resets.
    .org NMI
    .word interrupt ; at NMI, go to interrupt handler
    .word reset ; at RESET address, go to reset label
    .word interrupt ; When IRQ goes low or BRK is called, go to the interrupt handler
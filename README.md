# BeebEater

![Screenshot of BeebEater](/BeebEater_Screenshot.jpeg)

BeebEater is a port of BBC BASIC for the 6502. It’s fully compatible with Ben Eater's 6502 breadboard computer build.

I started this project because of the lack of easy, portable, and fully featured operating systems for beginners to install on a 6502 homebrew computer.

BBC BASIC is the BASIC interpreter found in the [BBC Micro Computer](https://en.wikipedia.org/wiki/BBC_Micro) from the 80s. It's considered by many to be the best BASIC interpreter ever made for the 6502!

_**BeebEater is designed for beginners. My goal for BeebEater is to be the go-to recommendation for those who ask "What OS should I install on my 6502?"**_

No extra modifications are required. All you need to do is load `BeebEater.rom` into EEPROM, and then reset.

_You can also try out BeebEater in an emulator without the need for hardware! See [Emulator](#emulator) for details._

> [!WARNING]  
> Consider BeebEater still in BETA until version v1.0 is released. It's fully functional, but needs minor fixes and performance boosts.
> Let me know of any issues you find by submitting a new issue in the "Issues" tab on this GitHub repo. If your issue is not already there, I don't know it exists yet. Thanks!

# Features

**BeebEater features:**
 * **Small code base** - Less than 1000 lines of assembly!
 * **Heavily annotated** - Line by line comments to help you understand what's happening as much as possible.
 * **Extensible** - Designed to be easy for you to extend with your own features or custom configurations.

**BBC BASIC features:**
 * **Modern programming concepts** - BBC BASIC uses modern programming concepts such as `IF`, `FOR` and even functions with `DEF PROC`.
 * **Built-in assembler** - Supports executing assembly code. **That's right, you can program your 6502 USING your 6502!**
 * **It's fast** - BBC BASIC is famous for being one of the best perfoming BASIC interpreters for the 6502.
 * **Native LCD and PS/2 Keyboard support** - LCD with a PS/2 Keyboard turns your 6502 build into a fully independent computer!

## Supported features
 * **Serial Terminal IO** - Interact with BBC BASIC via the serial terminal. 
 * **Keyboard** - Support for a PS/2 Keyboard connected to PORTA on the VIA.
 * **LCD** - Support for a 16x2 LCD connected to PORTB on the VIA (4-bit mode).
 * **Backspace/Delete** on current input.
 * **Escape key** for leaving those happy little mistakes.
 * (CoolTerm Only) **Clear the serial terminal screen** using `CLS`.

## Unsupported features
 * **'Star commands' such as `*EDIT` and `*RUN`.** These are BBC Micro-specific commands, and aren't handled inside BASIC.
 * **Commands that require graphics, such as `PLOT`.** Serial terminals do not support anything beyond ASCII characters.
 * **Commands that require sound, such as `SOUND`.** Sound requires a sound chip, which hasn't been covered yet by Ben Eater.
 * **Any ROM-based software written for the BBC Micro/Master.**

# Requirements
## Hardware
BeebEater assumes you have the [standard Ben Eater 6502 build](https://eater.net/6502), with the standard memory mapping:
 * 65C02 Microprocessor (with a 1Mhz clock)
 * 32k ROM at $8000 to $FFFF
 * 16k RAM at $0000 to $3FFF
 * 6551 ACIA at $5000-5003 (115200 Baud, with a 1.8432 Mhz external crystal)	
 * 6522 VIA at $6000-600F
   	* [PS/2 Keyboard](https://www.youtube.com/watch?v=w1SB9Ry8_Jg) on PORTA (Pins 2-9)
   	* LCD (4-bit mode) on PORTB (Pins 10-16)

![Ben Eater 6502 Schematic Diagram](https://eater.net/schematics/6502-serial.png)

### Required Additional Hardware Adjustments

BeebEater follows the same schematic as the Ben Eater one shown above, but with some minor additions: 
 1. Connect the PS/2 keyboard to the PA0-PA7 pins (Pins 2-9) on the VIA, as per [Ben's keyboard video.](https://www.youtube.com/watch?v=w1SB9Ry8_Jg)
 2. Connect the PS/2 keyboard's rising edge interrrupt signal to the CA1 pin on the VIA, as per [Ben's keyboard video.](https://www.youtube.com/watch?v=w1SB9Ry8_Jg)
 3. On the VIA, connect the unused pin 17 (PB7) to ground. Otherwise, the pin would be in a 'floating' state, and would corrupt data between the VIA and the LCD.
 4. On the ACIA, tie Pin 16 (DCDB) and 17 (DSRB) to ground. Otherwise, these pins would be 'floating', and cause IRQ triggers when it's not needed.
 5. **VERY IMPORTANT:** Don't have the LCD and/or Keyboard connected? Send any unused/unconnected PA and PB pins from the VIA (Pins 2-17 on the W65C22) directly to ground. Otherwise, BeebEater will get confused about the 'floating' state pins and won't run.

## Serial monitor
I recommend [**CoolTerm**](https://freeware.the-meiers.org) as your serial monitor application. It’s free, open source, cross-platform, and natively handles backspace/delete. Open `BeebEater_CoolTerm.cts` inside CoolTerm, you’ll have all the settings you need preloaded.

If you’re using some other non-CoolTerm serial monitor, you’ll likely need to make some changes to the settings:
 * 115200 baud rate, 8 data bits, no parity, 1 stop bit.
 * Enable handling of backspace/delete characters. Otherwise characters will not delete.
 * Interpret the Enter/Return key as ‘CR’, not ‘CR+LF’. Otherwise your cursor will get misaligned.
 * Ignore the ‘Line Feed’ (LF) character. Otherwise, lines will have twice as much spacing.
  * (Optional) Set the serial monitor to clear the screen when ‘0xFF’ is received. This means the `CLS` command will clear the screen.

# Installation

If you have a standard Ben Eater 6502 memory mapping, everything is done for you. Just load the ROM, and go!
 1. Download the latest `BeebEater.rom` from the ‘Releases’ section in this GitHub page.
 2. Load `BeebEater.rom` into the start address of your EEPROM. At 32KB, this should take up the complete EEPROM.
 3. Reset and run! 

If you want to make modifications to the code, you’ll need to assemble a new ROM using VASM. Use the following VASM flags to generate a new ROM:
	
`/PATH/TO/vasm/vasm6502_oldstyle -Fbin -dotdir ./BeebEater.asm -c02 -o ./BeebEaterNew.rom`

# Emulator

Don't have the hardware yet? Want to just try it out? You can use the [Symon 6502 Simulator](https://github.com/sethm/symon/tree/master) to try the original 6502 BBC BASIC in software. No hardware needed!

Download Symon 1.4.0 here: https://github.com/sethm/symon/releases/tag/v1.4.0.

Then, place BeebEater.rom in the same folder as Symon, then run Symon with the following:
`java -jar symon-1.4.0.jar -rom BeebEater.rom -cpu 65c02 -machine benEater`

# Special Thanks to…
 * **Ben Eater** - For inspiring me to get into computer hardware as a hobby!
 * **Sophie Wilson** - For single-handedly creating BBC BASIC, and for being such an inspiration to me!
 * **J.G Harston** from mdfs.net - For supplying the original BBC BASIC ROMs with annotated disassemblies.
 * **Toby Nelson** (tobyLobster) - For a [highly-annotated disassembly of Acorn MOS 1.20](https://tobylobster.github.io/mos/index.html)!

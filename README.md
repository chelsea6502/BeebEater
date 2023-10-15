# BeebEater

![Screenshot of BeebEater](/BeebEater_Screenshot.jpeg)

BeebEater is a port of BBC BASIC for the 6502. It’s fully compatible with Ben Eater's 6502 breadboard computer build.

I started this project because of the lack of easy, compatible, and fully featured operating systems for beginners to install on a 6502 homebrew computer.

BBC BASIC is the BASIC interpreter found in the [BBC Micro Computer](https://en.wikipedia.org/wiki/BBC_Micro) from the 80s. It's considered by many to be the best BASIC interpreter ever made for the 6502!

_**BeebEater is designed for beginners. My goal for BeebEater is to be the go-to recommendation for those who ask "What OS should I install on my 6502?"**_

No extra modifications are required. All you need to do is load `BeebEater.rom` into EEPROM, and connect to serial!

# Features

**BeebEater features:**
 * **Small code base** - Less than 500 lines of assembly!
 * **Highly annotated** - Line by line comments to help you understand what's happening as much as possible.
 * **Extensible** - Designed to be easy for you to extend with your own features or custom configurations.

**BBC BASIC features:**
 * **Modern programming concepts** - BBC BASIC uses modern programming concepts such as `IF`, `WHILE`, `FOR` and even functions with `DEF PROC`.
 * **Built-in assembler** - Supports executing assembly code. **That's right, you can program your 6502 USING your 6502!**
 * **It's fast** - BBC BASIC is famous for being one of the best perfoming BASIC interpreters for the 6502.
 * **Native video and sound commands** - While these aren't implemented by default, you have native graphics and sounds commands at your disposal to build upon.

## What works
 * **Output** from the ACIA to the serial terminal.
 * **Input** from the serial terminal to the ACIA.
 * **Error handling** from BBC BASIC
 * **Backspace/Delete** on current input.
 * **Escape key** when inputting a command
 * **The standard boot message** for the BBC Micro (hard-coded for now).
 * (CoolTerm Only) **Clear the serial terminal screen** using `CLS`.

## What doesn't work
 * **'Star commands' such as `*EDIT` and `*RUN`.** These are BBC Micro-specific commands, and aren't handled inside BASIC.
 * **Commands that require graphics, such as `PLOT`.** Serial terminals do not support anything beyond ASCII characters.
 * **Commands that require sound, such as `SOUND`.** Sound requires a sound chip, which hasn't been covered yet by Ben Eater.
 * **Escape key while BASIC is outputting.** If you get stuck in a loop, you'll have to hardware reset.

# Requirements
## Hardware
BeebEater assumes you have the [standard Ben Eater 6502 build](https://eater.net/6502), with the standard memory mapping:
 * 65C02 MPU (with a 1Mhz clock)
 * 32k ROM at $8000 to $FFFF
 * 16k RAM at $0000 to $3FFF
 * 6551 ACIA at $5000-5003 (with a 1.8432 Mhz external crystal)

**The 6522 VIA is optional.** It’s not used anywhere in this code.

**Don’t have RS-232?** You can connect the Rx and Tx pins to a [Serial to USB converter](https://www.jaycar.com.au/duinotech-arduino-compatible-usb-to-serial-adaptor/p/XC4464) like I do.

## Serial monitor
I recommend [**CoolTerm**](https://freeware.the-meiers.org) as your serial monitor application. It’s free, open source, cross-platform, and natively handles backspace/delete. Open `BeebEater_CoolTerm.cts` inside CoolTerm, you’ll have all the settings you need preloaded.

If you’d using some other non-CoolTerm serial monitor, you’ll need to make some changes to the settings:
 * 115200 baud rate, 8 data bits, no parity, 1 stop bit.
 * Enable handling of backspace/delete characters. Otherwise characters will not delete.
 * Interpret the Enter/Return key as ‘CR’, not ‘CR+LF’. Otherwise your cursor will get misaligned.
 * Ignore the ‘Line Feed’ (LF) character. Otherwise, lines will have twice as much spacing.
  * (Optional) Set the serial monitor to clear the screen when ‘0xFF’ is received. This means the `CLS` command will clear the screen.

# Installation

If you have a standard Ben Eater 6502 memory mapping, everything is done for you. Just load the ROM, and go!
 1. Download the latest `BeebEater.rom` from the ‘Releases’ section in this GitHub page.
 2. Load `BeebEater.rom` into the start address of your EEPROM. At 32KB, this should take up the complete EEPROM.
 3. Connect to a serial monitor, reset and run! 

If you want to make modifications to the code, you’ll need to assemble a new ROM using VASM. Use the following VASM flags to generate a new ROM:
	
`/PATH/TO/vasm/vasm6502_oldstyle -Fbin -dotdir ./BeebEater.asm -c02 -o ./BeebEaterNew.rom`

# Future plans

Some rough plans for future versions:
* Escape key support. Currently if you get stuck in a loop, you'll have to hardware reset.
* Upgrade from BBC BASIC 4r32 to BBC BASIC V. BBC BASIC V has lots of improvements, such as CASE and WHILE commands!
* PS/2 Keyboard support.
* LCD support
* SAVE/LOAD programs by sending/recieving raw data though serial.


# Special Thanks to…
 * **Ben Eater** - For inspiring me to get into computer hardware as a hobby!
 * **Sophie Wilson** - For single-handedly creating BBC BASIC, and for being such an inspiration to me!
 * **J.G Harston** from mdfs.net - For supplying the original BBC BASIC ROMs with annotated disassemblies.
 * **Toby Nelson** (tobyLobster) - For a [highly-annotated disassembly of Acorn MOS 1.20](https://tobylobster.github.io/mos/index.html)!

# BeebEater
BeebEater is a port of BBC BASIC for the 6502. It’s fully compatible with the 6502 build from Ben Eater.

# Requirements
BeebEater assumes you have the standard Ben Eater 6502 build, with the standard memory mapping:
 * 65C02 MPU with a 1Mhz clock
 * 32k ROM at $8000 to $FFFF
 * 16k RAM at $0000 to $3FFF
 * 6551 ACIA at $5000-5003, with a 1.8432 Mhz external crystal.

The 6522 VIA is optional. It’s not used anywhere in this code.

Don’t have RS-232? You can connect the Rx and Tx pins to a Serial to USB converter like I do.

# Serial monitor
I recommend CoolTerm as your serial monitor application. It’s free, open source, cross-platform, and natively handles backspace/delete. 

Open ‘BeebEater_CoolTerm.cts’ inside CoolTerm, you’ll have all the settings you need preloaded!
		
If you’d using some other non-CoolTerm serial monitor, you’ll need to make some changes to the settings:
 * 115200 baud rate, 8 data bits, no parity, 1 stop bit.
 * Enable handling of backspace/delete characters. Otherwise characters will not delete!
 * Interpret the Enter/Return key as ‘CR’, not ‘CR+LF’. Otherwise your cursor will get misaligned.
 * Ignore the ‘Line Feed’ (LF) character. Otherwise, lines will have twice as much spacing.
  * (Optional) Set the serial monitor to clear the screen when ‘0xFF’ is received. This means the `CLS` command will clear the screen!

# Installation
# The quick way
If you have a standard Ben Eater 6502 memory mapping, everything is done for you. Just load the ROM, and go!
 1. Download the latest `BeebEater.rom` from the ‘Releases’ section in this GitHub page.
 2. Load `BeebEater.rom` into the start address of your EEPROM. At 32KB, this should take up the complete EEPROM.
 3. Connect to a serial monitor, reset and run! 

# The not-so-quick way
If you want to make modifications to the code, you’ll need to compile it using VASM. Use the following VASM flags to generate a new ROM:
	
`/PATH/TO/vasm/vasm6502_oldstyle -Fbin -dotdir ./BeebEater.asm -c02 -o ./BeebEaterNew.rom`

# Special Thanks to…
 * **Ben Eater** - For inspiring me to get into computer hardware as a hobby!
 * **Sophie Wilson** - For single-handedly creating BBC BASIC, and for being such an inspiration to me!
 * **J.G Harston** from mdfs.net - For supplying original BBC BASIC ROMs with annotated disassemblies
 * **Toby Nelson** (tobyLobster) - For a highly-annotated disassembly of MOS 1.20!

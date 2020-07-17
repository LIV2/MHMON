# MHMON
Monitor ROM for my 65C02 system

### Floppy/FAT12 Support
Supports reading FAT12 floppies and running programs from them  
There is no support for writing or directories (yet)  

### Functions (subject to change)

|Name|Address|Arguments|Returns|Notes|
|----|-------|---------|-------|-----|
|CHARIN|E000|none|A: Character, Carry set if character available|Non-blocking, if Carry is clear no char was returned|
|CHAROUT|E003|A: Character to send|||
|CRLF|E006|||Print Carriage return & Linefeed|
|RESERVED|E009||||
|RESERVED|E00C||||
|CLS|E00F|||CLear Screen|
|HEXTOASC|E012|A: Hex number to print|||
|CHARINW|EO15||A: Character|Waits for a character before returning|
|HEXOUT|E018|A: Hex nibble to print||Print hex nibble|
|CHARINU|E01B||A: Character|Returns input character converted to uppercase|
|FOPEN|E01E|||TODO: document|
|FCLOSE|E021|||TODO: document|
|FREAD|E024|||TODO: document|
|ERRMSG|E027|||TODO: document|

### Commands
#### All arguments (Except filename) must be entered as hex digits
|Command|Arguments|Description|Example|
|-------|---------|-----------|-------|
|B||Print colour bars|B|
|C||Clear Screen|C|
|D|\<start address\> [end address]|Display memory addresses|D 0100|
|E|\<pathspec\>|Execute program from disk|E A:EHBASIC.COM|
|F|\<start\> \<end\> \<value\>|Fill memory with a specific value|F 1000 2000 FF|
|J|\<addr\>|Jump to program|J 1000|
|L||Load from SREC|L <paste SRECORD file>|
|M||Memory test|M|
|R|\<addr\>|Read memory location|R 0100|
|S|\<seconds\>|Sleep|S 0A|
|W|\<addr\> \<value\> [\<value\> ...]|Write memory location(s)|W 0100 AA|
|?||Display help|?|


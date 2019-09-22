PROGNAME = mhmon
# Final output filename
BINFILE = $(PROGNAME).bin
CFGFILE = liv2.cfg

obj/%.o: src/%.asm src/*.inc
	ca65 $< --cpu 65c02 -o $@ -I src

all:	obj/$(PROGNAME).o
	ld65 -o bin/$(BINFILE) -Ln label -v -vm -m obj/map -C $(CFGFILE) $^ 

clean:
	- mkdir -p obj
	- mkdir -p bin
	- rm -f obj/*
	- rm -f bin/*
install: all
	- (cat loader.txt ; srec_cat bin/$(BINGILE) -Binary -offset 24576 -output -Execution_Start_Address 0x5000 -HEader "ROM") | pbcopy

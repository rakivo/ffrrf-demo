LIBS = -lraylib -lc -lm
LIBS_PATHS = /lib64/ld-linux-x86-64.so.2

all: main.o main

main.o: main.asm
	fasm $<

main: main.o
	ld -o $@ $< -dynamic-linker $(LIBS_PATHS) $(LIBS)

clean:
	rm -f main main.o

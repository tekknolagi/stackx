all: memory.o machine.o um.o seq.o
	gcc -o um memory.o machine.o um.o seq.o

memory.o: memory.c
	gcc -c memory.c

%.o: %.c
	gcc -c $<

clean:
	rm -f *.o um

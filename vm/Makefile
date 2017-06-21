all: memory.o machine.o um.o seq.o
	gcc -g3 -O3 -DNDEBUG -o um memory.o machine.o um.o seq.o

%.o: %.c
	gcc -g3 -O3 -DNDEBUG -c $<

clean:
	rm -f *.o um

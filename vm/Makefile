all: seq.o memory.o vm.o
	gcc -Wall -g3 -O3 -DNDEBUG -o vm seq.o memory.o vm.o

%.o: %.c
	gcc -Wall -g3 -O3 -DNDEBUG -c $<

clean:
	rm -f *.o vm

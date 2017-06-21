all: memory.o vm.o seq.o
	gcc -g3 -O3 -DNDEBUG -o vm memory.o vm.o seq.o

%.o: %.c
	gcc -g3 -O3 -DNDEBUG -c $<

clean:
	rm -f *.o vm

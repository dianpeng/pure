all: libpure

pure.o: pure.h pure.c
	gcc -c -O2 pure.c
	
libpure: pure.o
	ar rcs libpure.a pure.o
	
clean:
	rm -f *.o *a

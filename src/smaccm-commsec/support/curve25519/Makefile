CFLAGS=-Wmissing-prototypes -Wdeclaration-after-statement -O2 -Wall
CFLAGS_32=-m32

targets: curve25519-donna.a

test: test-donna

clean:
	rm -f *.o *.a *.pp test-curve25519-donna test-curve25519-donna-c64 speed-curve25519-donna speed-curve25519-donna-c64 test-noncanon-curve25519-donna test-noncanon-curve25519-donna-c64

curve25519-donna.a: curve25519-donna.o
	ar -rc curve25519-donna.a curve25519-donna.o
	ranlib curve25519-donna.a

curve25519-donna.o: curve25519-donna.c
	gcc -c curve25519-donna.c $(CFLAGS) $(CFLAGS_32)

test-donna: test-curve25519-donna
	./test-curve25519-donna | head -123456 | tail -1

test-curve25519-donna: test-curve25519.c curve25519-donna.a
	gcc -o test-curve25519-donna test-curve25519.c curve25519-donna.a $(CFLAGS) $(CFLAGS_32)

speed-curve25519-donna: speed-curve25519.c curve25519-donna.a
	gcc -o speed-curve25519-donna speed-curve25519.c curve25519-donna.a $(CFLAGS) $(CFLAGS_32)

test-noncanon-donna: test-noncanon-curve25519-donna
	./test-noncanon-curve25519-donna

test-noncanon-curve25519-donna: test-noncanon.c curve25519-donna.a
	gcc -o test-noncanon-curve25519-donna test-noncanon.c curve25519-donna.a $(CFLAGS) $(CFLAGS_32)

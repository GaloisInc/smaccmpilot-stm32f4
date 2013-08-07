CFLAGS = -g

all: interopTest.chs *.c *.h commsec.a
	c2hs interopTest.chs
	ghc -O2 interopTest.hs -o interopTest commsec.a

commsec.a: *.c *.h
	gcc $(CFLAGS) -c aescrypt.c
	gcc $(CFLAGS) -c aeskey.c
	gcc $(CFLAGS) -c aestab.c
	gcc $(CFLAGS) -c commsec.c
	gcc $(CFLAGS) -c gcm.c
	gcc $(CFLAGS) -c gf128mul.c
	gcc $(CFLAGS) -c gf_convert.c
	ar cr commsec.a aescrypt.o aeskey.o aestab.o commsec.o gcm.o gf128mul.o gf_convert.o 

example: example.c commsec.a
	gcc $(CFLAGS) example.c -o example commsec.a

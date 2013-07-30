CFLAGS = -g

all: interopTest.hs *.c *.h
	gcc $(CFLAGS) -c aescrypt.c
	gcc $(CFLAGS) -c aeskey.c
	gcc $(CFLAGS) -c aestab.c
	gcc $(CFLAGS) -c commsec.c
	gcc $(CFLAGS) -c gcm.c
	gcc $(CFLAGS) -c gf128mul.c
	gcc $(CFLAGS) -c gf_convert.c
#	gcc -c interopTest_hsc_make.c
#	gcc -c test-main.c
	ar cr commsec.a aescrypt.o aeskey.o aestab.o commsec.o gcm.o gf128mul.o gf_convert.o 
	c2hs interopTest.chs
	ghc -O2 interopTest.hs -o interopTest commsec.a

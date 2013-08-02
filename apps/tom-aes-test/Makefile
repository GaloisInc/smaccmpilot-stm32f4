CFLAGS        := -g
CABAL         := cabal-dev
CABAL_INSTALL := $(CABAL) install

all: interopTest

interopTest: interopTest.hs commsec.a *.c *.h
	$(CABAL) install cereal
	$(CABAL) install cipher-aes
	ghc -O2 interopTest.hs -o interopTest commsec.a

%.hs: %.chs
	c2hs $<

%.o: %.c
	gcc $(CFLAGS) -c $<

commsec.a:
	ar cr commsec.a aescrypt.o aeskey.o aestab.o commsec.o gcm.o gf128mul.o gf_convert.o

.PHONY: clean
clean:
	@rm -rf cabal-dev
	@rm interopTest.hs
	@rm interopTest

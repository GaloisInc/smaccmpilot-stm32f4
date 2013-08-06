# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-

CFLAGS        := -g
CABAL         := cabal
CABAL_INSTALL := $(CABAL) install

all: interopTest

interopTest: interopTest.hs commsec.a *.c *.h
	$(CABAL_INSTALL) cereal
	$(CABAL_INSTALL) cipher-aes
	ghc -O2 interopTest.hs -o interopTest commsec.a

%.hs: %.chs
	c2hs $<

%.o: %.c
	gcc $(CFLAGS) -c $<

commsec.a:
	ar cr commsec.a\
     aescrypt.o aeskey.o aestab.o commsec.o gcm.o gf128mul.o gf_convert.o

.PHONY: clean
clean:
	@rm -rf cabal-dev
	@rm -f interopTest.hs
	@rm -f interopTest

# vim: set ft=make noet ts=2:

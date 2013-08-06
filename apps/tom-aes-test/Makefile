# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-

CFLAGS        := -g
CABAL         := cabal
CABAL_INSTALL := $(CABAL) install
CLIB          := aeslib

all: interopTest

interopTest: interopTest.hs $(CLIB)/commsec.a $(CLIB)/*.c $(CLIB)/*.h
	$(CABAL_INSTALL) cereal
	$(CABAL_INSTALL) cipher-aes
	ghc -O2 interopTest.hs -o interopTest commsec.a

%.hs: %.chs
	c2hs $<

%.o: $(CLIB)/%.c
	gcc $(CFLAGS) -c $<

$(CLIB)/commsec.a: $(CLIB)/aescrypt.o $(CLIB)/aescrypt.o $(CLIB)/aeskey.o \
    $(CLIB)/aestab.o $(CLIB)/commsec.o $(CLIB)/gcm.o $(CLIB)/gf128mul.o \
    $(CLIB)/gf_convert.o
	ar cr $@ $?

.PHONY: clean
clean:
  @rm -f $(CLIB)/*.o
	@rm -rf cabal-dev
	@rm -f interopTest.hs
	@rm -f interopTest

# vim: set ft=make noet ts=2:

# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-

CFLAGS        := -g -Wall
CABAL         := cabal
CABAL_INSTALL := $(CABAL) install
CLIB          := aeslib

example: example.c $(CLIB)/commsec.a
	gcc $(CFLAGS) example.c -o example $(CLIB)/commsec.a

interopTest: interopTest.hs $(CLIB)/commsec.a $(CLIB)/*.c $(CLIB)/*.h
	$(CABAL_INSTALL) cereal
	$(CABAL_INSTALL) cipher-aes
	ghc -O2 interopTest.hs -o interopTest $(CLIB)/commsec.a

%.hs: %.chs
	c2hs $<

%.o: $(CLIB)/%.c
	gcc $(CFLAGS) -c $<

$(CLIB)/commsec.a: $(CLIB)/aescrypt.o $(CLIB)/aescrypt.o $(CLIB)/aeskey.o \
    $(CLIB)/aestab.o $(CLIB)/commsec.o $(CLIB)/gcm.o $(CLIB)/gf128mul.o \
    $(CLIB)/gf_convert.o
	ar cr $@ $?

# ------------------------------------------------------------------------------

GEN := ivory-gen
HS  := Main

%.c %h:
	../../../cabal-dev/bin/commsec-test

ivory-example: ivory-example.c
ivory-example: $(GEN)/IvoryGCM.c
ivory-example: $(GEN)/IvoryGCM.h
ivory-example: $(GEN)/ivory.h
ivory-example: ivory-commsec-shim.h
ivory-example: $(CLIB)/commsec.a
ivory-example:
	gcc -std=c99 -Wall -o $@ -I. -Iivory-gen -Iaeslib \
          ivory-example.c \
          $(GEN)/IvoryGCM.c \
          $(CLIB)/commsec.a

.PHONY: clean
clean:
	@rm -f $(CLIB)/*.o
	@rm -rf cabal-dev
	@rm -f interopTest.hs
	@rm -f interopTest.chs.h
	@rm -f interopTest.hi
	@rm -f interopTest.o
	@rm -f interopTest.chi
	@rm -f interopTest
	@rm -f example
	@rm -f ivory-example
	@rm -rf ivory-gen

# vim: set ft=make noet ts=2:

# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-

IVORY_REPO ?= ../../../../ivory

CFLAGS        := -g -Wall
CABAL         := cabal
CABAL_INSTALL := $(CABAL) install
CLIB          := aeslib

test: example ivory-example
	@echo "Running C Example:"
	./example
	@echo "Running Ivory Example:"
	./ivory-example

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

.PHONY: create-sandbox
create-sandbox:
	cabal sandbox init
	cabal sandbox add-source $(IVORY_REPO)/ivory
	cabal sandbox add-source $(IVORY_REPO)/ivory-artifact
	cabal sandbox add-source $(IVORY_REPO)/ivory-stdlib
	cabal sandbox add-source $(IVORY_REPO)/ivory-opts
	cabal sandbox add-source $(IVORY_REPO)/ivory-backend-c
	cabal install --dependencies-only

.PHONY: run-commsec-test-gen
run-commsec-test-gen:
	cabal run commsec-test-gen -- --src-dir=$(GEN)

ivory-example: run-commsec-test-gen
ivory-example: ivory-example.c
ivory-example: ivory-commsec-shim.h
ivory-example: $(CLIB)/commsec.a
ivory-example:
	gcc -std=c99 -Wall -o $@ -I. -I$(GEN) -Iaeslib \
          -DIVORY_TEST \
          ivory-example.c \
          $(GEN)/IvoryGCM.c \
          $(CLIB)/commsec.a

.PHONY: clean
clean:
	@rm -f $(CLIB)/*.o
	@rm -rf dist
	@rm -f interopTest.hs
	@rm -f interopTest.chs.h
	@rm -f interopTest.hi
	@rm -f interopTest.o
	@rm -f interopTest.chi
	@rm -f interopTest
	@rm -f example
	@rm -f ivory-example
	@rm -rf ivory-gen

.PHONY: clean-sandbox
clean-sandbox: clean
	@rm -rf .cabal-sandbox
# vim: set ft=make noet ts=2:

GENERATEDDIR=flight-generated

# Relative to current dir
SANDBOX=../dsl/cabal-dev

SRCDIR=$(GENERATEDDIR)/src
INCDIR=$(GENERATEDDIR)/include/smaccmpilot

GEN=smaccmpilot-gen
EXEC=ivory/dist/build/$(GEN)/$(GEN)

IVORY_OPTS=--const-fold --overflow --div-zero
# A little too noisy: --fp-check

GENERATED_DEP=$(GENERATEDDIR)/dep.mk

include $(GENERATED_DEP)

# ------------------------------------------------------------------------------

# Generate the srcs and headers.
IVORY += ivory-build
.PHONY: ivory-build
ivory-build: $(EXEC) $(FLIGHT_GENERATED_HEADERS) $(FLIGHT_GENERATED_SOURCES)

$(GENERATED_DEP): 
	$(SANDBOX)/bin/$(GEN) --src-dir=$(SRCDIR) --include-dir=$(INCDIR) \
		--deps=$(GENERATED_DEP) --dep-prefix=FLIGHT_GENERATED

# We don't add the .hs files as dependencies.  It's up to the user to clean
# them.  (It's not even clear if that's the right thing to do, since even if
# they don't change, if the compiler changes, you should rebuild.)
$(FLIGHT_GENERATED_HEADERS) $(FLIGHT_GENERATED_SOURCES): $(GENERATED_DEP)
	$(SANDBOX)/bin/$(GEN) --src-dir=$(SRCDIR) --include-dir=$(INCDIR) \
		$(IVORY_OPTS)

# Build the binary to generate the code.
.PRECIOUS: $(EXEC)
$(EXEC):
	cabal-dev -s $(SANDBOX) install --builddir=$(SMACCMDIR)/ivory \
		$(SMACCMDIR)/ivory

CLEAN     += $(GENERATED_DEP)
# use wildcard, not the dep file, to clean subdirs, because if dep file
# doesn't exist we won't get a proper clean.
CLEAN     += $(wildcard $(SRCDIR)/*.c)
CLEAN     += $(wildcard $(INCDIR)/*.h)
VERYCLEAN += $(SMACCMDIR)/ivory/dist


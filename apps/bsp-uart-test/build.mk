
BSPUARTTEST_GENERATEDDIR=apps/bsp-uart-test/generated
BSPUARTTEST_GRAPHS_DIR=apps/bsp-uart-test/graphs

# Relative to current dir
BSPUARTTEST_SANDBOX=$(CONFIG_CABAL_SANDBOX)

BSPUARTTEST_SRCDIR=$(BSPUARTTEST_GENERATEDDIR)/src
BSPUARTTEST_INCDIR=$(BSPUARTTEST_GENERATEDDIR)/include/generated

BSPUARTTEST_GEN=bsp-uart-test-gen
BSPUARTTEST_GENERATOR_EXE=$(BSPUARTTEST_SANDBOX)/bin/$(BSPUARTTEST_GEN)

BSPUARTTEST_IVORY_OPTS=--const-fold --overflow --div-zero
# A little too noisy: --fp-check

BSPUARTTEST_GENERATED_DEP=$(BSPUARTTEST_GENERATEDDIR)/dep.mk

include $(BSPUARTTEST_GENERATED_DEP)

# ------------------------------------------------------------------------------

# Generate the srcs and headers.
BSPUARTTEST += bsp-uart-build
.PHONY: bsp-uart-build
bsp-uart-build: $(BSPUARTTEST_GENERATED_HEADERS) $(BSPUARTTEST_GENERATED_SOURCES)

$(BSPUARTTEST_GENERATED_DEP): $(BSPUARTTEST_GENERATOR_EXE)
	mkdir -p $(BSPUARTTEST_GRAPHS_DIR)
	mkdir -p $(BSPUARTTEST_SRCDIR)
	mkdir -p $(BSPUARTTEST_INCDIR)
	$(BSPUARTTEST_GENERATOR_EXE) \
	--src-dir=$(BSPUARTTEST_SRCDIR) \
	--include-dir=$(BSPUARTTEST_INCDIR) \
	--deps=$(BSPUARTTEST_GENERATED_DEP) \
	--dep-prefix=BSPUARTTEST_GENERATED \
	--cfg=true \
	--cfg-dot-dir=$(BSPUARTTEST_GRAPHS_DIR) \
	--cfg-proc=stabilize_from_rate \
	--cfg-proc=pid_update \
	$(BSPUARTTEST_IVORY_OPTS)

# We don't add the .hs files as dependencies.  It's up to the user to clean
# them.  (It's not even clear if that's the right thing to do, since even if
# they don't change, if the compiler changes, you should rebuild.)
$(BSPUARTTEST_GENERATED_HEADERS) $(BSPUARTTEST_GENERATED_SOURCES): $(BSPUARTTEST_GENERATED_DEP) $(BSPUARTTEST_GENERATOR_EXE)
	$(BSPUARTTEST_GENERATOR_EXE) \
	--src-dir=$(BSPUARTTEST_SRCDIR) \
	--include-dir=$(BSPUARTTEST_INCDIR) \
	$(BSPUARTTEST_IVORY_OPTS)

CLEAN     += $(BSPUARTTEST_GENERATED_DEP)
# use wildcard, not the dep file, to clean subdirs, because if dep file
# doesn't exist we won't get a proper clean.
CLEAN     += $(wildcard $(BSPUARTTEST_SRCDIR)/*.c)
CLEAN     += $(wildcard $(BSPUARTTEST_INCDIR)/*.h)

CLEAN     += $(BSPUARTTEST_GRAPHS_DIR)

include apps/bsp-uart-test/app.mk


TWRTEST_GENERATEDDIR=apps/tower-test/generated
TWRTEST_GRAPHS_DIR=apps/tower-test/graphs

# Relative to current dir
TWRTEST_SANDBOX=$(CONFIG_CABAL_SANDBOX)

TWRTEST_SRCDIR=$(TWRTEST_GENERATEDDIR)/src
TWRTEST_INCDIR=$(TWRTEST_GENERATEDDIR)/include/generated

TWRTEST_GEN=ivory-tower-freertos-example
TWRTEST_GENERATOR_EXE=$(TWRTEST_SANDBOX)/bin/$(TWRTEST_GEN)

TWRTEST_IVORY_OPTS=--const-fold --overflow --div-zero
# A little too noisy: --fp-check

TWRTEST_GENERATED_DEP=$(TWRTEST_GENERATEDDIR)/dep.mk

include $(TWRTEST_GENERATED_DEP)

# ------------------------------------------------------------------------------

# Generate the srcs and headers.
TWRTEST += twrtest-build
.PHONY: twrtest-build
twrtest-build: $(TWRTEST_GENERATED_HEADERS) $(FLIGHT_GENERATED_SOURCES)

$(TWRTEST_GENERATED_DEP): $(TWRTEST_GENERATOR_EXE)
	mkdir -p $(TWRTEST_GRAPHS_DIR)
	mkdir -p $(TWRTEST_SRCDIR)
	mkdir -p $(TWRTEST_INCDIR)
	$(TWRTEST_GENERATOR_EXE) \
	--src-dir=$(TWRTEST_SRCDIR) \
	--include-dir=$(TWRTEST_INCDIR) \
	--deps=$(TWRTEST_GENERATED_DEP) \
	--dep-prefix=TWRTEST_GENERATED \
	--cfg=true \
	--cfg-dot-dir=$(TWRTEST_GRAPHS_DIR) \
	--cfg-proc=stabilize_from_rate \
	--cfg-proc=pid_update \
	$(TWRTEST_IVORY_OPTS)

# We don't add the .hs files as dependencies.  It's up to the user to clean
# them.  (It's not even clear if that's the right thing to do, since even if
# they don't change, if the compiler changes, you should rebuild.)
$(TWRTEST_GENERATED_HEADERS) $(TWRTEST_GENERATED_SOURCES): $(TWRTEST_GENERATED_DEP) $(TWRTEST_GENERATOR_EXE)
	$(TWRTEST_GENERATOR_EXE) \
	--src-dir=$(TWRTEST_SRCDIR) \
	--include-dir=$(TWRTEST_INCDIR) \
	$(TWRTEST_IVORY_OPTS)

CLEAN     += $(TWRTEST_GENERATED_DEP)
# use wildcard, not the dep file, to clean subdirs, because if dep file
# doesn't exist we won't get a proper clean.
CLEAN     += $(wildcard $(TWRTEST_SRCDIR)/*.c)
CLEAN     += $(wildcard $(TWRTEST_INCDIR)/*.h)

CLEAN     += $(TWRTEST_GRAPHS_DIR)

include apps/tower-test/app.mk

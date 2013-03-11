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

GRAPHS_DIR=$(TOP)/ivory/graphs

include $(GENERATED_DEP)

# ------------------------------------------------------------------------------

# Generate the srcs and headers.
IVORY += ivory-build
.PHONY: ivory-build
ivory-build: $(EXEC) $(FLIGHT_GENERATED_HEADERS) $(FLIGHT_GENERATED_SOURCES)

$(GENERATED_DEP):
	mkdir -p $(GRAPHS_DIR)
	$(SANDBOX)/bin/$(GEN) \
	--src-dir=$(SRCDIR) \
	--include-dir=$(INCDIR) \
	--deps=$(GENERATED_DEP) \
	--dep-prefix=FLIGHT_GENERATED \
	--cfg=true \
	--cfg-dot-dir=$(GRAPHS_DIR) \
	--cfg-proc=stabilize_from_rate \
	--cfg-proc=pid_update

# We don't add the .hs files as dependencies.  It's up to the user to clean
# them.  (It's not even clear if that's the right thing to do, since even if
# they don't change, if the compiler changes, you should rebuild.)
$(FLIGHT_GENERATED_HEADERS) $(FLIGHT_GENERATED_SOURCES): $(GENERATED_DEP)
	$(SANDBOX)/bin/$(GEN) --src-dir=$(SRCDIR) --include-dir=$(INCDIR) \
		$(IVORY_OPTS)

# Build the binary to generate the code.
.PRECIOUS: $(EXEC)
$(EXEC):
	cabal-dev -s $(SANDBOX) install --builddir=$(TOP)/ivory \
		$(TOP)/ivory

CLEAN     += $(GENERATED_DEP)
# use wildcard, not the dep file, to clean subdirs, because if dep file
# doesn't exist we won't get a proper clean.
CLEAN     += $(wildcard $(SRCDIR)/*.c)
CLEAN     += $(wildcard $(INCDIR)/*.h)
VERYCLEAN += $(TOP)/ivory/dist

CLEAN     += $(GRAPHS_DIR)

# ------------------------------------------------------------------------------
# CBMC stuff
# ------------------------------------------------------------------------------

CBMC_INCS = \
  -I./flight-generated/include/smaccmpilot \
  -I./flight-generated/include \
  -I./bsp/hwf4/include \
  -I./bsp/include \
  -I./ivory-runtime \
  -I./ivory-freertos-wrapper/include \
  -I./flight-support/include \
  -I./smavlink/include \
  -I./smavlink/include/smavlink/messages \
  -I./flight-support/include/smaccmpilot

STARTS := $(shell $(SANDBOX)/bin/$(GEN)\
  --src-dir=$(SRCDIR) \
  --include-dir=$(INCDIR) \
  --out-proc-syms)

CBMC_EXEC			:= $(addprefix $(CONFIG_CBMC_PREFIX)/, cbmc)
CBMC_REPORT		:= $(addprefix $(CONFIG_CBMC_REPORT)/, cbmc-report)
ENTRY_FUNCS		:= $(patsubst %, --function %, $(STARTS))
CBMC_SRCS			:= $(patsubst %, --src %, $(FLIGHT_GENERATED_SOURCES))

	# $(shell $(CBMC_REPORT) --help --cbmc $(CBMC_EXEC) $(ENTRY_FUNCS) \
  #   -D IVORY_CBMC $(CBMC_INCS) $(FLIGHT_GENERATED_SOURCES) >&2)
#--cbmc $(CBMC_EXEC) $(ENTRY_FUNCS) -D IVORY_CBMC

# $(FLIGHT_GENERATED_SOURCES)

# >&2 redirects cbmc output from stderr so you can see it.
.PHONY: verify
verify: $(FLIGHT_GENERATED_HEADERS) $(FLIGHT_GENERATED_SOURCES)
	# echo $(ENTRY_FUNCS)
	$(CBMC_REPORT) --verbose --cbmc $(CBMC_EXEC) $(ENTRY_FUNCS) \
    -- -D IVORY_CBMC $(CBMC_INCS) $(FLIGHT_GENERATED_SOURCES)




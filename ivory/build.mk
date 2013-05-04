GENERATEDDIR=flight-generated

# Relative to current dir
SANDBOX=$(CONFIG_CABAL_SANDBOX)

SRCDIR=$(GENERATEDDIR)/src
INCDIR=$(GENERATEDDIR)/include/flight-generated

GEN=smaccmpilot-gen
GENERATOR_EXE=$(SANDBOX)/bin/$(GEN)
# EXEC=ivory/dist/build/$(GEN)/$(GEN)

IVORY_OPTS=--const-fold --overflow --div-zero
# A little too noisy: --fp-check

GENERATED_DEP=$(GENERATEDDIR)/dep.mk

GRAPHS_DIR=$(TOP)/ivory/graphs

include $(GENERATED_DEP)

# ------------------------------------------------------------------------------

# Generate the srcs and headers.
IVORY += ivory-build
.PHONY: ivory-build
ivory-build: $(FLIGHT_GENERATED_HEADERS) $(FLIGHT_GENERATED_SOURCES)

$(GENERATED_DEP): $(GENERATOR_EXE)
	mkdir -p $(GRAPHS_DIR)
	$(SANDBOX)/bin/$(GEN) \
	--src-dir=$(SRCDIR) \
	--include-dir=$(INCDIR) \
	--deps=$(GENERATED_DEP) \
	--dep-prefix=FLIGHT_GENERATED \
	--cfg=true \
	--cfg-dot-dir=$(GRAPHS_DIR) \
	--cfg-proc=stabilize_from_rate \
	--cfg-proc=pid_update \
  $(IVORY_OPTS)

# We don't add the .hs files as dependencies.  It's up to the user to clean
# them.  (It's not even clear if that's the right thing to do, since even if
# they don't change, if the compiler changes, you should rebuild.)
$(FLIGHT_GENERATED_HEADERS) $(FLIGHT_GENERATED_SOURCES): $(GENERATED_DEP) $(GENERATOR_EXE)
	$(SANDBOX)/bin/$(GEN) --src-dir=$(SRCDIR) --include-dir=$(INCDIR) \
		$(IVORY_OPTS)

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
  -I./bsp/hwf4/include \
  -I./bsp/include \
  -I./ivory-runtime \
  -I./ivory-freertos-wrapper/include \
  -I./smavlink/include \
  -I./smavlink/include/smavlink/messages \
  -I./flight-support/include \
  -I./flight-generated/include \
  -I./flight-generated/include/flight-generated \
  $(FREERTOS_INCLUDES)

STARTS := $(shell $(SANDBOX)/bin/$(GEN)\
  --src-dir=$(SRCDIR) \
  --include-dir=$(INCDIR) \
  --out-proc-syms)

CBMC_EXEC		:= $(addprefix $(CONFIG_CBMC_PREFIX)/, cbmc)
CBMC_REPORT	:= $(addprefix $(CONFIG_CBMC_REPORT)/, cbmc-reporter)
CBMC_SRCS		:= $(patsubst %, --src=%, $(FLIGHT_GENERATED_SOURCES))
TABLE        = $(TOP)/ivory/claims-table
ENTRY_FUNCS	:= $(patsubst %, --function=%, $(STARTS))

.PHONY: verify
verify: $(FLIGHT_GENERATED_HEADERS) $(FLIGHT_GENERATED_SOURCES)
	$(CBMC_REPORT) \
    --outfile=$(TABLE).md \
    --format=markdown \
    --timeout=60 \
    --no-asserts \
    --threads=2 \
    --sort=result \
    --cbmc=$(CBMC_EXEC) \
    $(CBMC_INCS) \
    $(CBMC_SRCS) \
    $(ENTRY_FUNCS) \
    -- -D IVORY_CBMC
	pandoc -o $(TABLE).html $(TABLE).md

# Just for testing
.PHONY: verify-test
verify-test: $(FLIGHT_GENERATED_HEADERS) $(FLIGHT_GENERATED_SOURCES)
	$(CBMC_REPORT) \
    --format=markdown \
    --timeout=60 \
    --no-asserts \
    --sort=result \
    --cbmc=$(CBMC_EXEC) \
    $(CBMC_INCS) \
    $(CBMC_SRCS) \
    --function=tower_entry \
    -- -D IVORY_CBMC

    # --outfile=$(TOP)/ivory/claims-table-tmp.md \

CLEAN += $(TABLE).html

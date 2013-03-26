
# ------------------------------------------------------------------------------
# CBMC stuff
# ------------------------------------------------------------------------------

INCS=                                    \
  -I./hwf4/include			 \
  -I./include				 \
  -I./smavlink/include			 \
  -I./smavlink/include/smavlink/messages \
  -I./include

STARTS := $(shell $(SANDBOX)/bin/$(GEN) --src-dir=$(SRCDIR) \
			--include-dir=$(INCDIR) --out-proc-syms)

# STARTS = stabilize_from_rate stabilize_from_angle

CBMC_EXEC := $(addprefix $(CONFIG_CBMC_PREFIX)/, cbmc)

# >&2 redirects cbmc output from stderr so you can see it.
.PHONY: verify
verify: $(HDRS) $(SRCS)
	$(foreach func, $(STARTS), \
    $(shell $(CBMC_EXEC) -D IVORY_CBMC $(INCS) --function $(func) $(SRCS) >&2 ))

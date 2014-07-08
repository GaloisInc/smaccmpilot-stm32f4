# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# frama-c-check.mk --- memory safety/no undefined behavior via abstract
# interpratation.
#
# Copyright (C) 2013, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#
# Written by Lee Pike <leepike@galois.com>

FRAMA_C_OUT = -frama-c.log

# .PHONY: %$(FRAMA_C_OUT)
# %$(FRAMA_C_OUT):
# 	@echo $@

define frama_c_pkg

$(1)_PREFIX       := $(dir $(lastword $(filter %/build.mk,$(MAKEFILE_LIST))))
$(1)_GEN_DIR      := $$(GEN_DIR)/$$($(1)_PREFIX)
$(1)_ENTRY_FILE   := $$($(1)_GEN_DIR)/$(2)_entrypoints.mk

# Defines the variables $(1)_TASKS, if they exist.
-include $$($(1)_ENTRY_FILE)

$(1)_ENTRY_FUNCS_LOGS += $$(patsubst %, %$(FRAMA_C_OUT), $$($(2)_TASKS))

# Set the targets to be built, called in mk/main.mk.
FRAMA_C += $$($(1)_ENTRY_FUNCS_LOGS)

# Rule to build each log file for each entry point.
$$($(1)_ENTRY_FUNCS_LOGS):
	cd $$($(1)_GEN_DIR) ; \
	$(CONFIG_FRAMA_C_PREFIX)/frama-c \
	-main $$(patsubst %$(FRAMA_C_OUT), %, $$@) \
  -unsafe-arrays -lib-entry \
  -val -slevel 1000 \
  -save $$@ \
  *.i

# FRAMA_C += $$($(1)_GEN_DIR)
#FRAMA_C += $$($(1)_ENTRY_FILE)


endef

# vim: set ft=make noet ts=2:

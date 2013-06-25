# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# ivory.mk --- Compiling generated Ivory packages.
#
# Copyright (C) 2012, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#
# Written by James Bielman <jamesjb@galois.com>, June 17, 2013
#

# $(1) -- prefix variable name unique to this ivory module
# $(2) -- name of the generator program (without path)
#
# Optional definitions:
#
#   $(1)_INCLUDE_DIR -- rel. subdirectory include files are placed in
#   $(1)_GEN_SYMS    -- if defined, generate $(1)_SYMS

define ivory_pkg
$(1)_PREFIX           := $(dir $(lastword $(filter %/build.mk,$(MAKEFILE_LIST))))
$(1)_GEN_DIR          := $$(GEN_DIR)/$$($(1)_PREFIX)
$(1)_DEP_FILE         := $$($(1)_GEN_DIR)/dep.mk
$(1)_GEN_EXE          := $$(CONFIG_CABAL_SANDBOX)/bin/$(2)
$(1)_CFLAGS           := -I$$($(1)_GEN_DIR)

-include $$($(1)_DEP_FILE)

$$($(1)_DEP_FILE): $$($(1)_GEN_EXE) $(MAKEFILE_LIST)
	$$($(1)_GEN_EXE)                                    \
	--src-dir=$$($(1)_GEN_DIR)                          \
	--include-dir=$$($(1)_GEN_DIR)/$$($(1)_INCLUDE_DIR) \
	--dep-file=$$($(1)_DEP_FILE)                                \
	--dep-prefix=$(1)

$$($(1)_HEADERS) $$($(1)_SOURCES): $$($(1)_DEP_FILE)
	$$($(1)_GEN_EXE)                                    \
	--src-dir=$$($(1)_GEN_DIR)                          \
	--include-dir=$$($(1)_GEN_DIR)/$$($(1)_INCLUDE_DIR) \
	$$(IVORY_OPTS)

ifdef $(1)_GEN_SYMS
$(1)_SYMS := $$(shell $$($(1)_GEN_EXE)                \
  --src-dir=$$($(1)_GEN_DIR)                          \
	--include-dir=$$($(1)_GEN_DIR)/$$($(1)_INCLUDE_DIR) \
	--out-proc-syms)
endif

$(1)_OBJECTS := $$(patsubst $$($(1)_GEN_DIR)%.c,$$(OBJ_DIR)/$$($(1)_PREFIX)%.o,$$($(1)_SOURCES))

endef

# vim: set ft=make noet ts=2:

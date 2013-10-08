# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 8 -*-
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

quiet_cmd_ivory_dep = IVORYDEP $2
      cmd_ivory_dep = $3

quiet_cmd_ivory_gen = IVORYGEN $2
      cmd_ivory_gen = $3

quiet_cmd_dot       = DOT      $2
      cmd_dot       = dot -Tpdf $3 -o $4

# Commsec keys
COMMSEC_ARGS = \
  --senderid=$(UAV_ID) \
  --sendkey=$(UAV_KEY) \
  --sendsalt=$(UAV_SALT) \
  --recvkey=$(BASE_KEY) \
  --recvsalt=$(BASE_SALT) \

# tower_pkg:
# $1 package name
# $2 package generator exe

define tower_pkg
$(1)_TOWER_DOT  = $$($(1)_GEN_DIR)/$(2).dot
$(1)_TOWER_PDF  = $$($(1)_GEN_DIR)/$(2).pdf

$(call ivory_pkg,$1,$2,--platform=$$($(CONFIG_PLATFORM)_TOWER_PLATFORM) \
                       --operating-system=$$($(CONFIG_PLATFORM)_TOWER_OS))

$$($(1)_TOWER_PDF): $$($(1)_TOWER_DOT)
	$(call cmd,dot,$1,$$($(1)_TOWER_DOT),$$($(1)_TOWER_PDF))

ifneq ($(CONFIG_BUILD_DOT_PDF),)
IMAGES += $$($(1)_TOWER_PDF)
endif

endef

# ivory_pkg:
# $1 package name
# $2 package generator exe
# $3 extra flags for package generator exe

define ivory_pkg
$(1)_PREFIX           := $(dir $(lastword $(filter %/build.mk,$(MAKEFILE_LIST))))
$(1)_GEN_DIR          := $$(GEN_DIR)/$$($(1)_PREFIX)
$(1)_DEP_FILE         := $$($(1)_GEN_DIR)/dep.mk
$(1)_GEN_EXE          := $$(CONFIG_CABAL_SANDBOX)/bin/$(2)
$(1)_CFLAGS           := -I$$($(1)_GEN_DIR)

-include $$($(1)_DEP_FILE)


$$($(1)_DEP_FILE): $$($(1)_GEN_EXE) $(MAKEFILE_LIST)
	$(call cmd,ivory_dep,$1,                              \
	  $$($(1)_GEN_EXE)                                    \
	  --src-dir=$$($(1)_GEN_DIR)                          \
	  --include-dir=$$($(1)_GEN_DIR)/$$($(1)_INCLUDE_DIR) \
	  --dep-file=$$($(1)_DEP_FILE)                        \
	  --dep-prefix=$(1)                                   \
	  $(3))

$$($(1)_HEADERS) $$($(1)_SOURCES) $$($(1)_TOWER_DOT): $$($(1)_DEP_FILE)
	$(call cmd,ivory_gen,$1,                              \
	  $$($(1)_GEN_EXE)                                    \
	  --src-dir=$$($(1)_GEN_DIR)                          \
	  --include-dir=$$($(1)_GEN_DIR)/$$($(1)_INCLUDE_DIR) \
          --overflow                                          \
          --div-zero                                          \
          --ix-check                                          \
          --const-fold                                        \
	  $$(IVORY_OPTS)                                            \
          $(COMMSEC_ARGS) \
	  $(3))

ifdef $(1)_GEN_SYMS
$(1)_SYMS := $$(shell $$($(1)_GEN_EXE)                  \
	  --src-dir=$$($(1)_GEN_DIR)                          \
	  --include-dir=$$($(1)_GEN_DIR)/$$($(1)_INCLUDE_DIR) \
	  --out-proc-syms                                     \
	  $(3))
endif

$(1)_OBJECTS := $$(patsubst $$($(1)_GEN_DIR)%.c,$$(OBJ_DIR)/$$($(1)_PREFIX)%.o,$$($(1)_SOURCES))

endef

# vim: set ft=make noet ts=2:

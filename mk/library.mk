# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# library.mk --- Building libraries from object files.
#
# Copyright (C) 2012, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#
# Written by James Bielman <jamesjb@galois.com>, December 07, 2012
#

# Compile a library from a set of objects.  This macro accepts a
# single argument that is the prefix to a set of variable names
# containing information about the library.
#
# If $(1) is "FOO", then the macro expansion will reference the
# following variables:
#
# FOO_LIB         name of the library (eg: libfoo.a)
# FOO_OBJECTS     list of .o files relative to the Makefile
# FOO_CFLAGS      additional compiler flags for "FOO_OBJECTS"
define library
$(1)_PREFIX         := $(dir $(lastword $(MAKEFILE_LIST)))
$(1)_REAL_OBJECTS   := $$(addprefix $$(OBJ_DIR)/$$($(1)_PREFIX),$$($(1)_OBJECTS))
$(1)_REAL_LIB       := $$(addprefix $$(LIB_DIR)/,$$($(1)_LIB))

$$($(1)_REAL_LIB): CFLAGS  += $$($(1)_CFLAGS)

LIBRARIES   += $$($(1)_REAL_LIB)
ALL_OBJECTS += $$($(1)_REAL_OBJECTS)

$$($(1)_REAL_LIB): $$($(1)_REAL_OBJECTS)
	$$(Q)mkdir -p $$(dir $$@)
	$$(call cmd,lib,$$($(1)_REAL_OBJECTS))

$($(1)_LIB): $$($(1)_REAL_LIB)
endef

# vim: set ft=make noet ts=2:

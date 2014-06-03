# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# image.mk --- Building program images.
#
# Copyright (C) 2012, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#
# Written by James Bielman <jamesjb@galois.com>, December 07, 2012
#

# Compile an image from a set of objects.  This macro accepts a single
# argument that is the prefix to a set of variable names containing
# information about the image.
#
# If $(1) is "FOO", then the macro expansion will reference the
# following variables:
#
# FOO_IMG         name of the library (eg: ledtest)
# FOO_OBJECTS     list of .o files relative to the Makefile
# FOO_CFLAGS      additional C compiler flags for "FOO_OBJECTS"
# FOO_CXXFLAGS    additional C++ compiler flags
# FOO_LDFLAGS     additional linker flags for "FOO_IMG"
# FOO_LIBRARIES   built libraries to link against (eg: libFreeRTOS.a)
# FOO_LIBS        system libraries to link against (eg: -lfoo)
define image
$(1)_PREFIX         := $(dir $(lastword $(filter %/build.mk,$(MAKEFILE_LIST))))
ifeq ($$($(1)_DISABLE_GLOBAL_STARTUP_OBJECTS),)
$(1)_REAL_OBJECTS   += $$(addprefix $$(OBJ_DIR)/,$$(STARTUP_OBJECTS))
endif
$(1)_REAL_OBJECTS   += $$(addprefix $$(OBJ_DIR)/$$($(1)_PREFIX),$$($(1)_OBJECTS))
$(1)_REAL_IMG       := $$(addprefix $$(IMG_DIR)/,$$($(1)_IMG))
$(1)_REAL_BIN       := $$($(1)_REAL_IMG).bin
$(1)_REAL_LDS       := $$($(1)_REAL_IMG).lds
$(1)_REAL_LIBRARIES := $$(addprefix $$(LIB_DIR)/,$$($(1)_LIBRARIES))

$$($(1)_REAL_IMG): CFLAGS   += $$($(1)_CFLAGS)
$$($(1)_REAL_IMG): CXXFLAGS += $$($(1)_CXXFLAGS)
$$($(1)_REAL_IMG): LDFLAGS  += $$($(1)_LDFLAGS)
$$($(1)_REAL_IMG): LDFLAGS  += -Wl,--script=$$($(1)_REAL_LDS)
$$($(1)_REAL_IMG): LIBS     += $$($(1)_LIBS)

IMAGES      += $$($(1)_REAL_IMG) $$($(1)_REAL_BIN)
ALL_OBJECTS += $$($(1)_REAL_OBJECTS)

$$($(1)_REAL_LDS): $(LDSCRIPT)
	$$(Q)mkdir -p $$(dir $$@)
	$$(call cmd,cpp_lds_S,$$($(1)_REAL_LDS))

$$($(1)_REAL_IMG): $$($(1)_REAL_OBJECTS) $$($(1)_REAL_LIBRARIES) $$($(1)_REAL_LDS)
	$$(Q)mkdir -p $$(dir $$@)
	$$(call cmd,link,$$($(1)_REAL_OBJECTS) $$($(1)_REAL_LIBRARIES))

$$($(1)_REAL_BIN): $$($(1)_REAL_IMG)
	$$(call cmd,elf_to_bin,$$($(1)_REAL_IMG))

$($(1)_IMG): $$($(1)_REAL_IMG)

ifneq ($$($(CONFIG_PLATFORM)_SUPPORT_PX4IMG),)

$(1)_REAL_PX4IMG    := $$($(1)_REAL_IMG).px4

$$($(1)_REAL_PX4IMG): $$($(1)_REAL_BIN)
	$$(call cmd,bin_to_px4,$$($(1)_REAL_BIN))

$($(1)_IMG): $$($(1)_REAL_PX4IMG)
IMAGES += $$($(1)_REAL_PX4IMG)

endif

endef

# vim: set ft=make noet ts=2:

# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# Makefile --- STM324 firmware build system.
#
# Copyright (C) 2012, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#

include Config.mk

include mk/arch/$(CONFIG_TOOLCHAIN).mk
include mk/board/$(CONFIG_BOARD).mk

TOP := .

# Per arch and board output directories.
BUILD_DIR := $(TOP)/build/$(CONFIG_TOOLCHAIN)/$(CONFIG_BOARD)
OBJ_DIR   := $(BUILD_DIR)/obj
LIB_DIR   := $(BUILD_DIR)/lib
IMG_DIR   := $(BUILD_DIR)/img

# Add the built library directory to the default linker flags.
LDFLAGS += -L$(LIB_DIR)

.PHONY: all
all: all-targets

include mk/cmd.lib
include mk/library.mk
include mk/image.mk

define project
  include $(1)/build.mk
endef

# Search for subprojects and include their "build.mk" makefiles.
$(foreach p,$(shell find . -name build.mk -exec dirname {} \;), \
          $(eval $(call project,$(p))))

ALL_TARGETS := $(LIBRARIES) $(IMAGES)
ALL_DEPS    := $(patsubst %.o,%.d,$(ALL_OBJECTS))

######################################################################
## Targets

.PHONY: all-targets
all-targets: $(ALL_TARGETS)

.PHONY: clean
clean:
	$(Q)rm -f $(ALL_TARGETS) $(ALL_OBJECTS) $(ALL_DEPS)

######################################################################
## Compilation Rules

quiet_cmd_cc_o_c = CC      $<
      cmd_cc_o_c = $(CC) $(CFLAGS) -MMD -c -o $@ $<

# Compile a C source file to an object and dependency file.
$(OBJ_DIR)/%.o: %.c
	$(Q)mkdir -p $(dir $@)
	$(call cmd,cc_o_c)

quiet_cmd_cxx_o_c = CXX     $<
      cmd_cxx_o_c = $(CXX) $(CXXFLAGS) -MMD -c -o $@ $<

# Compile a C++ source file to an object and dependency file.
$(OBJ_DIR)/%.o: %.cpp
	$(Q)mkdir -p $(dir $@)
	$(call cmd,cxx_o_c)

quiet_cmd_link = LINK    $@
      cmd_link = $(CC) -o $@ $(LDFLAGS) -Wl,-Map=$@.map $(2) $(LIBS)

quiet_cmd_lib = AR      $@
      cmd_lib = $(AR) rcs $@ $(2) && $(RANLIB) $@

quiet_cmd_as_o_S = AS      $<
      cmd_as_o_S = $(CC) $(CFLAGS) -c -o $@ $<

# Compile an assembly source (.S) file to an object file.
$(OBJ_DIR)/%.o: %.S
	$(Q)mkdir -p $(dir $@)
	$(call cmd,as_o_S)

quiet_cmd_as_o_s = AS      $<
      cmd_as_o_s = $(CC) $(CFLAGS) -c -o $@ $<

# Compile an assembly source (.s) file to an object file.
$(OBJ_DIR)/%.o: %.s
	$(Q)mkdir -p $(dir $@)
	$(call cmd,as_o_s)

quiet_cmd_elf_to_bin = OBJCOPY $@
      cmd_elf_to_bin = $(OBJCOPY) -O binary $(2) $(2).bin

# Make all object files depend on all included Makefiles, to force a
# rebuild if the build system or configuration is modified.
$(ALL_OBJECTS): $(MAKEFILE_LIST)

# Include generated dependency files if they exist.
-include $(ALL_DEPS)

# vim: set ft=make noet ts=2:

# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# cortex-m4.mk --- Cortex-M4 toolchain configuration.
#
# Copyright (C) 2012, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#
# Written by James Bielman <jamesjb@galois.com>, December 07, 2012
#

# Prefix for GCC commands for the ARM toolchain.  This must be set
# in the user's "Config.mk" which is included by the top-level Makefile.
TOOLCHAIN_PREFIX := $(CONFIG_CORTEX_M4_PREFIX)

# Compiler and linker commands.
CC     := $(TOOLCHAIN_PREFIX)gcc
LD     := $(TOOLCHAIN_PREFIX)ld
AR     := $(TOOLCHAIN_PREFIX)ar
RANLIB := $(TOOLCHAIN_PREFIX)ranlib

# Compiler flags.
CFLAGS := -g -std=gnu99 -Wall -mlittle-endian -mthumb \
          -mcpu=cortex-m4 -mfloat-abi=hard -mfpu=fpv4-sp-d16

# Linker flags.
LDFLAGS := -mlittle-endian -mcpu=cortex-m4 -mthumb -mfloat-abi=hard \
           -mfpu=fpv4-sp-d16 -Wl,--script=stm32_flash.ld

# Startup code source files.
STARTUP_OBJECTS := source/startup_stm32f4xx.o \
                   source/system_stm32f4xx.o

# vim: set ft=make noet ts=2:

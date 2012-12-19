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
CC      := $(TOOLCHAIN_PREFIX)gcc
CXX     := $(TOOLCHAIN_PREFIX)g++
LD      := $(TOOLCHAIN_PREFIX)ld
AR      := $(TOOLCHAIN_PREFIX)ar
RANLIB  := $(TOOLCHAIN_PREFIX)ranlib
OBJCOPY := $(TOOLCHAIN_PREFIX)objcopy

# Base compiler flags for C and C++.
BASE_CFLAGS := -g -Wall -mlittle-endian -mthumb -mcpu=cortex-m4 \
               -mfloat-abi=hard -mfpu=fpv4-sp-d16

# C compiler flags.
CFLAGS := $(BASE_CFLAGS) -std=gnu99

# C++ compiler flags.
CXXFLAGS := $(BASE_CFLAGS) -fno-exceptions -fno-rtti

# Path to the linker script.
LDSCRIPT := stm32_flash.ld

# Linker flags.
LDFLAGS := -mlittle-endian -mcpu=cortex-m4 -mthumb -mfloat-abi=hard \
           -mfpu=fpv4-sp-d16 -Wl,--script=$(LDSCRIPT)

# Startup code source files.
STARTUP_OBJECTS := source/startup_stm32f4xx.o \
                   source/system_stm32f4xx.o

# vim: set ft=make noet ts=2:

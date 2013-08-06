# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# platform/platform_px4fmu17_freertos.mk
# PX4FMU 1.7 (STM32F4 Cortex-M4) toolchain and board support package
# configuration.
#
# Copyright (C) 2012, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#
# Written by James Bielman <jamesjb@galois.com>, December 07, 2012
#            Pat Hickey    <pat@galois.com>,     August 06, 2013
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
BASE_CFLAGS := -g -Wall -O2 -mlittle-endian -mthumb -mcpu=cortex-m4 \
               -mfloat-abi=hard -mfpu=fpv4-sp-d16 \
               -Wno-parentheses
# C compiler flags.
CFLAGS := $(BASE_CFLAGS) -std=gnu99

# C++ compiler flags.
CXXFLAGS := $(BASE_CFLAGS) -fno-exceptions -fno-rtti

# Path to the linker script.
LDSCRIPT := src/bsp/stm32_flash.lds.S

# Linker flags.
LDFLAGS := -mlittle-endian -mcpu=cortex-m4 -mthumb -mfloat-abi=hard \
           -mfpu=fpv4-sp-d16

ifneq ($(CONFIG_PX4FMU_BOOTLOADER),)
LDSCRIPT_OPTS := -DCONFIG_PX4FMU_BOOTLOADER
endif

# Startup code source files.
STARTUP_OBJECTS := src/bsp/init/startup_stm32f4xx.o \
                   src/bsp/init/system_stm32f4xx.o

# Frequency of the HSE crystal in Hz.
BOARD_HSE_FREQ := 24000000

# Add the HSE frequency to the default CFLAGS.
CFLAGS += -DHSE_VALUE=$(BOARD_HSE_FREQ)

# Add a preprocessor definition for this board.
CFLAGS += -DCONFIG_BOARD_PX4

# Select the full-speed USB OTG core.
CFLAGS += -DUSE_USB_OTG_FS

# support building PX4 images for this platform
px4fmu17_freertos_SUPPORT_PX4IMG := 1

# platform argument for tower builds
px4fmu17_freertos_TOWER_PLATFORM := px4fmu17

# operating system argument for tower builds
px4fmu17_freertos_TOWER_OS := freertos

# vim: set ft=make noet ts=2:

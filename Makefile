# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# Makefile --- SMACCMPilot firmware build system.
#
# Copyright (C) 2013, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#

.SUFFIXES:
MAKEFLAGS += -r

# Only for overide of CONFIG_DEFAULT_TARGET
include Config.mk

PLATFORM_DIR := ./mk/platform/
PLATFORMS = $(subst platform_,,$(basename $(notdir $(wildcard $(PLATFORM_DIR)platform_*.mk))))

CONFIG_DEFAULT_TARGET ?= px4fmu17_ioar_freertos

# debugging:
MQUIET = --no-print-directory
#MQUIET = --print-directory

default: $(CONFIG_DEFAULT_TARGET)

allplatforms:
	@for platform in $(PLATFORMS); do \
		if [ -e mk/platform/platform_$$platform.mk ] ; then \
			echo building for platform $$platform; \
			make -f mk/main.mk $(MQUIET) $(TARGET) CONFIG_PLATFORM=$$platform; \
		else \
			echo ERROR: platform file for $$platform does not exist!; \
		fi \
	done

px4fmu17_ioar_freertos: PLATFORMS = px4fmu17_ioar_freertos
px4fmu17_ioar_freertos: allplatforms

px4fmu17_bare_freertos: PLATFORMS = px4fmu17_bare_freertos
px4fmu17_bare_freertos: allplatforms

aadl: PLATFORMS = px4fmu17_ioar_aadl
aadl: allplatforms

discovery: PLATFORMS = stm32f4discovery
discovery: allplatforms

open407: PLATFORMS = open407vc
open407: allplatforms

cbmc: TARGET = cbmc
cbmc: default

clean:
	-rm -rf ./build


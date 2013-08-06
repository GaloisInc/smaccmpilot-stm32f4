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

include Config.mk

PLATFORM_DIR := ./mk/platform/
PLATFORMS = $(subst platform_,,$(basename $(notdir $(wildcard $(PLATFORM_DIR)platform_*.mk))))

# debugging:
MQUIET = --no-print-directory
#MQUIET = --print-directory

default: px4fmu17_freertos

allplatforms:
	@for platform in $(PLATFORMS); do \
		make -f mk/main.mk $(MQUIET) $(TARGET) CONFIG_PLATFORM=$$platform; \
	done

px4fmu17_freertos: PLATFORMS = px4fmu17_freertos
px4fmu17_freertos: allplatforms

aadl: PLATFORMS = px4fmu17_aadl
aadl: allplatforms

clean:
	-rm -rf ./build


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

# Only for overide of CONFIG_DEFAULT_PLATFORM
include Config.mk

PLATFORM_DIR := ./mk/platform/
PLATFORM_FILES := $(wildcard $(PLATFORM_DIR)platform_*.mk)

ifneq ($(CONFIG_PLATFORMS),)
COMMA := ,
PLATFORMS := $(subst $(COMMA), ,$(CONFIG_PLATFORMS))
else
PLATFORMS := $(subst platform_,,$(basename $(notdir $(PLATFORM_FILES))))
endif

CONFIG_DEFAULT_PLATFORM ?= px4fmu17_ioar_freertos

# debugging:
MQUIET = --no-print-directory
#MQUIET = --print-directory

default: $(CONFIG_DEFAULT_PLATFORM)

allplatforms:
	@echo selected platforms: $(PLATFORMS)
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

# Target defined in mk/main.mk
cbmc: TARGET = cbmc
cbmc: default

# Target defined in mk/cppcheck.mk
cppcheck: TARGET = cpp-check
cppcheck: default

framac-check: TARGET = frama-c-check
framac-check: default

clean:
	-rm -rf ./build

include mk/mavlink.mk
mavlink: $(MAVLINK_TARGETS)


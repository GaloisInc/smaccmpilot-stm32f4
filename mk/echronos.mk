# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# build.mk --- Build rules for FreeRTOS.
#
# Copyright (C) 2012, Galois, Inc.
# All Rights Reserved.
#
# Written by James Bielman <jamesjb@galois.com>, December 07, 2012
# Modified for eChronos by Alexander Kroh <alex.kroh@nicta.com.au>, November 13, 2013

ifeq ($($(CONFIG_PLATFORM)_TOWER_OS),echronos)
ifndef CONFIG_ECHRONOS_PREFIX
$(error "CONFIG_ECHRONOS_PREFIX is not defined.")
endif

ECHRONOS_PATH=$(CONFIG_ECHRONOS_PREFIX)
ECHRONOS_PACKAGE=$(ECHRONOS_PATH)/share/packages
PRJ=$(realpath $(CONFIG_ECHRONOS_PRJ)/prj)

ECHRONOS_RAW_OBJECTS := ctxt-switch.o               \
                        armv7m.exception-preempt.o  \
                        generic.debug.o             \
                        rtos-kochab.o               \
                        semihost-debug.o            \
                        vectable.o                  \
                        resources.o

LIBECHRONOS_DIR := $(TOP)/src/bsp/eChronos

%/default.ld.S: %/default.ld
	cp $< $@

define echronos_gen
$(1)_PREFIX           := $(dir $(lastword $(filter %/build.mk,$(MAKEFILE_LIST))))
$(1)_GEN_DIR          := $$(GEN_DIR)/$$($(1)_PREFIX)

$(1)_ECHRONOS_PRX     ?= $$($(1)_IMG).prx
$(1)_ECHRONOS_OUT     := $$($(1)_GEN_DIR)rtos

# We need absolute paths here as we are calling an external
$(1)_ECHRONOS_DIR     := $(CURDIR)/$$($(1)_PREFIX)
$(1)_ECHRONOS_PRX     := $$($(1)_ECHRONOS_DIR)/$$($(1)_ECHRONOS_PRX)
$(1)_ECHRONOS_OUT_ABS := $(CURDIR)/$$($(1)_ECHRONOS_OUT)

# Update the entry points in our prx file.
$$($(1)_ECHRONOS_PRX): $$(wildcard $$($(1)_GEN_DIR)/*_entrypoints.xml)
	sed "s/.*taskname=\\\"\(.*\)_\(.*\)_proc.*/s\/\\\(\1\\\)_[0-9][0-9]*_proc\/\\\1_\2_proc\/I/" $$< | sed -f - -i $$@

# OS recipe
$$($(1)_ECHRONOS_OUT_ABS): $$($(1)_ECHRONOS_PRX)
	cd $(ECHRONOS_PACKAGE) &&                         \
            $(PRJ) -o $$($(1)_ECHRONOS_OUT_ABS)     \
                    --search .                      \
                    --no-project                    \
                    gen $$($(1)_ECHRONOS_PRX)
	rm -f $$($(1)_ECHRONOS_OUT_ABS)/stm32f4xx.h
	cp $(LIBECHRONOS_DIR)/support/resources.c $$@

# Ensure that echronos is built before the application
# REAL_OBJECTS do not exist yet
$$(addprefix $$(OBJ_DIR)/$$($(1)_PREFIX),$$($(1)_OBJECTS)): $$($(1)_ECHRONOS_OUT_ABS)

# Additional resources for applications
$(1)_INCLUDES  += -I$$($(1)_ECHRONOS_OUT)
$(1)_INCLUDES  += -I$(TOP)/src/bsp/eChronos/include
$(1)_LIBRARIES += libeChronos.a
$(1)_OBJECTS   += $(addprefix rtos/,$(ECHRONOS_RAW_OBJECTS))

# build specific linker script do we need an image prefix here?
LDSCRIPT       := $$($(1)_ECHRONOS_OUT)/default.ld.S


endef
endif
# vim: set ft=make noet ts=2:

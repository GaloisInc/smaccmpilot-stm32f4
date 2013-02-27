# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# build.mk --- Build rules for FreeRTOS.
#
# Copyright (C) 2012, Galois, Inc.
# All Rights Reserved.
#
# Written by James Bielman <jamesjb@galois.com>, December 07, 2012
#

ifndef CONFIG_FREERTOS_PREFIX
$(error "CONFIG_FREERTOS_PREFIX is not defined.")
endif

FREERTOS_SRC    := $(CONFIG_FREERTOS_PREFIX)/FreeRTOS/Source

FREERTOS_LIB    := libFreeRTOS.a

FREERTOS_INCLUDES := -I$(FREERTOS_SRC)/include
FREERTOS_INCLUDES += -I$(FREERTOS_SRC)/portable/GCC/ARM_CM4F
FREERTOS_INCLUDES += -I$(TOP)/bsp/include

FREERTOS_CFLAGS   += $(FREERTOS_INCLUDES)

FREERTOS_OBJECTS :=                              \
  src/list.o                                     \
  src/croutine.o                                 \
  src/queue.o                                    \
  src/tasks.o                                    \
  src/timers.o                                   \
  src/portable/GCC/ARM_CM4F/port.o               \
  src/portable/MemMang/heap_1.o                  \
  support/default_hooks.o                        \
  support/default_handlers.o                     \
  support/syscalls.o

# Copy files from the FreeRTOS source tree into our local tree.  Our
# build system doesn't make it easy to compile out-of-tree sources.
FreeRTOS/src/%.c: $(FREERTOS_SRC)/%.c
	mkdir -p $(dir $@)
	cp $< $@

$(eval $(call library,FREERTOS))

# vim: set ft=make noet ts=2:

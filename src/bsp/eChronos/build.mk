# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# build.mk --- Build rules for FreeRTOS.
#
# Copyright (C) 2012, Galois, Inc.
# All Rights Reserved.
#
# Written by James Bielman <jamesjb@galois.com>, December 07, 2012
# Modified for eChronos by Alexander Kroh <alex.kroh@nicta.com.au>, November 13, 2013

LIBECHRONOS_LIB    := libeChronos.a


LIBECHRONOS_INCLUDES += -I$(TOP)/src/standalone_apahrs
LIBECHRONOS_INCLUDES += -I$(TOP)/src/bsp/eChronos/include
LIBECHRONOS_INCLUDES += -I$(TOP)/src/bsp/include
LIBECHRONOS_CFLAGS   += $(LIBECHRONOS_INCLUDES)

LIBECHRONOS_OBJECTS += support/mutex.o
LIBECHRONOS_OBJECTS += support/port.o
LIBECHRONOS_OBJECTS += support/task.o
LIBECHRONOS_OBJECTS += support/queue.o
LIBECHRONOS_OBJECTS += support/syscalls.o
LIBECHRONOS_OBJECTS += support/echronos_wrapper.o

LIBECHRONOS_OBJECTS += support/gpio.o
LIBECHRONOS_OBJECTS += support/fault.o
LIBECHRONOS_OBJECTS += support/led.o
LIBECHRONOS_OBJECTS += support/rcc.o

$(eval $(call when_os,echronos,library,LIBECHRONOS))

# vim: set ft=make noet ts=2:

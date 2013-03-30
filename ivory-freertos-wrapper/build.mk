# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# build.mk --- Build rules for the smavlink library.
#
# Copyright (C) 2013, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#
# Written by Pat Hickey <pat@galois.com>, 26 Feb 2013
#

IVORY_FREERTOS_WRAPPER_LIB       := libivoryfreertoswrapper.a

IVORY_FREERTOS_WRAPPER_INCLUDES  += -I$(TOP)/ivory-freertos-wrapper/include

IVORY_FREERTOS_WRAPPER_CFLAGS    += $(IVORY_FREERTOS_WRAPPER_INCLUDES)
IVORY_FREERTOS_WRAPPER_CFLAGS    += $(FREERTOS_CFLAGS)

IVORY_FREERTOS_WRAPPER_OBJECTS :=    \
  src/freertos_semaphore_wrapper.o \
  src/freertos_queue_wrapper.o \
  src/freertos_task_wrapper.o

$(eval $(call library,IVORY_FREERTOS_WRAPPER))

# vim: set ft=make noet ts=2:

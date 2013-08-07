# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# build.mk --- Build rules for stabilize app
#
# Copyright (C) 2012, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#
# Written by Pat Hickey <pat@galois.com>, January 08, 2013
#

SP_STABILIZE_IMG          := stabilize

SP_STABILIZE_OBJECTS      := main.o

SP_STABILIZE_INCLUDES      = $(SMACCMPILOT_INCLUDES)
SP_STABILIZE_INCLUDES     += $(FREERTOS_INCLUDES)
SP_STABILIZE_INCLUDES     += $(ARDUPILOT_LIBINCLUDES)
SP_STABILIZE_INCLUDES     += -I$(TOP)/src/flight/include
SP_STABILIZE_INCLUDES     += -I$(TOP)/src/bsp/hwf4/include
SP_STABILIZE_INCLUDES     += -I$(GEN_DIR)/src/flight
SP_STABILIZE_INCLUDES     += -I$(GEN_DIR)/src/flight/flight

SP_STABILIZE_CFLAGS        = $(SP_STABILIZE_INCLUDES)
SP_STABILIZE_CXXFLAGS      = $(SP_STABILIZE_INCLUDES)

SP_STABILIZE_LIBRARIES    += libflight.a
SP_STABILIZE_LIBRARIES    += libardupilot.a
SP_STABILIZE_LIBRARIES    += libhwf4.a
SP_STABILIZE_LIBRARIES    += libstm32_usb.a
SP_STABILIZE_LIBRARIES    += libFreeRTOS.a

SP_STABILIZE_LIBS         += -lm

$(eval $(call when_os,freertos,image,SP_STABILIZE))

# vim: set ft=make noet ts=2:

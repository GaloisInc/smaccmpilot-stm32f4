# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# build.mk --- Build rules for stabilize
#
# Copyright (C) 2012, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#
# Written by Pat Hickey <pat@galois.com>, January 08, 2013
#

SP_STABILIZE_IMG      := stabilize
SP_STABILIZE_OBJECTS  := main.o
SP_STABILIZE_OBJECTS  += sensors.o
SP_STABILIZE_OBJECTS  += userinput.o
SP_STABILIZE_OBJECTS  += motorsoutput.o
SP_STABILIZE_OBJECTS  += apmotors_wrapper.o
SP_STABILIZE_OBJECTS  += gcs.o

SP_STABILIZE_CXXFLAGS  = $(ARDUPILOT_CXXFLAGS)
SP_STABILIZE_CXXFLAGS += -I$(TOP)/smavlink/include
SP_STABILIZE_CXXFLAGS += -I$(TOP)/../dsl/ivory-lang-c/runtime


SP_STABILIZE_LIBRARIES += libsmavlink.a
SP_STABILIZE_LIBRARIES += libardupilot.a
SP_STABILIZE_LIBRARIES += libhwf4.a
SP_STABILIZE_LIBRARIES += libstm32_usb.a
SP_STABILIZE_LIBRARIES += libFreeRTOS.a

SP_STABILIZE_LIBS += -lm

ifdef CONFIG_ARDUPILOT_PREFIX
$(eval $(call image,SP_STABILIZE))
endif

# vim: set ft=make noet ts=2:

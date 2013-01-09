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

APP_STABILIZE_IMG      := stabilize
APP_STABILIZE_OBJECTS  := main.o
APP_STABILIZE_OBJECTS  += sensors.o
APP_STABILIZE_OBJECTS  += userinput.o

APP_STABILIZE_CXXFLAGS = $(ARDUPILOT_CXXFLAGS)

APP_STABILIZE_LIBRARIES += libardupilot.a
APP_STABILIZE_LIBRARIES += libhwf4.a
APP_STABILIZE_LIBRARIES += libstm32_usb.a
APP_STABILIZE_LIBRARIES += libFreeRTOS.a

APP_STABILIZE_LIBS += -lm

ifdef CONFIG_ARDUPILOT_PREFIX
$(eval $(call image,APP_STABILIZE))
endif

# vim: set ft=make noet ts=2:

# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# build.mk --- Build rules for stabilize_hil
#
# Copyright (C) 2012, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#
# Written by Pat Hickey <pat@galois.com>, January 08, 2013
#

APP_STABILIZE_HIL_IMG      := stabilize_hil
APP_STABILIZE_HIL_OBJECTS  := main.o
APP_STABILIZE_HIL_OBJECTS  += gcs.o
APP_STABILIZE_HIL_OBJECTS  += userinput.o
APP_STABILIZE_HIL_OBJECTS  += motorsoutput.o
APP_STABILIZE_HIL_OBJECTS  += apmotors_wrapper.o

APP_STABILIZE_HIL_CXXFLAGS = $(ARDUPILOT_CXXFLAGS)

APP_STABILIZE_HIL_LIBRARIES += libardupilot.a
APP_STABILIZE_HIL_LIBRARIES += libhwf4.a
APP_STABILIZE_HIL_LIBRARIES += libstm32_usb.a
APP_STABILIZE_HIL_LIBRARIES += libFreeRTOS.a

APP_STABILIZE_HIL_LIBS += -lm

ifdef CONFIG_ARDUPILOT_PREFIX
$(eval $(call image,APP_STABILIZE_HIL))
endif

# vim: set ft=make noet ts=2:

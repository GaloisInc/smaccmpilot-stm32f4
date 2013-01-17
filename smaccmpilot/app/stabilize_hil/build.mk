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

SP_STABILIZE_HIL_IMG      := stabilize_hil
SP_STABILIZE_HIL_OBJECTS  := main.o
SP_STABILIZE_HIL_OBJECTS  += gcs.o

SP_STABILIZE_HIL_INCLUDES  = $(SMACCMPILOT_INCLUDES)
SP_STABILIZE_HIL_INCLUDES += $(SMAVLINK_INCLUDES)

SP_STABILIZE_HIL_CFLAGS     = $(SP_STABILIZE_HIL_INCLUDES)
SP_STABILIZE_HIL_CXXFLAGS   = $(SP_STABILIZE_HIL_INCLUDES)

SP_STABILIZE_HIL_LIBRARIES += libsmaccmpilot.a
SP_STABILIZE_HIL_LIBRARIES += libsmavlink.a
SP_STABILIZE_HIL_LIBRARIES += libardupilot.a
SP_STABILIZE_HIL_LIBRARIES += libhwf4.a
SP_STABILIZE_HIL_LIBRARIES += libstm32_usb.a
SP_STABILIZE_HIL_LIBRARIES += libFreeRTOS.a

SP_STABILIZE_HIL_LIBS += -lm

ifdef CONFIG_ARDUPILOT_PREFIX
$(eval $(call image,SP_STABILIZE_HIL))
endif

# vim: set ft=make noet ts=2:

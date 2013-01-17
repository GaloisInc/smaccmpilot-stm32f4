# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# build.mk --- Build rules for the smaccmpilot library.
#
# Copyright (C) 2012, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#
# Written by Pat Hickey <pat@galois.com>, 17 Jan 2013
#

SMACCMPILOT_LIB     := libsmaccmpilot.a

SMACCMPILOT_INCLUDES  += -I$(TOP)/smaccmpilot/include
SMACCMPILOT_INCLUDES  += $(HWF4_INCLUDES)
SMACCMPILOT_INCLUDES  += $(ARDUPILOT_LIBINCLUDES)
SMACCMPILOT_INCLUDES  += $(IVORYRUNTIME_INCLUDES)
SMACCMPILOT_INCLUDES  += $(FREERTOS_CFLAGS)

SMACCMPILOT_CFLAGS    += $(SMACCMPILOT_INCLUDES)
SMACCMPILOT_CXXFLAGS  += $(SMACCMPILOT_INCLUDES)

SMACCMPILOT_OBJECTS :=       \
  src/apmotors_wrapper.o     \
  src/motorsoutput.o         \
  src/sensors.o              \
  src/userinput.o            \
  src/stabilize.o            \
  src/ivory/pid_stabilize.o

$(eval $(call library,SMACCMPILOT))

# vim: set ft=make noet ts=2:

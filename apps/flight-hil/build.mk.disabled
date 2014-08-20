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

IVORY_PKG_FLIGHT_HIL_GEN_SYMS    := true

$(eval $(call when_platforms,px4fmu17_ioar_freertos px4fmu17_bare_freertos \
				,tower_pkg,IVORY_PKG_FLIGHT_HIL,flight-hil-gen))

FLIGHT_HIL_IMG       := flight-hil

FLIGHT_HIL_INCLUDES  += $(HWF4_INCLUDES)
FLIGHT_HIL_INCLUDES  += -I$(TOP)/src/standalone_apahrs
FLIGHT_HIL_INCLUDES  += -I$(TOP)/src/apwrapper/include
FLIGHT_HIL_INCLUDES  += $(FREERTOS_CFLAGS)
FLIGHT_HIL_INCLUDES  += $(IVORY_PKG_FLIGHT_HIL_CFLAGS)

# For the cryto lib
FLIGHT_HIL_INCLUDES  += -I$(TOP)/src/crypto/include
FLIGHT_HIL_INCLUDES  += -DARM

FLIGHT_HIL_CFLAGS     += $(FLIGHT_HIL_IVORY_FLAG)
FLIGHT_HIL_CFLAGS     += $(FLIGHT_HIL_INCLUDES)

FLIGHT_HIL_CXXFLAGS   += $(FLIGHT_HIL_INCLUDES)
FLIGHT_HIL_CXXFLAGS   += $(FLIGHT_HIL_IVORY_FLAG)
FLIGHT_HIL_CXXFLAGS   += -Wno-psabi

FLIGHT_HIL_OBJECTS := main.o

FLIGHT_HIL_REAL_OBJECTS += $(IVORY_PKG_FLIGHT_HIL_OBJECTS)

FLIGHT_HIL_LIBRARIES    += libapwrapper.a
FLIGHT_HIL_LIBRARIES    += libstandalone-aphal.a
FLIGHT_HIL_LIBRARIES    += libstandalone-apahrs.a
FLIGHT_HIL_LIBRARIES    += libFreeRTOS.a
FLIGHT_HIL_LIBRARIES    += commsec.a

FLIGHT_HIL_LIBS         += -lm

$(eval $(call when_platforms,px4fmu17_bare_freertos px4fmu17_ioar_freertos \
				,image,FLIGHT_HIL))


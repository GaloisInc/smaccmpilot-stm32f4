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

FLIGHT_HIL_CFLAGS    += $(FREERTOS_CFLAGS)
FLIGHT_HIL_CFLAGS    += $(FLIGHT_HIL_INCLUDES)
FLIGHT_HIL_CFLAGS    += -DIVORY_DEPLOY
FLIGHT_HIL_CFLAGS    += $(IVORY_PKG_FLIGHT_HIL_CFLAGS)

FLIGHT_HIL_OBJECTS := main.o

FLIGHT_HIL_REAL_OBJECTS += $(IVORY_PKG_FLIGHT_HIL_OBJECTS)

FLIGHT_HIL_LIBRARIES    += libFreeRTOS.a
FLIGHT_HIL_LIBS         += -lm

$(eval $(call when_platforms,px4fmu17_bare_freertos px4fmu17_ioar_freertos \
				,image,FLIGHT_HIL))


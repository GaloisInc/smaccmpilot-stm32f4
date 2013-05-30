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

FLIGHT_SUPPORT_LIB     := libflight-support.a

FLIGHT_SUPPORT_INCLUDES  += -I$(TOP)/src/flight-support/include
FLIGHT_SUPPORT_INCLUDES  += -I$(TOP)/generated/flight-generated/include
FLIGHT_SUPPORT_INCLUDES  += $(HWF4_INCLUDES)
FLIGHT_SUPPORT_INCLUDES  += $(ARDUPILOT_LIBINCLUDES)
FLIGHT_SUPPORT_INCLUDES  += $(FREERTOS_CFLAGS)

FLIGHT_SUPPORT_CFLAGS    += $(FLIGHT_SUPPORT_INCLUDES)

# Allow overriding the GCS UART from Config.mk:
ifdef CONFIG_GCS_UART
FLIGHT_SUPPORT_CFLAGS    += -DCONFIG_GCS_UART=$(CONFIG_GCS_UART)
endif

# Ignore ASSERTS() REQUIRES() in the source (used by CBMC).
FLIGHT_SUPPORT_CFLAGS += -DIVORY_DEPLOY

FLIGHT_SUPPORT_CXXFLAGS  += $(FLIGHT_SUPPORT_INCLUDES)

FLIGHT_SUPPORT_OBJECTS := $(addprefix src/,\
	apmotors_wrapper.o \
	console_prim.o \
	ivory_string_prim.o \
	sensors_capture.o \
	userinput_capture.o \
	)

$(eval $(call library,FLIGHT_SUPPORT))

# vim: set ft=make noet ts=2:

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

include flight-generated/dep.mk

FLIGHT_GENERATED_LIB     := libflight-generated.a

FLIGHT_GENERATED_INCLUDES  += -I$(TOP)/flight-generated/include/flight-generated
FLIGHT_GENERATED_INCLUDES  += -I$(TOP)/flight-generated/include
FLIGHT_GENERATED_INCLUDES  += -I$(TOP)/flight-support/include
FLIGHT_GENERATED_INCLUDES  += $(HWF4_INCLUDES)
FLIGHT_GENERATED_INCLUDES  += $(ARDUPILOT_LIBINCLUDES)
FLIGHT_GENERATED_INCLUDES  += $(FREERTOS_CFLAGS)
FLIGHT_GENERATED_INCLUDES  += -I$(TOP)/ivory-freertos-wrapper/include

FLIGHT_GENERATED_CFLAGS    += $(FLIGHT_GENERATED_INCLUDES)

# Ignore ASSERTS() REQUIRES() in the source (used by CBMC).
FLIGHT_GENERATED_CFLAGS += -DIVORY_DEPLOY

FLIGHT_GENERATED_CXXFLAGS  += $(SMACCMPILOT_INCLUDES)

#eliminate local prefix before using in a library macro
FLIGHT_GENERATED_OBJECTS = $(subst flight-generated/,,$(FLIGHT_GENERATED_SOURCES:.c=.o))

$(eval $(call library,FLIGHT_GENERATED))

# vim: set ft=make noet ts=2:

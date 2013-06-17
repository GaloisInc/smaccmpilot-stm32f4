# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# build.mk --- Build rules for the smaccmpilot library.
#
# Copyright (C) 2012, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.

include apps/sample-rtv-task/generated/dep.mk

RTV_GENERATED_LIB     := librtvtest-generated.a

RTV_GENERATED_INCLUDES  += \
  -I$(TOP)/apps/sample-rtv-task/generated/include/generated
RTV_GENERATED_INCLUDES  += -I$(TOP)/apps/sample-rtv-task/checker
RTV_GENERATED_INCLUDES  += -I$(TOP)/apps/sample-rtv-task/legacy
RTV_GENERATED_INCLUDES  += $(HWF4_INCLUDES)
RTV_GENERATED_INCLUDES  += $(FREERTOS_CFLAGS)

RTV_GENERATED_CFLAGS    += $(RTV_GENERATED_INCLUDES)

# Ignore ASSERTS() REQUIRES() in the source (used by CBMC).
RTV_GENERATED_CFLAGS += -DIVORY_DEPLOY

RTV_GENERATED_CXXFLAGS  += $(RTV_INCLUDES)

#eliminate local prefix before using in a library macro
RTV_GENERATED_OBJECTS = \
  $(subst apps/sample-rtv-task/generated/,,$(RTV_GENERATED_SOURCES:.c=.o))

$(eval $(call library,RTV_GENERATED))

# vim: set ft=make noet ts=2:

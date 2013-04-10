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

include tower-test/generated/dep.mk

TWRTEST_GENERATED_LIB     := libtwrtest-generated.a

TWRTEST_GENERATED_INCLUDES  += -I$(TOP)/tower-test/generated/include/generated
TWRTEST_GENERATED_INCLUDES  += $(HWF4_INCLUDES)
TWRTEST_GENERATED_INCLUDES  += $(FREERTOS_CFLAGS)
TWRTEST_GENERATED_INCLUDES  += -I$(TOP)/ivory-runtime
TWRTEST_GENERATED_INCLUDES  += -I$(TOP)/ivory-freertos-wrapper/include

TWRTEST_GENERATED_CFLAGS    += $(TWRTEST_GENERATED_INCLUDES)

# Ignore ASSERTS() REQUIRES() in the source (used by CBMC).
TWRTEST_GENERATED_CFLAGS += -DIVORY_DEPLOY

TWRTEST_GENERATED_CXXFLAGS  += $(TWRTEST_INCLUDES)

#eliminate local prefix before using in a library macro
TWRTEST_GENERATED_OBJECTS = $(subst tower-test/generated/,,$(TWRTEST_GENERATED_SOURCES:.c=.o))

$(eval $(call library,TWRTEST_GENERATED))

# vim: set ft=make noet ts=2:

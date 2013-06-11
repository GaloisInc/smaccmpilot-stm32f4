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

include apps/bsp-uart-test/generated/dep.mk

BSPUARTTEST_GENERATED_LIB     := libbspuarttest-generated.a

BSPUARTTEST_GENERATED_INCLUDES  += -I$(TOP)/apps/bsp-uart-test/generated/include/generated
BSPUARTTEST_GENERATED_INCLUDES  += $(HWF4_INCLUDES)
BSPUARTTEST_GENERATED_INCLUDES  += $(FREERTOS_CFLAGS)

BSPUARTTEST_GENERATED_CFLAGS    += $(BSPUARTTEST_GENERATED_INCLUDES)

# Ignore ASSERTS() REQUIRES() in the source (used by CBMC).
BSPUARTTEST_GENERATED_CFLAGS += -DIVORY_DEPLOY

BSPUARTTEST_GENERATED_CXXFLAGS  += $(BSPUARTTEST_INCLUDES)

#eliminate local prefix before using in a library macro
BSPUARTTEST_GENERATED_OBJECTS = $(subst apps/bsp-uart-test/generated/,,$(BSPUARTTEST_GENERATED_SOURCES:.c=.o))

$(eval $(call library,BSPUARTTEST_GENERATED))

# vim: set ft=make noet ts=2:

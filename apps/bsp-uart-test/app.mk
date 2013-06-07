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

APP_BSPUARTTEST_IMG          := bsp-uart-test

APP_BSPUARTTEST_OBJECTS      := main.o

APP_BSPUARTTEST_INCLUDES     += -I$(TOP)/apps/bsp-uart-test/generated/include/generated
APP_BSPUARTTEST_INCLUDES     += -I$(TOP)/apps/bsp-uart-test/generated/include
APP_BSPUARTTEST_INCLUDES     += $(FREERTOS_INCLUDES)

APP_BSPUARTTEST_CFLAGS        = $(APP_BSPUARTTEST_INCLUDES)
APP_BSPUARTTEST_CFLAGS       += -DIVORY_DEPLOY
APP_BSPUARTTEST_CXXFLAGS      = $(APP_BSPUARTTEST_INCLUDES)

APP_BSPUARTTEST_LIBRARIES    += libbspuarttest-generated.a
APP_BSPUARTTEST_LIBRARIES    += libFreeRTOS.a

APP_BSPUARTTEST_LIBS         += -lm

$(eval $(call image,APP_BSPUARTTEST))

# vim: set ft=make noet ts=2:

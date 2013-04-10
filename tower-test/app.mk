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

APP_TWRTEST_IMG          := tower-test

APP_TWRTEST_OBJECTS      := main.o

APP_TWRTEST_INCLUDES     += -I$(TOP)/tower-test/generated/include
APP_TWRTEST_INCLUDES     += $(FREERTOS_INCLUDES)
APP_TWRTEST_INCLUDES     += -I$(TOP)/ivory-freertos-wrapper/include
APP_TWRTEST_INCLUDES     += -I$(TOP)/ivory-runtime/
APP_TWRTEST_INCLUDES     += -I$(TOP)/bsp/hwf4/include

APP_TWRTEST_CFLAGS        = $(APP_TWRTEST_INCLUDES)
APP_TWRTEST_CXXFLAGS      = $(APP_TWRTEST_INCLUDES)

APP_TWRTEST_LIBRARIES    += libtwrtest-generated.a
APP_TWRTEST_LIBRARIES    += libhwf4.a
APP_TWRTEST_LIBRARIES    += libstm32_usb.a
APP_TWRTEST_LIBRARIES    += libivoryfreertoswrapper.a
APP_TWRTEST_LIBRARIES    += libFreeRTOS.a

APP_TWRTEST_LIBS         += -lm

$(eval $(call image,APP_TWRTEST))

# vim: set ft=make noet ts=2:

# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# build.mk
#
# Copyright (C) 2012, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#
# Written by Pat Hickey <pat@galois.com>, January 08, 2013
#

$(eval $(call tower_pkg,IVORY_PKG_TOWER_TEST,tower-example-simple))

APP_TWRTEST_IMG          := tower-test

APP_TWRTEST_OBJECTS      := main.o
APP_TWRTEST_REAL_OBJECTS += $(IVORY_PKG_TOWER_TEST_OBJECTS)

APP_TWRTEST_INCLUDES     += $(FREERTOS_INCLUDES)
APP_TWRTEST_INCLUDES     += -I$(TOP)/src/bsp/hwf4/include

APP_TWRTEST_CFLAGS        = $(APP_TWRTEST_INCLUDES)
APP_TWRTEST_CFLAGS       += -DIVORY_DEPLOY
APP_TWRTEST_CFLAGS       += $(IVORY_PKG_TOWER_TEST_CFLAGS)
APP_TWRTEST_CXXFLAGS      = $(APP_TWRTEST_INCLUDES)
APP_TWRTEST_CXXFLAGS     += $(IVORY_PKG_TOWER_TEST_CFLAGS)

APP_TWRTEST_LIBRARIES    += libhwf4.a
APP_TWRTEST_LIBRARIES    += libstm32_usb.a
APP_TWRTEST_LIBRARIES    += libFreeRTOS.a

APP_TWRTEST_LIBS         += -lm

$(eval $(call cbmc_pkg,APP_TWRTEST,IVORY_PKG_TOWER_TEST))

$(eval $(call when_os,freertos,image,APP_TWRTEST))

# vim: set ft=make noet ts=2:

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

$(eval $(call tower_pkg,IVORY_PKG_TOWER_SIGNALS_TEST,tower-example-signals))

APP_TOWER_SIGNALS_TEST_IMG          := tower-signals-test

APP_TOWER_SIGNALS_TEST_OBJECTS      := main.o
APP_TOWER_SIGNALS_TEST_REAL_OBJECTS += $(IVORY_PKG_TOWER_SIGNALS_TEST_OBJECTS)

APP_TOWER_SIGNALS_TEST_INCLUDES     += $(FREERTOS_INCLUDES)
APP_TOWER_SIGNALS_TEST_INCLUDES     += $(IVORY_PKG_TOWER_SIGNALS_TEST_CFLAGS)

APP_TOWER_SIGNALS_TEST_CFLAGS        = $(APP_TOWER_SIGNALS_TEST_INCLUDES)
APP_TOWER_SIGNALS_TEST_CFLAGS       += -DIVORY_DEPLOY
APP_TOWER_SIGNALS_TEST_CFLAGS       += $(IVORY_PKG_TOWER_SIGNALS_TEST_CFLAGS)
APP_TOWER_SIGNALS_TEST_CXXFLAGS      = $(APP_TOWER_SIGNALS_TEST_INCLUDES)
APP_TOWER_SIGNALS_TEST_CXXFLAGS     += $(IVORY_PKG_TOWER_SIGNALS_TEST_CFLAGS)

APP_TOWER_SIGNALS_TEST_LIBRARIES    += libhwf4.a
APP_TOWER_SIGNALS_TEST_LIBRARIES    += libstm32_usb.a
APP_TOWER_SIGNALS_TEST_LIBRARIES    += libFreeRTOS.a

APP_TOWER_SIGNALS_TEST_LIBS         += -lm

$(eval $(call when_os,freertos,cbmc_pkg,APP_TOWER_SIGNALS_TEST,IVORY_PKG_TOWER_SIGNALS_TEST))

$(eval $(call when_os,freertos,image,APP_TOWER_SIGNALS_TEST))

LIB_TOWER_SIGNALS_TEST_LIB          := libtower-signals-test.a
LIB_TOWER_SIGNALS_TEST_REAL_OBJECTS := $(call filteroutstring,tower_signal_comm_, \
                                        $(call filteroutstring,tower_task_loop_, \
                                         $(IVORY_PKG_TOWER_SIGNALS_TEST_OBJECTS)))
LIB_TOWER_SIGNALS_TEST_CFLAGS       := $(IVORY_PKG_TOWER_SIGNALS_TEST_CFLAGS)
LIB_TOWER_SIGNALS_TEST_CFLAGS       += -DIVORY_DEPLOY

$(eval $(call when_os,aadl,library,LIB_TOWER_SIGNALS_TEST))

# vim: set ft=make noet ts=2:

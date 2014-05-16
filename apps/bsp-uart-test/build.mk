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

$(eval $(call tower_pkg,IVORY_PKG_UART_TEST,bsp-uart-tower-test-gen))

APP_BSPUARTTEST_IMG          := bsp-uart-test

APP_BSPUARTTEST_LIBRARIES    += libFreeRTOS.a
APP_BSPUARTTEST_INCLUDES     += $(FREERTOS_CFLAGS)

APP_BSPUARTTEST_REAL_OBJECTS += $(IVORY_PKG_UART_TEST_OBJECTS)
APP_BSPUARTTEST_LIBS         += -lm

#APP_BSPUARTTEST_INCLUDES     += -I$(TOP)/apps/bsp-uart-test
APP_BSPUARTTEST_INCLUDES     += -I$(TOP)/src/bsp/include
APP_BSPUARTTEST_INCLUDES     += $(IVORY_PKG_UART_TEST_CFLAGS)

APP_BSPUARTTEST_CFLAGS       += -O2 $(APP_BSPUARTTEST_INCLUDES)
APP_BSPUARTTEST_DISABLE_GLOBAL_STARTUP_OBJECTS := 1

$(eval $(call cbmc_pkg,APP_BSPUARTTEST,IVORY_PKG_UART_TEST))

$(eval $(call when_os,freertos,image,APP_BSPUARTTEST))
$(eval $(call when_os,echronos,echronos_gen,APP_BSPUARTTEST))
$(eval $(call when_os,echronos,image,APP_BSPUARTTEST))

# ------------------------------------------------------------------------------
# AADL Build
# ------------------------------------------------------------------------------

LIB_BSPUARTTEST_LIB          := libbspuarttest.a
LIB_BSPUARTTEST_REAL_OBJECTS += $(call filteroutstring,tower_task_loop_, \
                                      $(IVORY_PKG_UART_TEST_OBJECTS))
LIB_BSPUARTTEST_CFLAGS       += $(LIB_BSPUARTTEST_INCLUDES)
LIB_BSPUARTTEST_CFLAGS       += $(IVORY_PKG_UART_TEST_CFLAGS)

$(eval $(call when_os,aadl,library,LIB_BSPUARTTEST))

# vim: set ft=make noet ts=2:

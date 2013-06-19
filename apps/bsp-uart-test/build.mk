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

$(eval $(call ivory_pkg,IVORY_PKG_UART_TEST,bsp-uart-tower-test-gen))

APP_BSPUARTTEST_IMG          := bsp-uart-test
APP_BSPUARTTEST_OBJECTS      := main.o
APP_BSPUARTTEST_REAL_OBJECTS += $(IVORY_PKG_UART_TEST_OBJECTS)
APP_BSPUARTTEST_LIBRARIES    += libFreeRTOS.a
APP_BSPUARTTEST_LIBS         += -lm

APP_BSPUARTTEST_CFLAGS       += $(FREERTOS_INCLUDES)
APP_BSPUARTTEST_CFLAGS       += -DIVORY_DEPLOY
APP_BSPUARTTEST_CFLAGS       += $(IVORY_PKG_UART_TEST_CFLAGS)

$(eval $(call image,APP_BSPUARTTEST))

# vim: set ft=make noet ts=2:

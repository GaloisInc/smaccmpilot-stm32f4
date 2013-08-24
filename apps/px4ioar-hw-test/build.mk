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

$(eval $(call tower_pkg,IVORY_PKG_PX4IOAR_HW_TEST,px4ioar-hw-test-gen))

APP_PX4IOAR_HW_TEST_IMG          := px4ioar-hw-test
APP_PX4IOAR_HW_TEST_OBJECTS      := main.o
APP_PX4IOAR_HW_TEST_REAL_OBJECTS += $(IVORY_PKG_PX4IOAR_HW_TEST_OBJECTS)
APP_PX4IOAR_HW_TEST_LIBRARIES    += libFreeRTOS.a
APP_PX4IOAR_HW_TEST_LIBS         += -lm

APP_PX4IOAR_HW_TEST_INCLUDES     += $(FREERTOS_CFLAGS)
APP_PX4IOAR_HW_TEST_INCLUDES     += -I$(TOP)/src/bsp/include
APP_PX4IOAR_HW_TEST_INCLUDES     += $(IVORY_PKG_PX4IOAR_HW_TEST_CFLAGS)

APP_PX4IOAR_HW_TEST_CFLAGS       += -O2 $(APP_PX4IOAR_HW_TEST_INCLUDES)
APP_PX4IOAR_HW_TEST_CFLAGS       += -DIVORY_DEPLOY

$(eval $(call cbmc_pkg,APP_PX4IOAR_HW_TEST,IVORY_PKG_PX4IOAR_HW_TEST))

$(eval $(call when_os,freertos,image,APP_PX4IOAR_HW_TEST))

# vim: set ft=make noet ts=2:

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

$(eval $(call when_platforms,px4fmu17_bare_freertos px4fmu17_ioar_freertos \
				,tower_pkg,IVORY_PKG_COPTER_MOTOR_HW_TEST,copter-motor-test-gen))

APP_COPTER_MOTOR_HW_TEST_IMG          := copter-motor-test
APP_COPTER_MOTOR_HW_TEST_OBJECTS      := main.o
APP_COPTER_MOTOR_HW_TEST_REAL_OBJECTS += $(IVORY_PKG_COPTER_MOTOR_HW_TEST_OBJECTS)
APP_COPTER_MOTOR_HW_TEST_LIBRARIES    += libFreeRTOS.a
APP_COPTER_MOTOR_HW_TEST_LIBS         += -lm

APP_COPTER_MOTOR_HW_TEST_INCLUDES     += $(FREERTOS_CFLAGS)
APP_COPTER_MOTOR_HW_TEST_INCLUDES     += -I$(TOP)/src/bsp/include
APP_COPTER_MOTOR_HW_TEST_INCLUDES     += $(IVORY_PKG_COPTER_MOTOR_HW_TEST_CFLAGS)

APP_COPTER_MOTOR_HW_TEST_CFLAGS       += -O2 $(APP_COPTER_MOTOR_HW_TEST_INCLUDES)
APP_COPTER_MOTOR_HW_TEST_CFLAGS       += -DIVORY_DEPLOY

$(eval $(call when_platforms,px4fmu17_bare_freertos px4fmu17_ioar_freertos \
				,cbmc_pkg,APP_COPTER_MOTOR_HW_TEST,IVORY_PKG_COPTER_MOTOR_HW_TEST))

$(eval $(call when_platforms,px4fmu17_bare_freertos px4fmu17_ioar_freertos \
				,image,APP_COPTER_MOTOR_HW_TEST))

# vim: set ft=make noet ts=2:

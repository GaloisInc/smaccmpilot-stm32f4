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

$(eval $(call when_platforms,px4fmu17_bare_freertos \
			     px4fmu17_ioar_freertos \
				,tower_pkg,IVORY_PKG_COPTER_MOTOR_TEST,copter-motor-test-gen))

COPTER_MOTOR_TEST_IMG          := copter-motor-test

COPTER_MOTOR_TEST_LIBRARIES    += libFreeRTOS.a
COPTER_MOTOR_TEST_INCLUDES     += $(FREERTOS_CFLAGS)

COPTER_MOTOR_TEST_REAL_OBJECTS += $(IVORY_PKG_COPTER_MOTOR_TEST_OBJECTS)
COPTER_MOTOR_TEST_LIBS         += -lm

COPTER_MOTOR_TEST_INCLUDES     += $(IVORY_PKG_COPTER_MOTOR_TEST_CFLAGS)

COPTER_MOTOR_TEST_CFLAGS       += $(COPTER_MOTOR_TEST_INCLUDES)
COPTER_MOTOR_TEST_DISABLE_GLOBAL_STARTUP_OBJECTS := 1

$(eval $(call when_platforms,px4fmu17_bare_freertos \
			     px4fmu17_ioar_freertos \
				,cbmc_pkg,COPTER_MOTOR_TEST,IVORY_PKG_COPTER_MOTOR_TEST))

$(eval $(call when_platforms,px4fmu17_bare_freertos \
			     px4fmu17_ioar_freertos \
				,image,COPTER_MOTOR_TEST))

# vim: set ft=make noet ts=2:

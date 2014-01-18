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
			     px4fmu17_ioar_echronos \
			     px4fmu17_ioar_freertos \
				,tower_pkg,IVORY_PKG_COPTER_MOTOR_HW_TEST,copter-motor-test-gen))

APP_COPTER_MOTOR_HW_TEST_IMG          := copter-motor-test

ifneq ($($(CONFIG_PLATFORM)_TOWER_OS),echronos)
APP_COPTER_MOTOR_HW_TEST_OBJECTS      := freertos/main.o
APP_COPTER_MOTOR_HW_TEST_LIBRARIES    += libFreeRTOS.a
APP_COPTER_MOTOR_HW_TEST_INCLUDES     += $(FREERTOS_CFLAGS)
else
APP_COPTER_MOTOR_HW_TEST_ECHRONOS_PRX := echronos/copter-motor-test.prx
APP_COPTER_MOTOR_HW_TEST_OBJECTS      := echronos/main.o
endif

APP_COPTER_MOTOR_HW_TEST_REAL_OBJECTS += $(IVORY_PKG_COPTER_MOTOR_HW_TEST_OBJECTS)
APP_COPTER_MOTOR_HW_TEST_LIBS         += -lm

APP_COPTER_MOTOR_HW_TEST_INCLUDES     += -I$(TOP)/src/bsp/include
APP_COPTER_MOTOR_HW_TEST_INCLUDES     += $(IVORY_PKG_COPTER_MOTOR_HW_TEST_CFLAGS)

APP_COPTER_MOTOR_HW_TEST_CFLAGS       += -O2 $(APP_COPTER_MOTOR_HW_TEST_INCLUDES)

$(eval $(call when_platforms,px4fmu17_bare_freertos \
			     px4fmu17_ioar_freertos \
			     px4fmu17_ioar_echronos \
				,cbmc_pkg,APP_COPTER_MOTOR_HW_TEST,IVORY_PKG_COPTER_MOTOR_HW_TEST))

$(eval $(call when_os,echronos,echronos_gen,APP_COPTER_MOTOR_HW_TEST))
$(eval $(call when_platforms,px4fmu17_bare_freertos \
			     px4fmu17_ioar_freertos \
			     px4fmu17_ioar_echronos \
				,image,APP_COPTER_MOTOR_HW_TEST))

# vim: set ft=make noet ts=2:

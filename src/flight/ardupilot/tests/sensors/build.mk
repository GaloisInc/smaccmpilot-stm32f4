# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# build.mk --- Build rules for an AP_HAL_SMACCM test.
#
# Copyright (C) 2012, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#
# Written by James Bielman <jamesjb@galois.com>, December 07, 2012
#

AP_HAL_SENSORS_TEST_IMG      := ap_hal_sensors_test
AP_HAL_SENSORS_TEST_OBJECTS  := main.o

AP_HAL_SENSORS_TEST_CFLAGS   += -I$(TOP)/bsp/include
AP_HAL_SENSORS_TEST_CXXFLAGS += $(ARDUPILOT_CXXFLAGS)

AP_HAL_SENSORS_TEST_LIBRARIES += libardupilot.a
AP_HAL_SENSORS_TEST_LIBRARIES += libhwf4.a
AP_HAL_SENSORS_TEST_LIBRARIES += libstm32_usb.a
AP_HAL_SENSORS_TEST_LIBRARIES += libFreeRTOS.a

AP_HAL_SENSORS_TEST_LIBS += -lm

ifdef CONFIG_ARDUPILOT_PREFIX
# $(eval $(call when_os,freertos,image,AP_HAL_SENSORS_TEST))
endif

# vim: set ft=make noet ts=2:

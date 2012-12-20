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

AP_HAL_SCHEDULER_TEST_IMG      := ap_hal_scheduler_test
AP_HAL_SCHEDULER_TEST_OBJECTS  := main.o

AP_HAL_SCHEDULER_TEST_CXXFLAGS += $(FREERTOS_CFLAGS)
AP_HAL_SCHEDULER_TEST_CXXFLAGS += -I$(TOP)/hwf4/include
AP_HAL_SCHEDULER_TEST_CXXFLAGS += -I$(TOP)/include
AP_HAL_SCHEDULER_TEST_CXXFLAGS += -I$(TOP)/stm32_usb/include
AP_HAL_SCHEDULER_TEST_CXXFLAGS += -I$(TOP)/ap_hal/AP_HAL
AP_HAL_SCHEDULER_TEST_CXXFLAGS += -I$(TOP)/ap_hal/AP_HAL_SMACCM
AP_HAL_SCHEDULER_TEST_CXXFLAGS += -I$(TOP)/ap_hal/AP_Progmem
AP_HAL_SCHEDULER_TEST_CXXFLAGS += -DCONFIG_HAL_BOARD=HAL_BOARD_SMACCM
AP_HAL_SCHEDULER_TEST_CXXFLAGS += -Wno-psabi

AP_HAL_SCHEDULER_TEST_LIBRARIES += libAP_HAL_SMACCM.a
AP_HAL_SCHEDULER_TEST_LIBRARIES += libhwf4.a
AP_HAL_SCHEDULER_TEST_LIBRARIES += libstm32_usb.a
AP_HAL_SCHEDULER_TEST_LIBRARIES += libFreeRTOS.a

$(eval $(call image,AP_HAL_SCHEDULER_TEST))

# vim: set ft=make noet ts=2:

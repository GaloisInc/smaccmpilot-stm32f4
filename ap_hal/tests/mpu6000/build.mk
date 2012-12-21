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

AP_HAL_MPU6000_TEST_IMG      := ap_hal_mpu6000_test
AP_HAL_MPU6000_TEST_OBJECTS  := main.o

AP_HAL_MPU6000_TEST_CXXFLAGS = $(AP_HAL_SMACCM_CXXFLAGS)

AP_HAL_MPU6000_TEST_LIBRARIES += libAP_HAL_SMACCM.a
AP_HAL_MPU6000_TEST_LIBRARIES += libhwf4.a
AP_HAL_MPU6000_TEST_LIBRARIES += libstm32_usb.a
AP_HAL_MPU6000_TEST_LIBRARIES += libFreeRTOS.a

AP_HAL_MPU6000_TEST_LIBS += -lm

$(eval $(call image,AP_HAL_MPU6000_TEST))

# vim: set ft=make noet ts=2:

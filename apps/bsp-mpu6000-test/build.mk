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

$(eval $(call tower_pkg,IVORY_PKG_MPU6000_TEST,bsp-mpu6000-tower-test-gen))

BSP_MPU6000_TEST_IMG          := bsp-mpu6000-test

BSP_MPU6000_TEST_OBJECTS      := freertos/main.o
BSP_MPU6000_TEST_LIBRARIES    += libFreeRTOS.a
BSP_MPU6000_TEST_INCLUDES     += $(FREERTOS_CFLAGS)

BSP_MPU6000_TEST_REAL_OBJECTS += $(IVORY_PKG_MPU6000_TEST_OBJECTS)
BSP_MPU6000_TEST_CFLAGS       += $(IVORY_PKG_MPU6000_TEST_CFLAGS)
BSP_MPU6000_TEST_CFLAGS       += $(BSP_MPU6000_TEST_INCLUDES)


$(eval $(call cbmc_pkg,BSP_MPU6000_TEST,IVORY_PKG_MPU6000_TEST))

$(eval $(call when_os,freertos,image,BSP_MPU6000_TEST))
$(eval $(call when_os,echronos,echronos_gen,BSP_MPU6000_TEST))
$(eval $(call when_os,echronos,image,BSP_MPU6000_TEST))

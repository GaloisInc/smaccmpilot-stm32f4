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

$(eval $(call tower_pkg,IVORY_PKG_MPU6K_TEST,px4-mpu6k-test-gen))

PX4_MPU6K_TEST_IMG          := px4-mpu6k-test

PX4_MPU6K_TEST_LIBRARIES    += libFreeRTOS.a
PX4_MPU6K_TEST_INCLUDES     += $(FREERTOS_CFLAGS)

PX4_MPU6K_TEST_REAL_OBJECTS += $(IVORY_PKG_MPU6K_TEST_OBJECTS)
PX4_MPU6K_TEST_CFLAGS       += $(IVORY_PKG_MPU6K_TEST_CFLAGS)
PX4_MPU6K_TEST_CFLAGS       += $(PX4_MPU6K_TEST_INCLUDES)
PX4_MPU6K_TEST_DISABLE_GLOBAL_STARTUP_OBJECTS := 1


$(eval $(call cbmc_pkg,PX4_MPU6K_TEST,IVORY_PKG_MPU6K_TEST))

$(eval $(call when_os,freertos,image,PX4_MPU6K_TEST))
$(eval $(call when_os,echronos,echronos_gen,PX4_MPU6K_TEST))
$(eval $(call when_os,echronos,image,PX4_MPU6K_TEST))

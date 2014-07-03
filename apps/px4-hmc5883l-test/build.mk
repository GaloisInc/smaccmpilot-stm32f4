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

$(eval $(call when_platforms,px4fmu17_ioar_freertos px4fmu17_bare_freertos,\
                tower_pkg,IVORY_PKG_HMC5883L_TEST,px4-hmc5883l-test-gen))

PX4_HMC5883L_TEST_IMG          := px4-hmc5883l-test

PX4_HMC5883L_TEST_LIBRARIES    += libFreeRTOS.a
PX4_HMC5883L_TEST_INCLUDES     += $(FREERTOS_CFLAGS)

PX4_HMC5883L_TEST_REAL_OBJECTS += $(IVORY_PKG_HMC5883L_TEST_OBJECTS)
PX4_HMC5883L_TEST_CFLAGS       += $(IVORY_PKG_HMC5883L_TEST_CFLAGS)
PX4_HMC5883L_TEST_CFLAGS       += $(PX4_HMC5883L_TEST_INCLUDES)
PX4_HMC5883L_TEST_DISABLE_GLOBAL_STARTUP_OBJECTS := 1


$(eval $(call when_platforms,px4fmu17_ioar_freertos px4fmu_bare_freertos,\
                  image,PX4_HMC5883L_TEST))

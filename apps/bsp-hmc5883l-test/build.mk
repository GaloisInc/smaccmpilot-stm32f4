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

$(eval $(call tower_pkg,IVORY_PKG_HMC5883L_TEST,px4-hmc5883l-test-gen))

BSP_HMC5883L_TEST_IMG          := bsp-hmc5883l-test

BSP_HMC5883L_TEST_OBJECTS      := freertos/main.o
BSP_HMC5883L_TEST_LIBRARIES    += libFreeRTOS.a
BSP_HMC5883L_TEST_INCLUDES     += $(FREERTOS_CFLAGS)

BSP_HMC5883L_TEST_REAL_OBJECTS += $(IVORY_PKG_HMC5883L_TEST_OBJECTS)
BSP_HMC5883L_TEST_CFLAGS       += $(IVORY_PKG_HMC5883L_TEST_CFLAGS)
BSP_HMC5883L_TEST_CFLAGS       += $(BSP_HMC5883L_TEST_INCLUDES)


$(eval $(call cbmc_pkg,BSP_HMC5883L_TEST,IVORY_PKG_HMC5883L_TEST))

$(eval $(call when_os,freertos,image,BSP_HMC5883L_TEST))
$(eval $(call when_os,echronos,echronos_gen,BSP_HMC5883L_TEST))
$(eval $(call when_os,echronos,image,BSP_HMC5883L_TEST))

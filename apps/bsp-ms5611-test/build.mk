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

$(eval $(call tower_pkg,IVORY_PKG_MS5611_TEST,px4-ms5611-test-gen))

BSP_MS5611_TEST_IMG          := bsp-ms5611-test

BSP_MS5611_TEST_OBJECTS      := freertos/main.o
BSP_MS5611_TEST_LIBRARIES    += libFreeRTOS.a
BSP_MS5611_TEST_INCLUDES     += $(FREERTOS_CFLAGS)

BSP_MS5611_TEST_REAL_OBJECTS += $(IVORY_PKG_MS5611_TEST_OBJECTS)
BSP_MS5611_TEST_CFLAGS       += $(IVORY_PKG_MS5611_TEST_CFLAGS)
BSP_MS5611_TEST_CFLAGS       += $(BSP_MS5611_TEST_INCLUDES)


$(eval $(call cbmc_pkg,BSP_MS5611_TEST,IVORY_PKG_MS5611_TEST))

$(eval $(call when_os,freertos,image,BSP_MS5611_TEST))
$(eval $(call when_os,echronos,echronos_gen,BSP_MS5611_TEST))
$(eval $(call when_os,echronos,image,BSP_MS5611_TEST))

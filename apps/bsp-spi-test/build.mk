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

$(eval $(call tower_pkg,IVORY_PKG_SPI_TEST,bsp-spi-test-gen))

BSP_SPI_TEST_IMG          := bsp-spi-test

BSP_SPI_TEST_LIBRARIES    += libFreeRTOS.a
BSP_SPI_TEST_INCLUDES     += $(FREERTOS_CFLAGS)

BSP_SPI_TEST_REAL_OBJECTS += $(IVORY_PKG_SPI_TEST_OBJECTS)

BSP_SPI_TEST_INCLUDES     += $(IVORY_PKG_SPI_TEST_CFLAGS)

BSP_SPI_TEST_CFLAGS       += -O2 $(BSP_SPI_TEST_INCLUDES)
BSP_SPI_TEST_DISABLE_GLOBAL_STARTUP_OBJECTS := 1


$(eval $(call cbmc_pkg,BSP_SPI_TEST,IVORY_PKG_SPI_TEST))

$(eval $(call when_os,freertos,image,BSP_SPI_TEST))

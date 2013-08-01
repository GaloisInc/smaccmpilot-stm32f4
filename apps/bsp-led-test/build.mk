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

IVORY_PKG_LED_TEST_GEN_SYMS    := true

$(eval $(call ivory_pkg,IVORY_PKG_LED_TEST,bsp-led-tower-test-gen))

BSP_LED_TEST_IMG          := bsp-led-test
BSP_LED_TEST_OBJECTS      := main.o
BSP_LED_TEST_REAL_OBJECTS += $(IVORY_PKG_LED_TEST_OBJECTS)
BSP_LED_TEST_LIBRARIES    += libFreeRTOS.a

BSP_LED_TEST_INCLUDES     += $(FREERTOS_CFLAGS)
BSP_LED_TEST_INCLUDES     += -I$(TOP)/src/bsp/include
BSP_LED_TEST_INCLUDES     += $(IVORY_PKG_LED_TEST_CFLAGS)
BSP_LED_TEST_CFLAGS       += -DIVORY_DEPLOY
BSP_LED_TEST_CFLAGS       += -O2 $(BSP_LED_TEST_INCLUDES)

BSP_LED_TEST_LIBRARIES    += libFreeRTOS.a

$(eval $(call cbmc_pkg,BSP_LED_TEST,IVORY_PKG_LED_TEST))

$(eval $(call image,BSP_LED_TEST))


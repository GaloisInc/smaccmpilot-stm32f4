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

$(eval $(call tower_pkg,IVORY_PKG_CAN_TEST,bsp-can-test-gen))

BSP_CAN_TEST_IMG          := bsp-can-test

BSP_CAN_TEST_LIBRARIES    += libFreeRTOS.a
BSP_CAN_TEST_INCLUDES     += $(FREERTOS_CFLAGS)

BSP_CAN_TEST_REAL_OBJECTS += $(IVORY_PKG_CAN_TEST_OBJECTS)

BSP_CAN_TEST_INCLUDES     += $(IVORY_PKG_CAN_TEST_CFLAGS)

BSP_CAN_TEST_CFLAGS       += $(BSP_CAN_TEST_INCLUDES)

BSP_CAN_TEST_DISABLE_GLOBAL_STARTUP_OBJECTS := 1


$(eval $(call cbmc_pkg,BSP_CAN_TEST,IVORY_PKG_CAN_TEST))

$(eval $(call when_os,freertos,image,BSP_CAN_TEST))

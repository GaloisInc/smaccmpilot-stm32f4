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
# Written by Pat Hickey <pat@galois.com>, January 08, 2013
#

$(eval $(call tower_pkg,IVORY_PKG_BSP_UART_STRESS_TEST,bsp-uart-stress-test-gen))

BSP_UART_STRESS_TEST_IMG          := bsp-uart-stress-test
BSP_UART_STRESS_TEST_OBJECTS      := freertos/main.o
BSP_UART_STRESS_TEST_LIBRARIES    += libFreeRTOS.a
BSP_UART_STRESS_TEST_INCLUDES     += $(FREERTOS_CFLAGS)

BSP_UART_STRESS_TEST_REAL_OBJECTS += $(IVORY_PKG_BSP_UART_STRESS_TEST_OBJECTS)
BSP_UART_STRESS_TEST_LIBS         += -lm

BSP_UART_STRESS_TEST_INCLUDES     += -I$(TOP)/src/bsp/include
BSP_UART_STRESS_TEST_INCLUDES     += $(IVORY_PKG_BSP_UART_STRESS_TEST_CFLAGS)

BSP_UART_STRESS_TEST_CFLAGS       += -O2 $(BSP_UART_STRESS_TEST_INCLUDES)

$(eval $(call cbmc_pkg,BSP_UART_STRESS_TEST,IVORY_PKG_UART_STRESS_TEST))

$(eval $(call when_os,freertos,image,BSP_UART_STRESS_TEST))
$(eval $(call when_os,echronos,echronos_gen,BSP_UART_STRESS_TEST))
$(eval $(call when_os,echronos,image,BSP_UART_STRESS_TEST))


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

$(eval $(call tower_pkg,IVORY_PKG_BSP_UART_DEBUG,bsp-uart-debug-gen))

BSP_UART_DEBUG_IMG          := bsp-uart-debug
BSP_UART_DEBUG_LIBRARIES    += libFreeRTOS.a
BSP_UART_DEBUG_INCLUDES     += $(FREERTOS_CFLAGS)

BSP_UART_DEBUG_REAL_OBJECTS += $(IVORY_PKG_BSP_UART_DEBUG_OBJECTS)
BSP_UART_DEBUG_LIBS         += -lm

BSP_UART_DEBUG_INCLUDES     += $(IVORY_PKG_BSP_UART_DEBUG_CFLAGS)

BSP_UART_DEBUG_CFLAGS       += -O2 $(BSP_UART_DEBUG_INCLUDES)
BSP_UART_DEBUG_DISABLE_GLOBAL_STARTUP_OBJECTS := 1

$(eval $(call cbmc_pkg,BSP_UART_DEBUG,IVORY_PKG_UART_DEBUG))

$(eval $(call when_os,freertos,image,BSP_UART_DEBUG))
$(eval $(call when_os,echronos,echronos_gen,BSP_UART_DEBUG))
$(eval $(call when_os,echronos,image,BSP_UART_DEBUG))


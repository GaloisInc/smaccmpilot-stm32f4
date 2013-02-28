# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# build.mk --- Build an STM32F4 test program.
#
# Copyright (C) 2012, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#
# Written by James Bielman <jamesjb@galois.com>, December 07, 2012
#

TIMERTEST_IMG       := timertest
TIMERTEST_OBJECTS   := main.o

TIMERTEST_CFLAGS    += -I$(TOP)/bsp/include
TIMERTEST_CFLAGS    += $(FREERTOS_CFLAGS)
TIMERTEST_CFLAGS    += -I$(TOP)/bsp/hwf4/include
TIMERTEST_LIBRARIES := libhwf4.a libstm32_usb.a libFreeRTOS.a

ifeq "$(CONFIG_BOARD)" "px4"
$(eval $(call image,TIMERTEST))
endif
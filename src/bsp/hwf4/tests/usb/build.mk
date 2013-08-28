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

USBTEST_IMG       := hwf4-usbtest
USBTEST_OBJECTS   := main.o

USBTEST_CFLAGS    += -I$(TOP)/src/bsp/include
USBTEST_CFLAGS    += -I$(TOP)/src/bsp/stm32_usb/include
USBTEST_CFLAGS    += $(FREERTOS_CFLAGS)
USBTEST_CFLAGS    += -I$(TOP)/src/bsp/hwf4/include
USBTEST_LIBRARIES := libhwf4.a libstm32_usb.a libFreeRTOS.a

$(eval $(call when_platform,px4fmu17_freertos,image,USBTEST))

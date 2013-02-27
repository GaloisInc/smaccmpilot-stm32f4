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

USBTEST_IMG       := usbtest
USBTEST_OBJECTS   := main.o

USBTEST_CFLAGS    += -I$(TOP)/include
USBTEST_CFLAGS    += -I$(TOP)/stm32_usb/include
USBTEST_CFLAGS    += $(FREERTOS_CFLAGS)
USBTEST_CFLAGS    += -I$(TOP)/hwf4/include
USBTEST_LIBRARIES := libhwf4.a libstm32_usb.a libFreeRTOS.a

$(eval $(call image,USBTEST))

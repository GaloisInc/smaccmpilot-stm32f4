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

USARTTEST_IMG       := usarttest
USARTTEST_OBJECTS   := main.o

USARTTEST_CFLAGS    += $(FREERTOS_CFLAGS)
USARTTEST_CFLAGS    += -I$(TOP)/include
USARTTEST_CFLAGS    += -I$(TOP)/hwf4/include
USARTTEST_LIBRARIES := libhwf4.a libstm32_usb.a libFreeRTOS.a

$(eval $(call image,USARTTEST))

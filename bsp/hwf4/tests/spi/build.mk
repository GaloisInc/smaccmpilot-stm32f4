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

SPITEST_IMG       := spitest
SPITEST_OBJECTS   := main.o

SPITEST_CFLAGS    += $(FREERTOS_CFLAGS)
SPITEST_CFLAGS    += -I$(TOP)/include
SPITEST_CFLAGS    += -I$(TOP)/hwf4/include
SPITEST_LIBRARIES := libhwf4.a libstm32_usb.a libFreeRTOS.a

$(eval $(call image,SPITEST))

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

I2CTEST_IMG       := i2ctest
I2CTEST_OBJECTS   := main.o

I2CTEST_CFLAGS    += -I$(TOP)/src/bsp/include
I2CTEST_CFLAGS    += $(FREERTOS_CFLAGS)
I2CTEST_CFLAGS    += -I$(TOP)/src/bsp/hwf4/include
I2CTEST_LIBRARIES := libhwf4.a libstm32_usb.a libFreeRTOS.a

$(eval $(call image,I2CTEST))

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

EEPROMTEST_IMG       := hwf4-eepromtest
EEPROMTEST_OBJECTS   := main.o

EEPROMTEST_CFLAGS    += $(FREERTOS_CFLAGS)
EEPROMTEST_CFLAGS    += -I$(TOP)/src/bsp/include
EEPROMTEST_CFLAGS    += -I$(TOP)/src/bsp/hwf4/include
EEPROMTEST_LIBRARIES := libhwf4.a libstm32_usb.a libFreeRTOS.a

$(eval $(call when_platforms,px4fmu17_freertos,image,EEPROMTEST))

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

ARDRONE_TEST_IMG       := hwf4-ardrone-test
ARDRONE_TEST_OBJECTS   := main.o

ARDRONE_TEST_CFLAGS    += $(FREERTOS_CFLAGS)
ARDRONE_TEST_CFLAGS    += -I$(TOP)/src/bsp/include
ARDRONE_TEST_CFLAGS    += -I$(TOP)/src/bsp/hwf4/include
ARDRONE_TEST_LIBRARIES := libhwf4.a libstm32_usb.a libFreeRTOS.a

ifeq "$(CONFIG_PLATFORM)" "px4fmu17_freertos"
$(eval $(call image,ARDRONE_TEST))
endif

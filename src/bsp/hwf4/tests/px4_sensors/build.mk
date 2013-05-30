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

PX4_SENSORS_IMG       := px4_sensors
PX4_SENSORS_OBJECTS   := main.o

PX4_SENSORS_CFLAGS    += $(FREERTOS_CFLAGS)
PX4_SENSORS_CFLAGS    += -I$(TOP)/src/bsp/include
PX4_SENSORS_CFLAGS    += -I$(TOP)/src/bsp/hwf4/include
PX4_SENSORS_LIBRARIES := libhwf4.a libstm32_usb.a libFreeRTOS.a

ifeq "$(CONFIG_BOARD)" "px4"
$(eval $(call image,PX4_SENSORS))
endif

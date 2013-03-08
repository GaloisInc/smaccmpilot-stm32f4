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

IVORY_LEDTEST_IMG       := ivory_ledtest
IVORY_LEDTEST_OBJECTS   := ledblink.o ivory_hw.o

IVORY_LEDTEST_INCLUDES  += -I$(TOP)/ivory-runtime
IVORY_LEDTEST_INCLUDES  += $(FREERTOS_CFLAGS)
IVORY_LEDTEST_INCLUDES  += -I$(TOP)/bsp/include
IVORY_LEDTEST_INCLUDES  += -I$(TOP)/../dsl/ivory-hw/include

IVORY_LEDTEST_CFLAGS    += -O2 $(IVORY_LEDTEST_INCLUDES)

#IVORY_LEDTEST_CFLAGS    += -I$(TOP)/bsp/hwf4/include
IVORY_LEDTEST_LIBRARIES := libFreeRTOS.a

$(eval $(call image,IVORY_LEDTEST))

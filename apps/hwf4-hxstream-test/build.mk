# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# build.mk
#
# Copyright (C) 2012, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#
# Written by Pat Hickey <pat@galois.com>, January 08, 2013
#

APP_HWF4_HXSTREAM_IMG          := hwf4-hxstream-test

APP_HWF4_HXSTREAM_OBJECTS      := main.o
APP_HWF4_HXSTREAM_OBJECTS      += test.o

APP_HWF4_HXSTREAM_INCLUDES     += $(FREERTOS_INCLUDES)
APP_HWF4_HXSTREAM_INCLUDES     += $(HWF4_INCLUDES)

APP_HWF4_HXSTREAM_CFLAGS        = $(APP_HWF4_HXSTREAM_INCLUDES)
APP_HWF4_HXSTREAM_CXXFLAGS      = $(APP_HWF4_HXSTREAM_INCLUDES)

APP_HWF4_HXSTREAM_LIBRARIES    += libhwf4.a
APP_HWF4_HXSTREAM_LIBRARIES    += libstm32_usb.a
APP_HWF4_HXSTREAM_LIBRARIES    += libFreeRTOS.a

APP_HWF4_HXSTREAM_LIBS         += -lm

ifeq ($(CONFIG_HWF4_COMPATIBILITY),1)
  $(eval $(call when_os,freertos,image,APP_HWF4_HXSTREAM))
endif

# vim: set ft=make noet ts=2:

# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# build.mk --- Build rules for the hwf4 library.
#
# Copyright (C) 2012, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#
# Written by James Bielman <jamesjb@galois.com>, December 07, 2012
#

HWF4_LIB     := libhwf4.a

HWF4_CFLAGS  += $(FREERTOS_CFLAGS)
HWF4_CFLAGS  += -I$(TOP)/hwf4/include
HWF4_CFLAGS  += -I$(TOP)/include
HWF4_CFLAGS  += -I$(TOP)/stm32_usb/include

HWF4_OBJECTS :=    \
  src/rcc.o        \
  src/interrupt.o  \
  src/i2c.o        \
  src/gpio.o       \
  src/usart.o      \
  src/spi.o        \
  src/fault.o      \
  src/timer.o      \
  src/led.o        \
  src/usb_cdc.o    \
  src/eeprom.o     \
  src/ardrone.o

$(eval $(call library,HWF4))

# vim: set ft=make noet ts=2:

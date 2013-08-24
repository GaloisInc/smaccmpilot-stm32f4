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

HWF4_INCLUDES  += -I$(TOP)/src/bsp/hwf4/include
HWF4_INCLUDES  += -I$(TOP)/src/bsp/include

HWF4_CFLAGS  += $(FREERTOS_CFLAGS)
HWF4_CFLAGS  += $(HWF4_INCLUDES)
HWF4_CFLAGS  += -I$(TOP)/src/bsp/stm32_usb/include

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

$(eval $(call when_os,freertos,library,HWF4))

HWF4_NOUART_LIB     := libhwf4-nouart.a

HWF4_NOUART_INCLUDES  += -I$(TOP)/src/bsp/hwf4/include
HWF4_NOUART_INCLUDES  += -I$(TOP)/src/bsp/include

HWF4_NOUART_CFLAGS  += $(FREERTOS_CFLAGS)
HWF4_NOUART_CFLAGS  += $(HWF4_NOUART_INCLUDES)

HWF4_NOUART_OBJECTS :=    \
  src/rcc.o        \
  src/interrupt.o  \
  src/i2c.o        \
  src/gpio.o       \
  src/spi.o        \
  src/fault.o      \
  src/timer.o      \
  src/led.o        \
  src/eeprom.o

$(eval $(call when_os,freertos,library,HWF4_NOUART))
# vim: set ft=make noet ts=2:

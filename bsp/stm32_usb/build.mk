# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# build.mk --- Build rules for the STM32 USB stack.
#
# Copyright (C) 2012, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#
# Written by James Bielman <jamesjb@galois.oom>, December 07, 2012
#

STM32_USB_LIB    := libstm32_usb.a

STM32_USB_CFLAGS += $(FREERTOS_CFLAGS)
STM32_USB_CFLAGS += -I$(TOP)/include
STM32_USB_CFLAGS += -I$(TOP)/StdPeriph/include
STM32_USB_CFLAGS += -I$(TOP)/stm32_usb/include/usb
STM32_USB_CFLAGS += -I$(TOP)/hwf4/include

STM32_USB_OBJECTS :=           \
  src/usb_bsp.o                \
  src/usb_core.o               \
  src/usb_dcd.o                \
  src/usb_dcd_int.o            \
  src/usbd_core.o              \
  src/usbd_ioreq.o             \
  src/usbd_req.o               \
  src/usbd_desc.o              \
  src/usbd_usr.o               \
  src/usbd_cdc_core.o          \
  src/usbd_cdc_vcp.o

$(eval $(call library,STM32_USB))

# vim: set ft=make noet ts=2:

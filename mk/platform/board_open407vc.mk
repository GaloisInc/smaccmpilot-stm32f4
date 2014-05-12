# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# board_stm32f4discovery.mk --- STM32F4 Discovery board support.
#
# Copyright (C) 2012, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#
# Written by James Bielman <jamesjb@galois.com>, December 07, 2012
#

# Frequency of the HSE crystal in Hz.
BOARD_HSE_FREQ := 8000000

# for our purposes, its compatible with f405
# which is the same chip w/o ethernet peripheral
CHIP := stm32f405

# Note compatibility with HWF4 library
CONFIG_LIBHWF4_COMPAT := 1

# Add the HSE frequency to the default CFLAGS.
CFLAGS += -DHSE_VALUE=$(BOARD_HSE_FREQ)

# Add a preprocessor definition for this board.
CFLAGS += -DCONFIG_BOARD_OPEN407VC

# Select the full-speed USB OTG core.
CFLAGS += -DUSE_USB_OTG_FS

# vim: set ft=make noet ts=2:

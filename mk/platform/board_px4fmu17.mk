# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# platform/board_px4fmu17.mk
# BSP (libhwf4) configuration specific to the PX4FMU v1.7
#
# Copyright (C) 2013, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#
# Written by James Bielman <jamesjb@galois.com>, December 07, 2012
#            Pat Hickey    <pat@galois.com>,     August 06, 2013
#
# Frequency of the HSE crystal in Hz.
BOARD_HSE_FREQ := 24000000

CHIP := stm32f405

# Add the HSE frequency to the default CFLAGS.
CFLAGS += -DHSE_VALUE=$(BOARD_HSE_FREQ)

# Note compatibility with HWF4 library
CONFIG_LIBHWF4_COMPAT := 1

# Add a preprocessor definition for this board.
CFLAGS += -DCONFIG_BOARD_PX4

# Select the full-speed USB OTG core.
CFLAGS += -DUSE_USB_OTG_FS


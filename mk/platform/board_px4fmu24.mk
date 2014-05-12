# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# platform/board_px4fmu24.mk
# configuration specific to the PX4FMU v2.4 (PixHawk main processor)
#
# Copyright (C) 2014, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#
# Written by Pat Hickey <pat@galois.com>, May 12, 2014
#
# Frequency of the HSE crystal in Hz.
BOARD_HSE_FREQ := 24000000

# Note compatibility with HWF4 library
CONFIG_LIBHWF4_COMPAT := 0

# Add the HSE frequency to the default CFLAGS.
CFLAGS += -DHSE_VALUE=$(BOARD_HSE_FREQ)


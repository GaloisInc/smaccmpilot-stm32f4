# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# fmu_17.mk --- PX4FMU v1.7 board support.
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
BOARD_HSE_FREQ := 24000000

# Add the HSE frequency to the default CFLAGS.
CFLAGS += -DHSE_VALUE=$(BOARD_HSE_FREQ)

# Add a preprocessor definition for this board.
CFLAGS += -DCONFIG_BOARD_PX4

# Select the full-speed USB OTG core.
CFLAGS += -DUSE_USB_OTG_FS

# vim: set ft=make noet ts=2:

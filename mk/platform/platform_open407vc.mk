# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# platform_stm32f4discovery.mk --- STM32F4 Discovery platform support.
#
# Copyright (C) 2012, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#
# Written by James Bielman <jamesjb@galois.com>, December 07, 2012
#            Pat Hickey    <pat@galois.com>,     August 06, 2013
#

include mk/platform/toolchain_stm32f4.mk
include mk/platform/board_open407vc.mk

# platform argument for tower builds
open407vc_TOWER_PLATFORM := open407vc

# operating system argument for tower builds
open407vc_TOWER_OS := freertos

# vim: set ft=make noet ts=2:

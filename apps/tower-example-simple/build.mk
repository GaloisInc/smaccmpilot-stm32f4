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

$(eval $(call tower_pkg,IVORY_PKG_TOWER_EXAMPLE_SIMPLE,tower-example-simple))

APP_TOWER_EXAMPLE_SIMPLE_IMG          := tower-example-simple

APP_TOWER_EXAMPLE_SIMPLE_OBJECTS      := main.o
APP_TOWER_EXAMPLE_SIMPLE_REAL_OBJECTS += $(IVORY_PKG_TOWER_EXAMPLE_SIMPLE_OBJECTS)

APP_TOWER_EXAMPLE_SIMPLE_INCLUDES     += $(FREERTOS_INCLUDES)
# Required because we still used old style startup files:
APP_TOWER_EXAMPLE_SIMPLE_INCLUDES     += $(HWF4_INCLUDES)

APP_TOWER_EXAMPLE_SIMPLE_CFLAGS        = $(APP_TOWER_EXAMPLE_SIMPLE_INCLUDES)
APP_TOWER_EXAMPLE_SIMPLE_CFLAGS       += $(IVORY_PKG_TOWER_EXAMPLE_SIMPLE_CFLAGS)
APP_TOWER_EXAMPLE_SIMPLE_CXXFLAGS      = $(APP_TOWER_EXAMPLE_SIMPLE_INCLUDES)
APP_TOWER_EXAMPLE_SIMPLE_CXXFLAGS     += $(IVORY_PKG_TOWER_EXAMPLE_SIMPLE_CFLAGS)

APP_TOWER_EXAMPLE_SIMPLE_LIBRARIES    += libFreeRTOS.a

APP_TOWER_EXAMPLE_SIMPLE_LIBS         += -lm

$(eval $(call when_os,freertos,image,APP_TOWER_EXAMPLE_SIMPLE))

# vim: set ft=make noet ts=2:

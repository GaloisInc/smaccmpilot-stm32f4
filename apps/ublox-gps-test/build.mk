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

UBLOX_GPS_TEST_PLATFORMS := px4fmu17_bare_freertos px4fmu17_ioar_freertos open407vc

$(eval $(call when_platforms, $(UBLOX_GPS_TEST_PLATFORMS) \
				,tower_pkg,IVORY_PKG_UBLOX_GPS_TEST,ublox-gps-test-gen))

UBLOX_GPS_TEST_IMG          := ublox-gps-test

UBLOX_GPS_TEST_LIBRARIES    += libFreeRTOS.a
UBLOX_GPS_TEST_INCLUDES     += $(FREERTOS_CFLAGS)

UBLOX_GPS_TEST_REAL_OBJECTS += $(IVORY_PKG_UBLOX_GPS_TEST_OBJECTS)
UBLOX_GPS_TEST_LIBS         += -lm

UBLOX_GPS_TEST_INCLUDES     += $(IVORY_PKG_UBLOX_GPS_TEST_CFLAGS)

UBLOX_GPS_TEST_CFLAGS       += -O2 $(UBLOX_GPS_TEST_INCLUDES)
UBLOX_GPS_TEST_CFLAGS       += -O2 $(UBLOX_GPS_TEST_INCLUDES)
UBLOX_GPS_TEST_DISABLE_GLOBAL_STARTUP_OBJECTS := 1

$(eval $(call when_platforms,$(UBLOX_GPS_TEST_PLATFORMS) \
				,cbmc_pkg,UBLOX_GPS_TEST,IVORY_PKG_UBLOX_GPS_TEST))

$(eval $(call when_platforms,$(UBLOX_GPS_TEST_PLATFORMS) \
				,image,UBLOX_GPS_TEST))

# vim: set ft=make noet ts=2:

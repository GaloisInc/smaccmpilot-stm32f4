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

$(eval $(call tower_pkg,IVORY_PKG_ALL_SENSORS_TEST,all-sensors-test-gen))

PX4_ALL_SENSORS_TEST_IMG          := px4-all-sensors-test

PX4_ALL_SENSORS_TEST_LIBRARIES    += libFreeRTOS.a
PX4_ALL_SENSORS_TEST_INCLUDES     += $(FREERTOS_CFLAGS)

PX4_ALL_SENSORS_TEST_REAL_OBJECTS += $(IVORY_PKG_ALL_SENSORS_TEST_OBJECTS)
PX4_ALL_SENSORS_TEST_CFLAGS       += $(IVORY_PKG_ALL_SENSORS_TEST_CFLAGS)
PX4_ALL_SENSORS_TEST_CFLAGS       += $(PX4_ALL_SENSORS_TEST_INCLUDES)

PX4_ALL_SENSORS_TEST_DISABLE_GLOBAL_STARTUP_OBJECTS := 1

$(eval $(call cbmc_pkg,PX4_ALL_SENSORS_TEST,IVORY_PKG_ALL_SENSORS_TEST))

$(eval $(call when_os,freertos,image,PX4_ALL_SENSORS_TEST))
$(eval $(call when_os,echronos,echronos_gen,PX4_ALL_SENSORS_TEST))
$(eval $(call when_os,echronos,image,PX4_ALL_SENSORS_TEST))

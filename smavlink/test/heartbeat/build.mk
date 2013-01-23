# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# build.mk --- Build rules for SMAVLink tests.
#
# Copyright (C) 2012, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#

SMAVLINK_HEARTBEAT_TEST_IMG      := smavlink_heartbeat_test
SMAVLINK_HEARTBEAT_TEST_OBJECTS  := main.o

SMAVLINK_HEARTBEAT_TEST_INCLUDES  = $(FREERTOS_INCLUDES)
SMAVLINK_HEARTBEAT_TEST_INCLUDES += $(HWF4_INCLUDES)
SMAVLINK_HEARTBEAT_TEST_INCLUDES += $(SMAVLINK_INCLUDES)

SMAVLINK_HEARTBEAT_TEST_CFLAGS    += $(SMAVLINK_HEARTBEAT_TEST_INCLUDES)
SMAVLINK_HEARTBEAT_TEST_CXXFLAGS  += $(SMAVLINK_HEARTBEAT_TEST_INCLUDES)

SMAVLINK_HEARTBEAT_TEST_LIBRARIES += libsmavlink.a
SMAVLINK_HEARTBEAT_TEST_LIBRARIES += libhwf4.a
SMAVLINK_HEARTBEAT_TEST_LIBRARIES += libstm32_usb.a
SMAVLINK_HEARTBEAT_TEST_LIBRARIES += libFreeRTOS.a

$(eval $(call image,SMAVLINK_HEARTBEAT_TEST))

# vim: set ft=make noet ts=2:

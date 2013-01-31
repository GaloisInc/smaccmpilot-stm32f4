# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# build.mk --- Build rules for the smaccmpilot library.
#
# Copyright (C) 2012, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#
# Written by Pat Hickey <pat@galois.com>, 17 Jan 2013
#

SMACCMPILOT_LIB     := libsmaccmpilot.a

SMACCMPILOT_INCLUDES  += -I$(TOP)/smaccmpilot/include
SMACCMPILOT_INCLUDES  += $(HWF4_INCLUDES)
SMACCMPILOT_INCLUDES  += $(ARDUPILOT_LIBINCLUDES)
SMACCMPILOT_INCLUDES  += $(SMAVLINK_INCLUDES)
SMACCMPILOT_INCLUDES  += $(FREERTOS_CFLAGS)

SMACCMPILOT_CFLAGS    += $(SMACCMPILOT_INCLUDES)
# need to include with unqualified name: ivory doesnt have a way to
# build c files whose own headers have qualified names.
SMACCMPILOT_CFLAGS    += -I$(TOP)/smaccmpilot/include/smaccmpilot

# Allow overriding the GCS UART from Config.mk:
ifdef CONFIG_GCS_UART
SMACCMPILOT_CFLAGS    += -DCONFIG_GCS_UART=$(CONFIG_GCS_UART)
endif

SMACCMPILOT_CXXFLAGS  += $(SMACCMPILOT_INCLUDES)

SMACCMPILOT_OBJECTS :=       \
  src/apmotors_wrapper.o     \
  src/motorsoutput.o         \
  src/sensors.o              \
  src/userinput.o            \
  src/stabilize.o            \
  src/gcs_receive.o          \
  src/gcs_transmit_driver.o  \
  src/gcs_transmit.o         \
  src/userinput_decode.o     \
  src/optflow_input.o        \
  src/ioar_relay.o           \
  src/altitude_controller.o  \
  src/position_estimator.o   \
  src/pid_stabilize.o

$(eval $(call library,SMACCMPILOT))

# vim: set ft=make noet ts=2:

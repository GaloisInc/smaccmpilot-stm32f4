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

ifneq ($(CONFIG_BUILD_RTV),)
export RTV_DECLS := $(TOP)/apps/sample-rtv-task/instrumented-decls

$(eval $(call tower_pkg,IVORY_PKG_SAMPLE_RTV_TASK,sample-rtv-task-checker-gen))

APP_RTV_IMG       := sample-rtv

APP_RTV_OBJECTS   :=                    \
	legacy/legacy.o                       \
	record_assignment/record_assignment.o \
	checker/instrumented.o

APP_RTV_REAL_OBJECTS += $(IVORY_PKG_SAMPLE_RTV_TASK_OBJECTS)

APP_RTV_INCLUDES  += -I$(TOP)/apps/sample-rtv-task/record_assignment
APP_RTV_INCLUDES  += -I$(TOP)/apps/sample-rtv-task/legacy
APP_RTV_INCLUDES  += -I$(TOP)/apps/sample-rtv-task/checker
APP_RTV_INCLUDES  += $(HWF4_INCLUDES)
APP_RTV_INCLUDES  += $(FREERTOS_INCLUDES)
APP_RTV_INCLUDES  += $(IVORY_PKG_SAMPLE_RTV_TASK_CFLAGS)

APP_RTV_CFLAGS    += $(APP_RTV_INCLUDES)
APP_RTV_CFLAGS    += -fplugin=gcc-plugin/instrument_plugin.so
APP_RTV_CFLAGS    += -DIVORY_DEPLOY

APP_RTV_LIBRARIES += libhwf4.a
APP_RTV_LIBRARIES += libstm32_usb.a
APP_RTV_LIBRARIES += libFreeRTOS.a

APP_RTV_LIBS      += -lm

$(eval $(call cbmc_pkg,APP_RTV,IVORY_PKG_SAMPLE))

$(eval $(call when_os,freertos,image,APP_RTV))
endif

# vim: set ft=make noet ts=2:

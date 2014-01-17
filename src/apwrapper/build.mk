# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# build.mk --- Build rules for the standalone-apahrs library.
#
# Copyright (C) 2013, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#
# Written by Pat Hickey <pat@galois.com>, September 19, 2013
#

AP_WRAPPER_LIB       := libapwrapper.a

AP_WRAPPER_INCLUDES  += -I$(TOP)/src/apwrapper/include
AP_WRAPPER_INCLUDES  += -I$(TOP)/src/standalone_apahrs
ifneq ($($(CONFIG_PLATFORM)_TOWER_OS),echronos)
AP_WRAPPER_INCLUDES  += $(FREERTOS_INCLUDES)
else
AP_WRAPPER_INCLUDES  += $(LIBECHRONOS_INCLUDES)
endif
AP_WRAPPER_INCLUDES  += -I$(TOP)/src/bsp/hwf4/include

AP_WRAPPER_CFLAGS    += $(AP_WRAPPER_INCLUDES)
AP_WRAPPER_CXXFLAGS  += $(AP_WRAPPER_INCLUDES)
AP_WRAPPER_CXXFLAGS  += -Wno-psabi

AP_WRAPPER_OBJECTS :=              \
        src/sensors_capture.o      \
        src/userinput_capture.o

$(eval $(call when_os,freertos,library,AP_WRAPPER))
$(eval $(call when_os,echronos,library,AP_WRAPPER))


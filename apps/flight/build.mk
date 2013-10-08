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

IVORY_PKG_FLIGHT_GEN_SYMS    := true

$(eval $(call when_platforms, \
				px4fmu17_ioar_freertos \
				px4fmu17_bare_freertos \
				px4fmu17_ioar_aadl \
				,tower_pkg,IVORY_PKG_FLIGHT,flight-gen))

FLIGHT_IMG       := flight

FLIGHT_INCLUDES  += $(HWF4_INCLUDES)
FLIGHT_INCLUDES  += -I$(TOP)/src/standalone_apahrs
FLIGHT_INCLUDES  += -I$(TOP)/src/apwrapper/include
FLIGHT_INCLUDES  += $(FREERTOS_CFLAGS)
FLIGHT_INCLUDES  += $(IVORY_PKG_FLIGHT_CFLAGS)

# For the cryto lib
FLIGHT_INCLUDES  += -I$(TOP)/src/crypto/include
FLIGHT_INCLUDES  += -DARM

# XXX some users of this library include it without putting the
# directory in the include file name.  We should clean this up.

FLIGHT_CFLAGS    += $(FLIGHT_INCLUDES)
FLIGHT_CFLAGS    += -DIVORY_TEST

FLIGHT_CXXFLAGS  += $(FLIGHT_INCLUDES)
FLIGHT_CXXFLAGS  += -Wno-psabi

FLIGHT_OBJECTS := main.o

FLIGHT_REAL_OBJECTS += $(IVORY_PKG_FLIGHT_OBJECTS)

FLIGHT_LIBRARIES    += libapwrapper.a
FLIGHT_LIBRARIES    += libstandalone-apahrs.a
FLIGHT_LIBRARIES    += libstandalone-aphal.a
FLIGHT_LIBRARIES    += libhwf4-nouart.a
FLIGHT_LIBRARIES    += libFreeRTOS.a
FLIGHT_LIBRARIES    += commsec.a
FLIGHT_LIBS         += -lm

$(eval $(call when_platforms,px4fmu17_bare_freertos px4fmu17_ioar_freertos \
				,cbmc_pkg,FLIGHT,IVORY_PKG_FLIGHT))

$(eval $(call when_platforms,px4fmu17_bare_freertos px4fmu17_ioar_freertos \
				,image,FLIGHT))

# ------------------------------------------------------------------------------
# AADL Build
# ------------------------------------------------------------------------------

LIB_FLIGHT_LIB          := libflight.a
LIB_FLIGHT_INCLUDES     += $(HWF4_INCLUDES)
LIB_FLIGHT_INCLUDES     += -I$(TOP)/src/standalone_apahrs
LIB_FLIGHT_INCLUDES     += -I$(TOP)/src/apwrapper/include
LIB_FLIGHT_REAL_OBJECTS += $(call filteroutstring,tower_task_loop_, \
                                      $(IVORY_PKG_FLIGHT_OBJECTS))
LIB_FLIGHT_CFLAGS       += $(LIB_FLIGHT_INCLUDES)
LIB_FLIGHT_CFLAGS       += $(IVORY_PKG_FLIGHT_CFLAGS)
LIB_FLIGHT_CFLAGS       += -DIVORY_DEPLOY

$(eval $(call when_os,aadl,library,LIB_FLIGHT))




# ------------------------------------------------------------------------------
# CBMC stuff - deprecated? ask lee
# ------------------------------------------------------------------------------

# CBMC_SRCS		:= $(patsubst %, --src=%, $(IVORY_PKG_FLIGHT_SOURCES))
# CBMC_INCLS  := \
#   -I./src/bsp/hwf4/include \
#   -I./src/bsp/include \
#   -I./src/flight/include \
# 	$(IVORY_PKG_FLIGHT_CFLAGS) \
# 	-I$(GEN_DIR)/src/flight/flight \
#   $(FREERTOS_INCLUDES)


# TABLE        = $(TOP)/src/flight/claims-table
# ENTRY_FUNCS	:= $(patsubst %, --function=%, $(IVORY_PKG_FLIGHT_SYMS))

# .PHONY: verify
# verify: $(IVORY_PKG_FLIGHT_HEADERS) $(IVORY_PKG_FLIGHT_SOURCES)
# 	$(CBMC_REPORT) \
# 		--outfile=$(TABLE).md \
# 		--format=markdown \
# 		--timeout=60 \
# 		--no-asserts \
# 		--threads=2 \
# 		--sort=result \
# 		--cbmc=$(CBMC_EXEC) \
# 		$(CBMC_INCS) \
# 		$(CBMC_SRCS) \
# 		$(ENTRY_FUNCS) \
# 		-- -D IVORY_CBMC
# 	pandoc -o $(TABLE).html $(TABLE).md

# # Just for testing
# .PHONY: verify-test
# verify-test: $(IVORY_PKG_FLIGHT_HEADERS) $(IVORY_PKG_FLIGHT_SOURCES)
# 	$(CBMC_REPORT) \
#     --format=markdown \
#     --timeout=60 \
#     --no-asserts \
#     --sort=result \
#     --cbmc=$(CBMC_EXEC) \
#     $(CBMC_INCS) \
#     $(CBMC_SRCS) \
#     --function=tower_entry \
#     -- -D IVORY_CBMC

    # --outfile=$(TOP)/ivory/claims-table-tmp.md \

# CLEAN += $(TABLE).html

# vim: set ft=make noet ts=2:

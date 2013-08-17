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

IVORY_PKG_FLIGHT_INCLUDE_DIR := flight
IVORY_PKG_FLIGHT_GEN_SYMS    := true

$(eval $(call when_os,freertos,tower_pkg,IVORY_PKG_FLIGHT,smaccmpilot-gen))

FLIGHT_LIB       := libflight.a

FLIGHT_INCLUDES  += -I$(TOP)/src/flight/include
FLIGHT_INCLUDES  += $(HWF4_INCLUDES)
FLIGHT_INCLUDES  += -I$(TOP)/src/flight/standalone_apahrs
FLIGHT_INCLUDES  += $(FREERTOS_CFLAGS)

# XXX some users of this library include it without putting the
# directory in the include file name.  We should clean this up.
FLIGHT_INCLUDES  += $(IVORY_PKG_FLIGHT_CFLAGS)
FLIGHT_INCLUDES  += -I$(GEN_DIR)/src/flight/flight

FLIGHT_CFLAGS    += $(FLIGHT_INCLUDES)
FLIGHT_CXXFLAGS  += $(FLIGHT_INCLUDES)

# Allow overriding the GCS UART from Config.mk:
ifdef CONFIG_GCS_UART
FLIGHT_CFLAGS    += -DCONFIG_GCS_UART=$(CONFIG_GCS_UART)
endif

# Ignore ASSERTS() REQUIRES() in the source (used by CBMC).
FLIGHT_CFLAGS += -DIVORY_DEPLOY

FLIGHT_CXXFLAGS  += $(FLIGHT_INCLUDES)

FLIGHT_OBJECTS := $(addprefix src/,\
	apmotors_wrapper_stub.o \
	console_prim.o \
	sensors_capture.o \
	userinput_capture.o \
	)

FLIGHT_REAL_OBJECTS += $(IVORY_PKG_FLIGHT_OBJECTS)

$(eval $(call when_os,freertos,cbmc_pkg,FLIGHT,IVORY_PKG_FLIGHT))

$(eval $(call when_os,freertos,library,FLIGHT))

# ------------------------------------------------------------------------------
# CBMC stuff
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

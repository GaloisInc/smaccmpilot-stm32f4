# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-

# IVORY += rtv-sample-task

RTV_GEN_HEADERS := $(OUTDIR)/instrumented.h $(OUTDIR)/runtime-checker.h
RTV_GEN_SOURCES := $(OUTDIR)/instrumented.c $(OUTDIR)/runtime-checker.c

# Main build entry point.
.PHONY: rtv-sample-task
rtv-sample-task: $(RTV_GEN_HEADERS) $(RTV_GEN_SOURCES) $(APP_RTV_IMG)

# $(IVORY_BSP_STM32F4_SOURCES)

PRJ := sample-rtv-task
OUTDIR := $(PRJ)/generated

APP_RTV_IMG         := sample-rtv

APP_RTV_OBJECTS     := main.o record_assignment.o checker_task.o \
                         generated/instrumented.o generated/runtime-checker.o

IVORY_RTV_SANDBOX   := $(TOP)/../dsl/cabal-dev

RTV_GETSET_GEN_EXE  := \
  $(IVORY_RTV_SANDBOX)/bin/$(PRJ)-setget-gen
RTV_CHECKER_GEN_EXE := \
  $(IVORY_RTV_SANDBOX)/bin/$(PRJ)-checker-gen

APP_RTV_INCLUDES     += $(FREERTOS_INCLUDES)
APP_RTV_INCLUDES     += -I$(TOP)/ivory-freertos-wrapper/include
APP_RTV_INCLUDES     += -I$(TOP)/ivory-runtime/
APP_RTV_INCLUDES     += -I$(TOP)/bsp/hwf4/include

APP_RTV_CFLAGS        = $(APP_RTV_INCLUDES)
APP_RTV_CFLAGS       += -fplugin=$(TOP)/../../ARM-analysis/GCC_plugin/instrument_plugin.so

APP_RTV_CXXFLAGS      = $(APP_RTV_INCLUDES)

APP_RTV_LIBRARIES    += libhwf4.a
APP_RTV_LIBRARIES    += libstm32_usb.a
APP_RTV_LIBRARIES    += libFreeRTOS.a

APP_RTV_LIBS         += -lm

$(eval $(call image,APP_RTV))

$(RTV_GEN_HEADERS) $(RTV_GEN_SOURCES): \
  $(RTV_GETSET_GEN_EXE) $(RTV_CHECKER_GEN_EXE)
	# XXX fix where to put output dir
	cd $(PRJ) && ../$(RTV_GETSET_GEN_EXE)
	cd $(PRJ) && ../$(RTV_CHECKER_GEN_EXE)

# Made from DSL file.
# .PRECIOUS: $(RTV_GETSET_GEN_EXE) $(RTV_CHECKER_GEN_EXE)
# $(RTV_GETSET_GEN_EXE) $(RTV_CHECKER_GEN_EXE):
# 	cabal-dev -s $(IVORY_RTV_SANDBOX) install --builddir=$(TOP)/$(PRJ) \
# 	          $(TOP)/$(PRJ)

# vim: set ft=make noet ts=2:

CLEAN += $(OUTDIR)

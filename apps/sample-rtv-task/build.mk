# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
# vim: set ft=make noet ts=2:

ifneq ($(CONFIG_BUILD_RTV),)

# Should be the sample name as the project directory.
PRJ := apps/sample-rtv-task
OUTDIR := $(PRJ)/generated
RTV_GEN_HEADERS := $(OUTDIR)/instrumented.h $(OUTDIR)/runtime-checker.h
RTV_GEN_SOURCES := $(OUTDIR)/instrumented.c $(OUTDIR)/runtime-checker.c

# $(IVORY_BSP_STM32F4_SOURCES)

# Build Haskell-generated stuff first
IVORY += $(RTV_GEN_HEADERS) $(RTV_GEN_SOURCES)

APP_RTV_IMG         := sample-rtv
APP_RTV_OBJECTS     := main.o record_assignment.o checker_task.o \
                         generated/instrumented.o generated/runtime-checker.o
IVORY_RTV_SANDBOX   := $(CONFIG_CABAL_SANDBOX)
RTV_CHECKER_GEN_EXE := \
  $(IVORY_RTV_SANDBOX)/bin/$(PRJ)-checker-gen

APP_RTV_INCLUDES     += $(FREERTOS_INCLUDES)
APP_RTV_INCLUDES     += -I$(TOP)/src/ivory-freertos-wrapper/include
APP_RTV_INCLUDES     += -I$(TOP)/src/bsp/hwf4/include
APP_RTV_INCLUDES     += -I$(TOP)/$(OUTDIR)

APP_RTV_CFLAGS       += $(APP_RTV_INCLUDES)
APP_RTV_CFLAGS       += \
  -fplugin=$(TOP)/../../ARM-analysis/GCC_plugin/instrument_plugin.so

APP_RTV_LIBRARIES    += libhwf4.a
APP_RTV_LIBRARIES    += libstm32_usb.a
APP_RTV_LIBRARIES    += libFreeRTOS.a

APP_RTV_LIBS         += -lm

# Build target for the entire project.
$(APP_RTV_IMG): $(RTV_GEN_HEADERS) $(RTV_GEN_SOURCES)

$(eval $(call image,APP_RTV))

$(RTV_GEN_HEADERS) $(RTV_GEN_SOURCES): $(RTV_CHECKER_GEN_EXE)
  # XXX fix where to put output dir
	echo "Generating RTV sources..."
	cd $(PRJ) && ../$(RTV_CHECKER_GEN_EXE)

CLEAN += $(OUTDIR)
CLEAN += $(addprefix $(OBJ_DIR)/, $(PRJ))

endif

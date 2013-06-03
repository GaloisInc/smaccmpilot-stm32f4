# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
# vim: set ft=make noet ts=2:

# ifneq ($(CONFIG_BUILD_RTV),)

# Should be the sample name as the project directory.
TEST := sample-rtv-task
PRJ := apps/$(TEST)
OUTDIR := $(PRJ)/generated

TOWER_HDRS = $(PRJ)/tower-hdrs
TOWER_SRCS = $(PRJ)/tower-srcs

RTV_GEN_HEADERS := \
  $(TOWER_HDRS)/*.h $(PRJ)/legacy/*.h $(PRJ)/record_assignment/*.h $(OUTDIR)/*.h
RTV_GEN_SOURCES := \
  $(TOWER_SRCS)/*.c $(PRJ)/legacy/*.c $(PRJ)/record_assignment/*.c $(OUTDIR)/*.c

# Build Haskell-generated stuff first
# IVORY += $(RTV_GEN_HEADERS) $(RTV_GEN_SOURCES)

APP_RTV_IMG         := sample-rtv

APP_RTV_OBJECTS     := \
  record_assignment/record_assignment.o tower-srcs/tower.o legacy/legacy.o
APP_RTV_OBJECTS     += generated/instrumented.o generated/runtime-checker.o

IVORY_RTV_SANDBOX   := $(CONFIG_CABAL_SANDBOX)

RTV_CHECKER_GEN_EXE := $(IVORY_RTV_SANDBOX)/bin/$(TEST)-checker-gen

APP_RTV_INCLUDES     += $(FREERTOS_INCLUDES)
APP_RTV_INCLUDES     += -I$(TOP)/$(PRJ)
APP_RTV_INCLUDES     += -I$(TOP)/src/ivory-freertos-wrapper/include
APP_RTV_INCLUDES     += -I$(TOP)/$(OUTDIR)
APP_RTV_INCLUDES     += -I$(TOP)/$(TOWER_HDRS)
APP_RTV_INCLUDES     += -I$(TOP)/$(PRJ)/legacy
APP_RTV_INCLUDES     += -I$(TOP)/$(PRJ)/record_assignment
APP_RTV_INCLUDES     += -I$(TOP)/src/bsp/hwf4/include

APP_RTV_CFLAGS       += $(APP_RTV_INCLUDES)
APP_RTV_CFLAGS       += \
  -fplugin=$(TOP)/../../ARM-analysis/GCC_plugin/instrument_plugin.so
APP_RTV_CFLAGS       += -DIVORY_DEPLOY

APP_RTV_LIBRARIES    += libhwf4.a
APP_RTV_LIBRARIES    += libstm32_usb.a
APP_RTV_LIBRARIES    += libivoryfreertoswrapper.a
APP_RTV_LIBRARIES    += libFreeRTOS.a

APP_RTV_LIBS         += -lm

# Build target for the entire project.
$(APP_RTV_IMG): $(RTV_GEN_HEADERS) $(RTV_GEN_SOURCES)

$(eval $(call image,APP_RTV))

# $(RTV_GEN_HEADERS) $(RTV_GEN_SOURCES): $(RTV_CHECKER_GEN_EXE)
#   # XXX fix where to put output dir
# 	echo "Generating RTV sources..."
# 	cd $(PRJ) && $(RTV_CHECKER_GEN_EXE)

# CLEAN += $(OUTDIR)
# CLEAN += $(addprefix $(OBJ_DIR)/, $(PRJ))

# endif

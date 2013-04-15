# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-

IVORY += ivory-rtv-sample-task-build

.PHONY: ivory-rtv-sample-task-build
ivory-rtv-sample-task-build: $(IVORY_RTV_GEN_EXEC) $(IVORY_RTV_HEADERS) # $(IVORY_BSP_STM32F4_SOURCES)

APP_RTVTEST_IMG          := sample-rtv

APP_RTVTEST_OBJECTS      := main.o record_assignment.o checker_task.o \
                            generated/instrumented.o generated/runtime-checker.o

IVORY_RTV_SANDBOX   := $(TOP)/../dsl/cabal-dev

RTVTEST_GETSET_GENERATOR_EXE := $(TOP)/../dsl/cabal-dev/bin/sample-rtv-task-setget-gen
RTVTEST_CHECKER_GENERATOR_EXE := $(TOP)/../dsl/cabal-dev/bin/sample-rtv-task-checker-gen
RTVTEST_GENERATED_HEADERS := sample-rtv-task/generated/instrumented.h sample-rtv-task/generated/runtime-checker.h
RTVTEST_GENERATED_SOURCES := generated/instrumented.c generated/runtime-checker.c

APP_RTVTEST_INCLUDES     += $(FREERTOS_INCLUDES)
APP_RTVTEST_INCLUDES     += -I$(TOP)/ivory-freertos-wrapper/include
APP_RTVTEST_INCLUDES     += -I$(TOP)/ivory-runtime/
APP_RTVTEST_INCLUDES     += -I$(TOP)/bsp/hwf4/include

APP_RTVTEST_CFLAGS        = $(APP_RTVTEST_INCLUDES)
APP_RTVTEST_CXXFLAGS      = $(APP_RTVTEST_INCLUDES)

APP_RTVTEST_LIBRARIES    += libhwf4.a
APP_RTVTEST_LIBRARIES    += libstm32_usb.a
APP_RTVTEST_LIBRARIES    += libFreeRTOS.a

APP_RTVTEST_LIBS         += -lm

$(eval $(call image,APP_RTVTEST))

$(RTVTEST_GENERATED_HEADERS) $(RTVTEST_GENERATED_SOURCES): $(RTVTEST_GETSET_GENERATOR_EXE) $(RTVTEST_CHECKER_GENERATOR_EXE)
	# blargh
	cd sample-rtv-task && ../$(RTVTEST_GETSET_GENERATOR_EXE)
	cd sample-rtv-task && ../$(RTVTEST_CHECKER_GENERATOR_EXE)

.PRECIOUS: $(RTVTEST_GETSET_GENERATOR_EXE) $(RTVTEST_CHECKER_GENERATOR_EXE)
$(RTVTEST_GETSET_GENERATOR_EXE) $(RTVTEST_CHECKER_GENERATOR_EXE):
	cabal-dev -s $(IVORY_RTV_SANDBOX) install --builddir=$(TOP)/sample-rtv-task \
	          $(TOP)/sample-rtv-task

# vim: set ft=make noet ts=2:

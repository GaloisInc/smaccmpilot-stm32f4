
APP_RTV_IMG          := sample-rtv

APP_RTV_OBJECTS      := \
  legacy/legacy.o \
  record_assignment/record_assignment.o \
  checker/instrumented.o

APP_RTV_INCLUDES     += -I$(TOP)/apps/sample-rtv-task/record_assignment
APP_RTV_INCLUDES     += \
  -I$(TOP)/apps/sample-rtv-task/generated/include/generated
APP_RTV_INCLUDES     += -I$(TOP)/apps/sample-rtv-task/generated/include
APP_RTV_INCLUDES     += $(FREERTOS_INCLUDES)
APP_RTV_INCLUDES     += -I$(TOP)/apps/sample-rtv-task/legacy
APP_RTV_INCLUDES     += -I$(TOP)/apps/sample-rtv-task/checker
APP_RTV_INCLUDES     += -I$(TOP)/src/bsp/hwf4/include

APP_RTV_CFLAGS       += $(APP_RTV_INCLUDES)
APP_RTV_CFLAGS       += -fplugin=$(GCC_PLUGIN)/instrument_plugin.so
APP_RTV_CFLAGS       += -DIVORY_DEPLOY

APP_RTV_LIBRARIES    += librtvtest-generated.a
APP_RTV_LIBRARIES    += libhwf4.a
APP_RTV_LIBRARIES    += libstm32_usb.a
APP_RTV_LIBRARIES    += libFreeRTOS.a

APP_RTV_LIBS         += -lm

$(eval $(call image,APP_RTV))

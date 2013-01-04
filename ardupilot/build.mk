# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# build.mk --- Build rules for SMACCM+Ardupilot.
#
# Copyright (C) 2012, Galois, Inc.
# All Rights Reserved.
#
# Written by James Bielman <jamesjb@galois.com>, December 07, 2012
#

ARDUPILOT_SRC    := $(CONFIG_ARDUPILOT_PREFIX)
ARDUPILOT_LIB    := libardupilot.a

# List of subdirectories to add to the include path.  There are a lot.
ARDUPILOT_LIB_DIRS :=             \
  libraries/AP_HAL_SMACCM         \
  libraries/AP_HAL                \
  libraries/AP_HAL/utility        \
  libraries/AP_ADC                \
  libraries/AP_AHRS               \
  libraries/AP_Airspeed           \
  libraries/AP_Baro               \
  libraries/AP_Common             \
  libraries/AP_Compass            \
  libraries/AP_Declination        \
  libraries/AP_GPS                \
  libraries/AP_InertialSensor     \
  libraries/AP_Math               \
  libraries/AP_Param              \
  libraries/AP_Progmem            \
  libraries/Filter

ARDUPILOT_CXXFLAGS += $(FREERTOS_CFLAGS)
ARDUPILOT_CXXFLAGS += -I$(TOP)/hwf4/include
ARDUPILOT_CXXFLAGS += $(addprefix -I$(ARDUPILOT_SRC)/,$(ARDUPILOT_LIB_DIRS))
ARDUPILOT_CXXFLAGS += -DCONFIG_HAL_BOARD=HAL_BOARD_SMACCM
ARDUPILOT_CXXFLAGS += -Wno-psabi

ARDUPILOT_OBJECTS :=                                            \
  libraries/AP_HAL_SMACCM/AnalogIn.o                            \
  libraries/AP_HAL_SMACCM/Console.o                             \
  libraries/AP_HAL_SMACCM/GPIO.o                                \
  libraries/AP_HAL_SMACCM/HAL_SMACCM_Class.o                    \
  libraries/AP_HAL_SMACCM/I2CDriver.o                           \
  libraries/AP_HAL_SMACCM/PrivateMember.o                       \
  libraries/AP_HAL_SMACCM/RCInput.o                             \
  libraries/AP_HAL_SMACCM/RCOutput.o                            \
  libraries/AP_HAL_SMACCM/Scheduler.o                           \
  libraries/AP_HAL_SMACCM/Semaphores.o                          \
  libraries/AP_HAL_SMACCM/SPIDriver.o                           \
  libraries/AP_HAL_SMACCM/Storage.o                             \
  libraries/AP_HAL_SMACCM/UARTDriver.o                          \
  libraries/AP_HAL_SMACCM/Util.o                                \
  libraries/AP_HAL/utility/Print.o                              \
  libraries/AP_AHRS/AP_AHRS.o                                   \
  libraries/AP_AHRS/AP_AHRS_DCM.o                               \
  libraries/AP_AHRS/AP_AHRS_MPU6000.o                           \
  libraries/AP_Airspeed/AP_Airspeed.o                           \
  libraries/AP_Baro/AP_Baro.o                                   \
  libraries/AP_Baro/AP_Baro_MS5611.o                            \
  libraries/AP_Common/c++.o                                     \
  libraries/AP_Compass/AP_Compass_HMC5843.o                     \
  libraries/AP_Compass/Compass.o                                \
  libraries/AP_Declination/AP_Declination.o                     \
  libraries/AP_InertialSensor/AP_InertialSensor.o               \
  libraries/AP_InertialSensor/AP_InertialSensor_MPU6000.o       \
  libraries/AP_Math/AP_Math.o                                   \
  libraries/AP_Math/location.o                                  \
  libraries/AP_Math/matrix3.o                                   \
  libraries/AP_Math/polygon.o                                   \
  libraries/AP_Math/quaternion.o                                \
  libraries/AP_Math/vector2.o                                   \
  libraries/AP_Math/vector3.o                                   \
  libraries/AP_Param/AP_Param.o                                 \
  libraries/Filter/DerivativeFilter.o

# Copy files from the Ardupilot source tree into our local tree.  Our
# build system doesn't make it easy to compile out-of-tree sources.
ardupilot/%.cpp: $(ARDUPILOT_SRC)/%.cpp
	mkdir -p $(dir $@)
	cp $< $@

ifdef CONFIG_ARDUPILOT_PREFIX
$(eval $(call library,ARDUPILOT))
endif

# vim: set ft=make noet ts=2:

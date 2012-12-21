# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# build.mk --- Build rules for the AP_HAL_SMACCM library.
#
# Copyright (C) 2012, Galois, Inc.
# All Rights Reserved.
#
# This software is released under the "BSD3" license.  Read the file
# "LICENSE" for more information.
#
# Written by James Bielman <jamesjb@galois.com>, December 07, 2012
#

AP_HAL_SMACCM_LIB     := libAP_HAL_SMACCM.a

AP_HAL_SMACCM_CXXFLAGS  += $(FREERTOS_CFLAGS)
AP_HAL_SMACCM_CXXFLAGS  += -I$(TOP)/hwf4/include
AP_HAL_SMACCM_CXXFLAGS  += -I$(TOP)/include
AP_HAL_SMACCM_CXXFLAGS  += -I$(TOP)/stm32_usb/include
AP_HAL_SMACCM_CXXFLAGS  += -I$(TOP)/ap_hal/AP_HAL
AP_HAL_SMACCM_CXXFLAGS  += -I$(TOP)/ap_hal/AP_HAL_SMACCM
AP_HAL_SMACCM_CXXFLAGS  += -I$(TOP)/ap_hal/AP_Common
AP_HAL_SMACCM_CXXFLAGS  += -I$(TOP)/ap_hal/AP_Math
AP_HAL_SMACCM_CXXFLAGS  += -I$(TOP)/ap_hal/AP_Param
AP_HAL_SMACCM_CXXFLAGS  += -I$(TOP)/ap_hal/AP_Progmem
AP_HAL_SMACCM_CXXFLAGS  += -I$(TOP)/ap_hal/AP_InertialSensor
AP_HAL_SMACCM_CXXFLAGS  += -I$(TOP)/ap_hal/AP_Declination
AP_HAL_SMACCM_CXXFLAGS  += -I$(TOP)/ap_hal/AP_Compass
AP_HAL_SMACCM_CXXFLAGS  += -I$(TOP)/ap_hal/Filter
AP_HAL_SMACCM_CXXFLAGS  += -I$(TOP)/ap_hal/AP_Baro
AP_HAL_SMACCM_CXXFLAGS  += -DCONFIG_HAL_BOARD=HAL_BOARD_SMACCM
AP_HAL_SMACCM_CXXFLAGS  += -Wno-psabi

AP_HAL_SMACCM_CFLAGS    := $(AP_HAL_SMACCM_CXXFLAGS)

AP_HAL_SMACCM_OBJECTS :=                        \
  AP_Common/c++.o                               \
  AP_Math/AP_Math.o                             \
  AP_Math/location.o                            \
  AP_Math/matrix3.o                             \
  AP_Math/polygon.o                             \
  AP_Math/quaternion.o                          \
  AP_Math/vector2.o                             \
  AP_Math/vector3.o                             \
  AP_Param/AP_Param.o                           \
  AP_HAL/utility/Print.o                        \
  AP_HAL_SMACCM/AnalogIn.o                      \
  AP_HAL_SMACCM/Console.o                       \
  AP_HAL_SMACCM/GPIO.o                          \
  AP_HAL_SMACCM/HAL_SMACCM_Class.o              \
  AP_HAL_SMACCM/I2CDriver.o                     \
  AP_HAL_SMACCM/PrivateMember.o                 \
  AP_HAL_SMACCM/RCInput.o                       \
  AP_HAL_SMACCM/RCOutput.o                      \
  AP_HAL_SMACCM/Scheduler.o                     \
  AP_HAL_SMACCM/Semaphore.o                     \
  AP_HAL_SMACCM/SPIDriver.o                     \
  AP_HAL_SMACCM/Storage.o                       \
  AP_HAL_SMACCM/UARTDriver.o                    \
  AP_HAL_SMACCM/Util.o                          \
  AP_InertialSensor/AP_InertialSensor.o         \
  AP_InertialSensor/AP_InertialSensor_MPU6000.o \
  AP_Declination/AP_Declination.o               \
  AP_Compass/AP_Compass_HIL.o                   \
  AP_Compass/AP_Compass_HMC5843.o               \
  AP_Compass/Compass.o                          \
  AP_Baro/AP_Baro.o                             \
  AP_Baro/AP_Baro_MS5611.o                      \
  Filter/DerivativeFilter.o

$(eval $(call library,AP_HAL_SMACCM))

# vim: set ft=make noet ts=2:

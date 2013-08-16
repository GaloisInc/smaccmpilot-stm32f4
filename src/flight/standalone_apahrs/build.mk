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
# Written by Pat Hickey <pat@galois.com>, August 16, 2013
#

STANDALONE_APAHRS_LIB       := libstandalone-apahrs.a

STANDALONE_APAHRS_INCLUDES  += -I$(TOP)/src/flight/standalone_apahrs

STANDALONE_APAHRS_CFLAGS    += $(STANDALONE_APAHRS_INCLUDES)
STANDALONE_APAHRS_CXXFLAGS  += $(STANDALONE_APAHRS_INCLUDES)
STANDALONE_APAHRS_CXXFLAGS  += -Wno-psabi

STANDALONE_APAHRS_OBJECTS :=                                           \
        AP_Airspeed/AP_Airspeed.o                                      \
        AP_Airspeed/Airspeed_Calibration.o                             \
        AP_GPS/AP_GPS_NMEA.o                                           \
        AP_GPS/AP_GPS_UBLOX.o                                          \
        AP_GPS/GPS.o                                                   \
        AP_Declination/AP_Declination.o                                \
        AP_HAL/utility/Print.o                                         \
        AP_Param/AP_Param.o                                            \
        AP_InertialSensor/AP_InertialSensor.o                          \
        AP_InertialSensor/AP_InertialSensor_MPU6000.o                  \
        AP_InertialSensor/AP_InertialSensor_UserInteract_Stream.o      \
        AP_AHRS/AP_AHRS.o                                              \
        AP_AHRS/AP_AHRS_DCM.o                                          \
        AP_Baro/AP_Baro.o                                              \
        AP_Baro/AP_Baro_MS5611.o                                       \
        AP_Math/AP_Math.o                                              \
        AP_Math/quaternion.o                                           \
        AP_Math/polygon.o                                              \
        AP_Math/matrix3.o                                              \
        AP_Math/location.o                                             \
        AP_Math/vector3.o                                              \
        AP_Math/vector2.o                                              \
        AP_Compass/Compass.o                                           \
        AP_Compass/AP_Compass_HMC5843.o                                \
        Filter/DerivativeFilter.o

$(eval $(call when_os,freertos,library,STANDALONE_APAHRS))

STANDALONE_HAL_LIB       := libstandalone-aphal.a

STANDALONE_HAL_INCLUDES  += -I$(TOP)/src/flight/standalone_apahrs

STANDALONE_HAL_CFLAGS    += $(STANDALONE_HAL_INCLUDES)
STANDALONE_HAL_CXXFLAGS  += $(STANDALONE_HAL_INCLUDES)
STANDALONE_HAL_CXXFLAGS  += -Wno-psabi

STANDALONE_HAL_OBJECTS := $(addprefix AP_HAL_SMACCM/,\
        RCInput.o                                    \
        SPIDriver.o                                  \
        Util.o                                       \
        Console.o                                    \
        Storage.o                                    \
        I2CDriver.o                                  \
        GPIO.o                                       \
        HAL_SMACCM_Class.o                           \
        UARTDriver.o                                 \
        Scheduler.o                                  \
        AnalogIn.o                                   \
        Semaphores.o                                 \
        RCOutput.o                                   \
        )

$(eval $(call when_os,freertos,library,STANDALONE_HAL))

# vim: set ft=make noet ts=2:

# -*- Mode: makefile-gmake; indent-tabs-mode: t; tab-width: 2 -*-
#
# build.mk --- Build rules for ArduPilot libraries.
#
# Copyright (C) 2012, Galois, Inc.
# All Rights Reserved.
#
# Written by James Bielman <jamesjb@galois.com>, December 07, 2012
#

ARDUPILOT_SRC    := $(CONFIG_ARDUPILOT_PREFIX)
ARDUPILOT_LIB    := libardupilot.a

ARDUPILOT_CXXFLAGS += $(FREERTOS_CFLAGS)
ARDUPILOT_CXXFLAGS += -I$(TOP)/hwf4/include
ARDUPILOT_CXXFLAGS += -I$(ARDUPILOT_SRC)/libraries/AP_HAL_SMACCM
ARDUPILOT_CXXFLAGS += -I$(ARDUPILOT_SRC)/libraries/AP_HAL/utility
ARDUPILOT_CXXFLAGS += $(addprefix -I,$(wildcard $(ARDUPILOT_SRC)/libraries/*))
ARDUPILOT_CXXFLAGS += -DCONFIG_HAL_BOARD=HAL_BOARD_SMACCM
ARDUPILOT_CXXFLAGS += -Wno-psabi
ARDUPILOT_CXXFLAGS += $(CONFIG_ARDUPILOT_EXTRA_CFLAGS)

ARDUPILOT_OBJECTS :=                                                    \
  libraries/AC_PID/AC_PID.o                                             \
  libraries/AP_ADC_AnalogSource/AP_ADC_AnalogSource.o                   \
  libraries/AP_ADC/AP_ADC_ADS7844.o                                     \
  libraries/AP_ADC/AP_ADC_HIL.o                                         \
  libraries/AP_ADC/AP_ADC.o                                             \
  libraries/AP_AHRS/AP_AHRS_DCM.o                                       \
  libraries/AP_AHRS/AP_AHRS_HIL.o                                       \
  libraries/AP_AHRS/AP_AHRS_MPU6000.o                                   \
  libraries/AP_AHRS/AP_AHRS.o                                           \
  libraries/AP_Airspeed/AP_Airspeed.o                                   \
  libraries/AP_Baro/AP_Baro_MS5611.o                                    \
  libraries/AP_Baro/AP_Baro_BMP085_hil.o                                \
  libraries/AP_Baro/AP_Baro.o                                           \
  libraries/AP_Camera/AP_Camera.o                                       \
  libraries/AP_Common/c++.o                                             \
  libraries/AP_Compass/AP_Compass_HIL.o                                 \
  libraries/AP_Compass/AP_Compass_HMC5843.o                             \
  libraries/AP_Compass/AP_Compass_PX4.o                                 \
  libraries/AP_Compass/Compass.o                                        \
  libraries/AP_Curve/AP_Curve.o                                         \
  libraries/AP_Declination/AP_Declination.o                             \
  libraries/AP_GPS/AP_GPS_406.o                                         \
  libraries/AP_GPS/AP_GPS_Auto.o                                        \
  libraries/AP_GPS/AP_GPS_HIL.o                                         \
  libraries/AP_GPS/AP_GPS_MTK16.o                                       \
  libraries/AP_GPS/AP_GPS_MTK19.o                                       \
  libraries/AP_GPS/AP_GPS_MTK.o                                         \
  libraries/AP_GPS/AP_GPS_NMEA.o                                        \
  libraries/AP_GPS/AP_GPS_SIRF.o                                        \
  libraries/AP_GPS/AP_GPS_UBLOX.o                                       \
  libraries/AP_GPS/GPS.o                                                \
  libraries/AP_HAL_SMACCM/AP_HAL_SMACCM_Main.o                          \
  libraries/AP_HAL_SMACCM/AnalogIn.o                                    \
  libraries/AP_HAL_SMACCM/Console.o                                     \
  libraries/AP_HAL_SMACCM/GPIO.o                                        \
  libraries/AP_HAL_SMACCM/HAL_SMACCM_Class.o                            \
  libraries/AP_HAL_SMACCM/I2CDriver.o                                   \
  libraries/AP_HAL_SMACCM/PrivateMember.o                               \
  libraries/AP_HAL_SMACCM/RCInput.o                                     \
  libraries/AP_HAL_SMACCM/RCOutput.o                                    \
  libraries/AP_HAL_SMACCM/Scheduler.o                                   \
  libraries/AP_HAL_SMACCM/Semaphores.o                                  \
  libraries/AP_HAL_SMACCM/SPIDriver.o                                   \
  libraries/AP_HAL_SMACCM/Storage.o                                     \
  libraries/AP_HAL_SMACCM/UARTDriver.o                                  \
  libraries/AP_HAL_SMACCM/Util.o                                        \
  libraries/AP_HAL/utility/Print.o                                      \
  libraries/AP_InertialNav/AP_InertialNav.o                             \
  libraries/AP_InertialSensor/AP_InertialSensor_MPU6000.o               \
  libraries/AP_InertialSensor/AP_InertialSensor.o                       \
  libraries/AP_InertialSensor/AP_InertialSensor_Oilpan.o                \
  libraries/AP_InertialSensor/AP_InertialSensor_PX4.o                   \
  libraries/AP_InertialSensor/AP_InertialSensor_Stub.o                  \
  libraries/AP_InertialSensor/AP_InertialSensor_UserInteract_Stream.o   \
  libraries/AP_LeadFilter/AP_LeadFilter.o                               \
  libraries/AP_Limits/AP_Limit_Altitude.o                               \
  libraries/AP_Limits/AP_Limit_Geofence.o                               \
  libraries/AP_Limits/AP_Limit_GPSLock.o                                \
  libraries/AP_Limits/AP_Limit_Module.o                                 \
  libraries/AP_Limits/AP_Limits.o                                       \
  libraries/AP_Math/AP_Math.o                                           \
  libraries/AP_Math/location.o                                          \
  libraries/AP_Math/matrix3.o                                           \
  libraries/AP_Math/polygon.o                                           \
  libraries/AP_Math/quaternion.o                                        \
  libraries/AP_Math/vector2.o                                           \
  libraries/AP_Math/vector3.o                                           \
  libraries/APM_Control/AP_PitchController.o                            \
  libraries/APM_Control/AP_RollController.o                             \
  libraries/APM_Control/AP_YawController.o                              \
  libraries/AP_Menu/AP_Menu.o                                           \
  libraries/APM_OBC/APM_OBC.o                                           \
  libraries/AP_Motors/AP_Motors_Class.o                                 \
  libraries/AP_Motors/AP_MotorsHeli.o                                   \
  libraries/AP_Motors/AP_MotorsHexa.o                                   \
  libraries/AP_Motors/AP_MotorsMatrix.o                                 \
  libraries/AP_Motors/AP_MotorsOcta.o                                   \
  libraries/AP_Motors/AP_MotorsOctaQuad.o                               \
  libraries/AP_Motors/AP_MotorsQuad.o                                   \
  libraries/AP_Motors/AP_MotorsTri.o                                    \
  libraries/AP_Motors/AP_MotorsY6.o                                     \
  libraries/AP_Mount/AP_Mount.o                                         \
  libraries/APM_PI/APM_PI.o                                             \
  libraries/AP_OpticalFlow/AP_OpticalFlow_ADNS3080.o                    \
  libraries/AP_OpticalFlow/AP_OpticalFlow.o                             \
  libraries/AP_Param/AP_Param.o                                         \
  libraries/AP_RangeFinder/AP_RangeFinder_MaxsonarI2CXL.o               \
  libraries/AP_RangeFinder/AP_RangeFinder_MaxsonarXL.o                  \
  libraries/AP_RangeFinder/AP_RangeFinder_SharpGP2Y.o                   \
  libraries/AP_RangeFinder/RangeFinder.o                                \
  libraries/AP_Relay/AP_Relay.o                                         \
  libraries/AP_Scheduler/AP_Scheduler.o                                 \
  libraries/DataFlash/DataFlash_APM1.o                                  \
  libraries/DataFlash/DataFlash_APM2.o                                  \
  libraries/DataFlash/DataFlash_Empty.o                                 \
  libraries/DataFlash/DataFlash.o                                       \
  libraries/DataFlash/DataFlash_SITL.o                                  \
  libraries/Filter/DerivativeFilter.o                                   \
  libraries/GCS_Console/GCS_Console.o                                   \
  libraries/GCS_MAVLink/GCS_MAVLink.o                                   \
  libraries/memcheck/memcheck.o                                         \
  libraries/PID/PID.o                                                   \
  libraries/RC_Channel/RC_Channel_aux.o                                 \
  libraries/RC_Channel/RC_Channel.o                                     \
  libraries/SITL/SITL.o

# Copy files from the Ardupilot source tree into our local tree.  Our
# build system doesn't make it easy to compile out-of-tree sources.
ardupilot/%.cpp: $(ARDUPILOT_SRC)/%.cpp
	mkdir -p $(dir $@)
	cp $< $@

ifdef CONFIG_ARDUPILOT_PREFIX
$(eval $(call library,ARDUPILOT))
endif

# vim: set ft=make noet ts=2:

/// -*- tab-width: 4; Mode: C++; c-basic-offset: 4; indent-tabs-mode: nil -*-
/*
 *   APM_Airspeed.cpp - airspeed (pitot) driver
 *
 *   This library is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU Lesser General Public License
 *   as published by the Free Software Foundation; either version 2.1
 *   of the License, or (at your option) any later version.
 */

#include <AP_HAL/AP_HAL.h>
#include <AP_Math/AP_Math.h>
#include <AP_Common/AP_Common.h>
#include "AP_Airspeed.h"

extern const AP_HAL::HAL& hal;

#define ARSPD_DEFAULT_PIN 0

// table of user settable parameters
const AP_Param::GroupInfo AP_Airspeed::var_info[] PROGMEM = {

    // @Param: ENABLE
    // @DisplayName: Airspeed enable
    // @Description: enable airspeed sensor
    // @Values: 0:Disable,1:Enable
    AP_GROUPINFO("ENABLE",    0, AP_Airspeed, _enable, 1),

    // @Param: USE
    // @DisplayName: Airspeed use
    // @Description: use airspeed for flight control
    // @Values: 1:Use,0:Don't Use
    AP_GROUPINFO("USE",    1, AP_Airspeed, _use, 0),

    // @Param: OFFSET
    // @DisplayName: Airspeed offset
    // @Description: Airspeed calibration offset
    // @Increment: 0.1
    AP_GROUPINFO("OFFSET", 2, AP_Airspeed, _offset, 0),

    // @Param: RATIO
    // @DisplayName: Airspeed ratio
    // @Description: Airspeed calibration ratio
    // @Increment: 0.1
    AP_GROUPINFO("RATIO",  3, AP_Airspeed, _ratio, 1.9936f),

    // @Param: PIN
    // @DisplayName: Airspeed pin
    // @Description: The analog pin number that the airspeed sensor is connected to. Set this to 0..9 for the APM2 analog pins. Set to 64 on an APM1 for the dedicated airspeed port on the end of the board. Set to 11 on PX4 for the analog airspeed port. Set to 65 on the PX4 for an EagleTree I2C airspeed sensor.
    // @User: Advanced
    AP_GROUPINFO("PIN",  4, AP_Airspeed, _pin, ARSPD_DEFAULT_PIN),

    // @Param: AUTOCAL
    // @DisplayName: Automatic airspeed ratio calibration
    // @Description: If this is enabled then the APM will automatically adjust the ARSPD_RATIO during flight, based upon an estimation filter using ground speed and true airspeed. The automatic calibration will save the new ratio to EEPROM every 2 minutes if it changes by more than 5%
    // @User: Advanced
    AP_GROUPINFO("AUTOCAL",  5, AP_Airspeed, _autocal, 0),

    AP_GROUPEND
};


/*
  this scaling factor converts from the old system where we used a 
  0 to 4095 raw ADC value for 0-5V to the new system which gets the
  voltage in volts directly from the ADC driver
 */
#define SCALING_OLD_CALIBRATION 819 // 4095/5

void AP_Airspeed::init()
{
    _last_pressure = 0;

    _source = hal.analogin->channel(_pin);

    _calibration.init(_ratio);
    _last_saved_ratio = _ratio;
    _counter = 0;
}

// read the airspeed sensor
float AP_Airspeed::get_pressure(void)
{
    if (!_enable) {
        return 0;
    }

    if (_source == NULL) {
        return 0;
    }
    _last_pressure = _source->voltage_average_ratiometric() * SCALING_OLD_CALIBRATION;
    return _last_pressure;
}

// calibrate the airspeed. This must be called at least once before
// the get_airspeed() interface can be used
void AP_Airspeed::calibrate()
{
    float sum = 0;
    uint8_t c;
    if (!_enable) {
        return;
    }
    // discard first reading
    get_pressure();
    for (c = 0; c < 10; c++) {
        hal.scheduler->delay(100);
        sum += get_pressure();
    }
    float raw = sum/c;
    _offset.set_and_save(raw);
    _airspeed = 0;
    _raw_airspeed = 0;
}

// read the airspeed sensor
void AP_Airspeed::read(void)
{
    float airspeed_pressure;
    if (!_enable) {
        return;
    }
    float raw               = get_pressure();
    airspeed_pressure       = max(raw - _offset, 0);
    _raw_airspeed           = sqrtf(airspeed_pressure * _ratio);
    _airspeed               = 0.7f * _airspeed  +  0.3f * _raw_airspeed;
}

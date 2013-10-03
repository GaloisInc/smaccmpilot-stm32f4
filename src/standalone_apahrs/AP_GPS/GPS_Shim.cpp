
#include "GPS_Shim.h"

GPS_Shim::GPS_Shim()
    : _lag(0.5) // Ublox Lag
    , _status(NO_GPS)
    , _velocity_north(0.0f)
    , _velocity_east(0.0f)
    , _velocity_down(0.0f)
{}

GPS::GPS_Status GPS_Shim::status(void) {
    return _status;
}

float GPS_Shim::get_lag() {
    return _lag;
}

// speed in m/s
float GPS_Shim::velocity_north(void) {
    return _velocity_north;
}

// speed in m/s
float GPS_Shim::velocity_east(void) {
    return _velocity_east;
}

// speed in m/s
float GPS_Shim::velocity_down(void) {
    return _velocity_down;
}

// latitude in degrees * 10,000,000
// longitude in degrees * 10,000,000
void GPS_Shim::set_lat_lon(int32_t lat, int32_t lon) {
    latitude = lat;
    longitude = lon;
}

// speed in cm/sec, course in 100ths of a degree
void GPS_Shim::set_ground_speed_course(uint32_t speed_cm, int32_t course_cd) {
    ground_speed_cm = speed_cm;
    ground_course_cd = course_cd;
}

// velocities in cm/sec, need to conver to floating m/sec
void GPS_Shim::set_velocity(int32_t v_north, int32_t v_east, int32_t v_down) {
    _velocity_north = ((float)v_north) / 100.0f;
    _velocity_east  = (float)v_east / 100.0f;
    _velocity_down  = (float)v_down / 100.0f;
}

// satellite fix
void GPS_Shim::set_fix(bool fix2d, bool fix3d, uint8_t sats) {
    if (fix3d) {
        _status = GPS_OK_FIX_3D;
    } else if (fix2d) {
        _status = GPS_OK_FIX_2D;
    } else {
        _status = NO_FIX;
    }
    num_sats = sats;
}


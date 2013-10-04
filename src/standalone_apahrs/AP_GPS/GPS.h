
// A simplified GPS class which presents the interfaces required
// by AP_AHRS.
// Pat Hickey, Galois Inc. 27 Sept 2013

#ifndef __GPS_H__
#define __GPS_H__

#include <stdint.h>

class GPS
{
public:
    enum GPS_Status {
        NO_GPS = 0,             ///< No GPS connected/detected
        NO_FIX = 1,             ///< Receiving valid GPS messages but no lock
        GPS_OK_FIX_2D = 2,      ///< Receiving valid messages and 2D lock
        GPS_OK_FIX_3D = 3       ///< Receiving valid messages and 3D lock
    };

    virtual GPS_Status status(void) = 0;
    virtual float get_lag() = 0; ///< the expected lag (in seconds) in the
                                 ///  position and velocity readings from the gps

    /// Components of the velocity in 3D, units meters/sec
    virtual float velocity_north(void) = 0;
    virtual float velocity_east(void) = 0;
    virtual float velocity_down(void) = 0;

    int32_t latitude;  ///< latitude in degrees * 10,000,000
    int32_t longitude; ///< longitude in degrees * 10,000,000

    uint32_t ground_speed_cm; ///< ground speed in cm/sec
    int32_t ground_course_cd; ///< ground course in 100ths of a degree

    uint8_t num_sats;       ///< Number of visible satelites
    uint32_t last_fix_time; ///< the time we got our last fix in system milliseconds

};

#endif // __GPS_H__


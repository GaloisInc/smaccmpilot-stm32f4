// Trivial shim implementing the GPS interface
// Pat Hickey, Galois Inc. 27 Sept 2013

#ifndef __GPS_SHIM_H__
#define __GPS_SHIM_H__

#include "GPS.h"

class GPS_Shim : public GPS
{
public:
    GPS_Shim();
    GPS::GPS_Status status(void);
    float get_lag();
    float velocity_north(void);
    float velocity_east(void);
    float velocity_down(void);

    // latitude, longitude in degrees * 10,000,000
    void set_lat_lon(int32_t lat, int32_t lon);

    // speed in cm/sec, course in 100ths of a degree
    void set_ground_speed_course(uint32_t speed_cm, int32_t course_cd);

    // velocity in cm/sec
    void set_velocity(int32_t v_north, int32_t v_east, int32_t v_down);

    // satellite fix
    void set_fix(bool fix2d, bool fix3d, uint8_t sats);

private:
    float _lag;
    GPS::GPS_Status _status;
    float _velocity_north;
    float _velocity_east;
    float _velocity_down;
};

#endif // __GPS_SHIM_H__


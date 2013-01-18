#ifndef __POSITION_H__
#define __POSITION_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
struct position_result {
    int32_t lat;
    int32_t lon;
    int32_t gps_alt;
    int16_t vx;
    int16_t vy;
    int16_t vz;
    int16_t xacc;
    int16_t yacc;
    int16_t zacc;
};

#ifdef __cplusplus
}
#endif
#endif /* __POSITION_H__ */

#ifndef __SMACCMPILOT_FLOAT_HELPER_IMPL_H__
#define __SMACCMPILOT_FLOAT_HELPER_IMPL_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

static inline int16_t  float_to_int16 (float f) { return (int16_t) f; }
static inline uint16_t float_to_uint16 (float f) { return (uint16_t) f; }
static inline float    int32_to_float(int32_t i) { return (float) i; }

#ifdef __cplusplus
}
#endif

#endif // __SMACCMPILOT_FLOAT_HELPER_IMPL_H__


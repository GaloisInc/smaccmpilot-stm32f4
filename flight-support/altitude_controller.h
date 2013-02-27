
#ifndef __SMACCMPILOT_ALTITUDE_CONTROLLER_H__
#define __SMACCMPILOT_ALTITUDE_CONTROLLER_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdbool.h>

#include "position_estimate_type.h"
#include "userinput_type.h"
#include "sensors_type.h"

void altitude_compensate(const struct position_estimate *pos,
                         const struct  sensors_result *sens,
                         const struct userinput_result *userin,
                         struct userinput_result *out);

float get_throttle_cruise_estimate(void);

#ifdef __cplusplus
}
#endif

#endif // __SMACCMPILOT_ALTITUDE_CONTROLLER_H__


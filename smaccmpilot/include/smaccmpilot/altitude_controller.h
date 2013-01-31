
#ifndef __SMACCMPILOT_ALTITUDE_CONTROLLER_H__
#define __SMACCMPILOT_ALTITUDE_CONTROLLER_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdbool.h>

#include "position_estimate_type.h"
#include "userinput_type.h"

void altitude_compensate(const struct position_estimate *pos,
                         const struct userinput_result *userin,
                         struct userinput_result *out);

#ifdef __cplusplus
}
#endif

#endif // __SMACCMPILOT_ALTITUDE_CONTROLLER_H__


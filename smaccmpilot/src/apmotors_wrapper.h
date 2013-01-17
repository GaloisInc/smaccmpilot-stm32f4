

#ifndef __SMACCMPILOT_APMOTORS_WRAPPER_H__
#define __SMACCMPILOT_APMOTORS_WRAPPER_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "smaccmpilot/motorsoutput.h"

void apmotors_output_init(void);
void apmotors_output_set(const struct motorsoutput_result *state);
void apmotors_output_get(struct servo_result *output);

#ifdef __cplusplus
}
#endif

#endif // __SMACCMPILOT_APMOTORS_WRAPPER_H__

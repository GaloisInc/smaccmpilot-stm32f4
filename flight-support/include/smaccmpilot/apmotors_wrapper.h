

#ifndef __SMACCMPILOT_APMOTORS_WRAPPER_H__
#define __SMACCMPILOT_APMOTORS_WRAPPER_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "smaccmpilot/controloutput_type.h"
#include "smaccmpilot/servos_type.h"
#include "smaccmpilot/flightmode_type.h"

void apmotors_output_init(void);
void apmotors_output_set(const struct controloutput *);
void apmotors_servo_get(struct servos *);

#ifdef __cplusplus
}
#endif

#endif // __SMACCMPILOT_APMOTORS_WRAPPER_H__

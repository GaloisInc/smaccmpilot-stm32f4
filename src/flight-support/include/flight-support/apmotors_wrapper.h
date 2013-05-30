

#ifndef __FLIGHT_SUPPORT_APMOTORS_WRAPPER_H__
#define __FLIGHT_SUPPORT_APMOTORS_WRAPPER_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "flight-generated/controloutput_type.h"
#include "flight-generated/servos_type.h"
#include "flight-generated/flightmode_type.h"

void apmotors_output_init(void);
void apmotors_output_set(const struct controloutput *,
                         const struct flightmode *);
void apmotors_servo_get(struct servos *);

#ifdef __cplusplus
}
#endif

#endif // __FLIGHT_SUPPORT_APMOTORS_WRAPPER_H__


#ifndef __FLIGHT_SUPPORT_SENSORS_CAPTURE_H__
#define __FLIGHT_SUPPORT_SENSORS_CAPTURE_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>
#include <stdint.h>

#include "smaccmpilot/sensors_type.h"

void sensors_begin(void);
void sensors_update(void);
void sensors_getstate(struct sensors_result *sensors);

#ifdef __cplusplus
}
#endif

#endif // __FLIGHT_SUPPORT_SENSORS_CAPTURE_H__


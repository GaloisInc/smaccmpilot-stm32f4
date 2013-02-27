
#ifndef __SMACCMPILOT_SENSORS_H__
#define __SMACCMPILOT_SENSORS_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>
#include <stdint.h>

#include "sensors_type.h"

void sensors_init(void);
void sensors_start_task(void);
void sensors_get(struct sensors_result *sensors);

#ifdef __cplusplus
}
#endif

#endif // __SMACCMPILOT_SENSORS_H__


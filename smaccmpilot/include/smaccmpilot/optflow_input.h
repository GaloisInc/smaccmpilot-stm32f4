
#ifndef __SMACCMPILOT_OPTFLOW_INPUT_H__
#define __SMACCMPILOT_OPTFLOW_INPUT_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdbool.h>

#include "optflow_type.h"

void optflow_input_init(void);
void optflow_input_start_task(void);
void optflow_input_get(struct optflow_result *optflow);

#ifdef __cplusplus
}
#endif

#endif // __SMACCMPILOT_OPTFLOW_INPUT_H__


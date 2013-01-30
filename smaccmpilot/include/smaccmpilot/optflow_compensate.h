
#ifndef __SMACCMPILOT_OPTFLOW_COMPENSATE_H__
#define __SMACCMPILOT_OPTFLOW_COMPENSATE_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdbool.h>

#include "optflow_type.h"
#include "userinput_type.h"

void optflow_compensate(struct optflow_result *optflow,
                        struct userinput_result *userin,
                        struct userinput_result *out);

#ifdef __cplusplus
}
#endif

#endif // __SMACCMPILOT_OPTFLOW_COMPENSATE_H__


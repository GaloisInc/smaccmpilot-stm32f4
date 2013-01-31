
#ifndef __ALT_HOLD_H__
#define __ALT_HOLD_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "optflow_type.h"

float alt_hold_controller(struct optflow_result *optflow, float user_throttle);

#ifdef __cplusplus
}
#endif

#endif // __ALT_HOLD_H__

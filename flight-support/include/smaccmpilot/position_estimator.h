
#ifndef __SMACCMPILOT_POSITION_ESTIMATOR_H__
#define __SMACCMPILOT_POSITION_ESTIMATOR_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "sensors_type.h"
#include "position_type.h"
#include "optflow_type.h"
#include "position_estimate_type.h"

void position_estimate(const struct sensors_result *sensors,
                       const struct position_result *position,
                       const struct optflow_result *optflow,
                             struct position_estimate *est );

void position_estimate_output( const struct position_estimate *est,
                                     struct position_result *position);

#ifdef __cplusplus
}
#endif

#endif // __SMACCMPILOT_POSITION_ESTIMATOR_H__

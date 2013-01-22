
#ifndef __SMACCMPILOT_GCS_RECEIVE_H__
#define __SMACCMPILOT_GCS_RECEIVE_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "sensors.h"
#include "position_type.h"

void gcs_receive_init(void);
bool gcs_receive_get_hilstate( struct sensors_result *sensors,
                               struct position_result *position );

#ifdef __cplusplus
}
#endif

#endif // __SMACCMPILOT_GCS_RECEIVE_H__


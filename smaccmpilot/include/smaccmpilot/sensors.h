
#ifndef __APP_STABILIZE_SENSORS_H__
#define __APP_STABILIZE_SENSORS_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <FreeRTOS.h>
#include <queue.h>

struct sensors_result {
  bool valid;
  /* roll, pitch, yaw in radians*/
  float roll;
  float pitch;
  float yaw;
  /* omega in radians per second */
  float omega_x;
  float omega_y;
  float omega_z;
  /* altitude in meters */
  float baro_alt;
};

void sensors_init(void);
void sensors_get(struct sensors_result *sensors);

#ifdef __cplusplus
}
#endif

#endif // __APP_STABILIZE_SENSORS_H__


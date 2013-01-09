
#ifndef __APP_STABILIZE_SENSORS_H__
#define __APP_STABILIZE_SENSORS_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <FreeRTOS.h>
#include <queue.h>

struct sensors_result {
  float roll;
  float pitch;
  float yaw;
  float omega_x;
  float omega_y;
  float omega_z;
  float baro_alt;
};

void sensors_init(void);

xQueueHandle get_sensors_queue(void);

#ifdef __cplusplus
}
#endif

#endif // __APP_STABILIZE_SENSORS_H__


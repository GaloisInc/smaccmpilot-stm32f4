
#ifndef __FLIGHT_SUPPORT_SENSORS_CAPTURE_H__
#define __FLIGHT_SUPPORT_SENSORS_CAPTURE_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>
#include <stdint.h>

void sensors_begin(bool flipped);
void sensors_update(void);

/* Write roll, pitch, yaw into an array of 3 floats
 * [ roll, pitch, yaw ]
 * units: radians
 */
void sensors_get_rpy(float *rpy);

/* Write omega (angular rate) into an array of 3 floats
 * [ omega_x, omega_y, omega_z ]
 * units: radians/sec
 */
void sensors_get_omega(float *omega);

/* Get barometric altitude estimate
 * unit: meters
 */
float sensors_get_baro_alt(void);

#ifdef __cplusplus
}
#endif

#endif // __FLIGHT_SUPPORT_SENSORS_CAPTURE_H__



#ifndef __FLIGHT_SUPPORT_USERINPUT_CAPTURE_H__
#define __FLIGHT_SUPPORT_USERINPUT_CAPTURE_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdbool.h>

bool userinput_capture(uint16_t *); /* should have 8 elems! */

#ifdef __cplusplus
}
#endif

#endif // __FLIGHT_SUPPORT_USERINPUT_CAPTURE_H__

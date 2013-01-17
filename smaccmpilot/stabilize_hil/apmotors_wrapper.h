

#ifndef __APP_STABILIZE_APMOTORS_WRAPPER_H__
#define __APP_STABILIZE_APMOTORS_WRAPPER_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "motorsoutput.h"

void apmotors_output_init(void);
void apmotors_output_set(const struct motorsoutput_result *state);

#ifdef __cplusplus
}
#endif

#endif // __APP_STABILIZE_APMOTORS_WRAPPER_H__

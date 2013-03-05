
#ifndef __MOTORSOUTPUT_H__
#define __MOTORSOUTPUT_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "smaccmpilot/motorsoutput_type.h"
#include "smaccmpilot/servo_type.h"

void motorsoutput_init(void);
void motorsoutput_start_task(void);
void motorsoutput_set(const struct motorsoutput_result *input);
void motorsoutput_getservo(struct servo_result *output);

#ifdef __cplusplus
}
#endif

#endif // __MOTORSOUTPUT_H__

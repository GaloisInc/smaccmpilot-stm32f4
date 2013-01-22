

#ifndef __APP_STABILIZE_USERINPUT_H__
#define __APP_STABILIZE_USERINPUT_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <FreeRTOS.h>

#include "userinput_type.h"

void userinput_init(void);
void userinput_get(struct userinput_result *input);

#ifdef __cplusplus
}
#endif

#endif // __APP_STABILIZE_USERINPUT_H__



#ifndef __APP_STABILIZE_USERINPUT_H__
#define __APP_STABILIZE_USERINPUT_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <FreeRTOS.h>

struct userinput_result {
    bool armed;
    float throttle;
    float roll;
    float pitch;
    float yaw;
    uint32_t time;
};

void userinput_init(void);
void userinput_get(struct userinput_result *input);

#ifdef __cplusplus
}
#endif

#endif // __APP_STABILIZE_USERINPUT_H__

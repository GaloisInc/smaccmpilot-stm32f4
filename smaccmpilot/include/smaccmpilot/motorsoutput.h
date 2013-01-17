
#ifndef __APP_STABILIZE_MOTORSOUTPUT_H__
#define __APP_STABILIZE_MOTORSOUTPUT_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <FreeRTOS.h>

struct motorsoutput_result {
    bool armed;
    float throttle;
    float roll;
    float pitch;
    float yaw;
    portTickType time;
};

void motorsoutput_init(void);
void motorsoutput_set(const struct motorsoutput_result *input);

#ifdef __cplusplus
}
#endif

#endif // __APP_STABILIZE_MOTORSOUTPUT_H__

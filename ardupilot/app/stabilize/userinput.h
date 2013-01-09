

#ifndef __APP_STABILIZE_USERINPUT_H__
#define __APP_STABILIZE_USERINPUT_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <FreeRTOS.h>
#include <queue.h>

struct userinput_result {
    bool armed;
    float throttle;
    float roll;
    float pitch;
    float yaw;
};

void userinput_init(void);

xQueueHandle get_userinput_queue(void);

#ifdef __cplusplus
}
#endif

#endif // __APP_STABILIZE_USERINPUT_H__

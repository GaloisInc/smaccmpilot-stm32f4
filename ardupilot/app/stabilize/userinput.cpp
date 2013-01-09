
#include "userinput.h"

#include <FreeRTOS.h>
#include <task.h>
#include <queue.h>

#include <AP_HAL.h>

#define PPM_MAX_CHANNELS 8

extern const AP_HAL::HAL& hal;

static xTaskHandle h_userinput_task;
static xQueueHandle q_userinput_result;

static void userinput_task(void* args);

void userinput_init(void) {
    if (h_userinput_task == NULL) {
        xTaskCreate(userinput_task, (signed char *)"user", 1024, NULL, 0,
                &h_userinput_task);
    }
}

xQueueHandle get_userinput_queue(void) {
    return q_userinput_result;
}


void userinput_task(void* args) {
    for(;;) {
        if (hal.rcin->valid()) {
            uint16_t ppm[PPM_MAX_CHANNELS];
            size_t count;

            count = hal.rcin->read(ppm, PPM_MAX_CHANNELS);

            hal.console->write("ppm:         ");
            for (size_t i = 0; i < count; ++i)
                hal.console->printf("%u ", ppm[i]);
            hal.console->write("\r\n");
        }
        vTaskDelay(10);
    }
}

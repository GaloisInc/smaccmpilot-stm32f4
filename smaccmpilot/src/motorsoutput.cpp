
#include "smaccmpilot/motorsoutput.h"
#include "apmotors_wrapper.h"

#include <FreeRTOS.h>
#include <task.h>
#include <semphr.h>

#include <AP_HAL.h>
extern const AP_HAL::HAL& hal;

static xTaskHandle motorsoutput_task_handle;
static xSemaphoreHandle motorsoutput_mutex;
static struct motorsoutput_result motorsoutput_shared_state;

static void motorsoutput_task(void* args);
static void motorsoutput_get(struct motorsoutput_result *state);
static void motorsoutput_failsafe(struct motorsoutput_result *state);

void motorsoutput_init(void) {
    apmotors_output_init();

    motorsoutput_mutex = xSemaphoreCreateMutex();
}

void motorsoutput_start_task(void) {
    xTaskCreate(motorsoutput_task, (signed char *)"motors", 1024, NULL, 0,
                &motorsoutput_task_handle);
}

static void motorsoutput_task(void* args) {

    struct motorsoutput_result state = {0};

    portTickType last_wake_time = xTaskGetTickCount();

    for(;;) {
        vTaskDelayUntil(&last_wake_time, 10);
        motorsoutput_get(&state);
        motorsoutput_failsafe(&state);
        apmotors_output_set(&state);
    }
}

/* motorsoutput_get: for internal thread to grab the shared state */
static void motorsoutput_get(struct motorsoutput_result *state) {
    if (xSemaphoreTake(motorsoutput_mutex, 1)) {
        memcpy(state, &motorsoutput_shared_state,
                sizeof(struct motorsoutput_result));
        xSemaphoreGive(motorsoutput_mutex);
    } else {
        struct motorsoutput_result panicstate = {0}; /* disarmed & all zero */
        apmotors_output_set(&panicstate);
        hal.scheduler->panic("PANIC: motorsoutput_get took too long to grab "
                "memory barrier (should never happen). motors shut down!");
    }
}

/* motorsoutput_set: for external threads to update the shared state */
void motorsoutput_set(const struct motorsoutput_result *out) {
    if (xSemaphoreTake(motorsoutput_mutex, 1)) {
        memcpy(&motorsoutput_shared_state, out,
                sizeof(struct motorsoutput_result));
        xSemaphoreGive(motorsoutput_mutex);
    } else {
        hal.scheduler->panic("PANIC: motorsoutput_set took too long to grab "
                "memory barrier (should never happen).");
    }
}

void motorsoutput_getservo(struct servo_result *output) {
    apmotors_output_get(output);
}

static void motorsoutput_failsafe(struct motorsoutput_result *state) {
    portTickType now = xTaskGetTickCount();
    portTickType dt = now - state->time;
    if (dt > 200) {
        state->armed = false;
    }
}



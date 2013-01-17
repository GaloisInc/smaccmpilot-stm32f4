
#include "userinput.h"

#include <hwf4/timer.h>

#include <FreeRTOS.h>
#include <task.h>
#include <semphr.h>

#include <AP_HAL.h>
#include <AP_HAL_SMACCM.h>
/* Use AP_HAL_SMACCM_Private for definition of SMACCM_RCINPUT_CHANNELS */
#include <AP_HAL_SMACCM_Private.h>

#if SMACCM_RCINPUT_CHANNELS < 6
#error Expect SMACCM_RCINPUT_CHANNELS to be at least 6
#endif

extern const AP_HAL::HAL& hal;

static xTaskHandle userinput_task_handle;
static xSemaphoreHandle userinput_mutex;
static struct userinput_result userinput_shared_state;

static void userinput_capture(struct userinput_result *capt);
static void userinput_failsafe(struct userinput_result *capt);
static void userinput_share(const struct userinput_result *capt);

static void userinput_task(void* args);

void userinput_init(void) {
    userinput_mutex = xSemaphoreCreateMutex();
    xTaskCreate(userinput_task, (signed char *)"user", 1024, NULL, 0,
                &userinput_task_handle);
}

static void userinput_task(void* args) {

    struct userinput_result state = {0};

    portTickType last_wake_time = xTaskGetTickCount();

    for(;;) {
        vTaskDelayUntil(&last_wake_time, 20);
        userinput_capture(&state);
        userinput_failsafe(&state);
        userinput_share(&state);
    }
}

/* userinput_get: for external threads to grab the shared state */
void userinput_get(struct userinput_result *input) {
    if (xSemaphoreTake(userinput_mutex, 1)) {
        memcpy(input, &userinput_shared_state, sizeof(struct userinput_result));
        xSemaphoreGive(userinput_mutex);
    } else {
        hal.scheduler->panic("PANIC: userinput_get took too long to take "
                "memory barrier");
    }
}

/* userinput_get: for internal thread to update the shared state */
static void userinput_share(const struct userinput_result *capt) {
    if (xSemaphoreTake(userinput_mutex, 1)) {
        memcpy(&userinput_shared_state, capt, sizeof(struct userinput_result));
        xSemaphoreGive(userinput_mutex);
    } else {
        hal.scheduler->panic("PANIC: userinput_share took too long to take "
                "memory barrier");
    }
}

static float fit_ch(uint16_t input, uint16_t center, uint16_t range,
        float min, float max) {
    int32_t centered = input - center;
    float    ranged  = centered / (float) range;
    if (ranged < min) return min;
    if (ranged > max) return max;
    return ranged;

}

static void userinput_capture(struct userinput_result *capt) {
    if (hal.rcin->valid()) {
        uint16_t ch[SMACCM_RCINPUT_CHANNELS];
        size_t count;

        count = hal.rcin->read(ch, SMACCM_RCINPUT_CHANNELS);

        if (count == SMACCM_RCINPUT_CHANNELS) {
            /* Update capt.time, recordkeeping for internal failsafe */
            capt->time     = xTaskGetTickCount();
            /* Update each captured channel */
            capt->roll     = fit_ch(ch[0], 1500, 500, -1.0f, 1.0f );
            capt->pitch    = fit_ch(ch[1], 1500, 500, -1.0f, 1.0f );
            capt->throttle = fit_ch(ch[2], 1000, 1000, 0.0f, 1.0f );
            capt->yaw      = fit_ch(ch[3], 1500, 500, -1.0f, 1.0f );

            /* Motor arming is mapped to channel 5 as a single switch, for
             * simplicity. */
            if (ch[5] > 1500) {
                capt->armed = true;
            } else {
                capt->armed = false;
            }
        }

    }
}

static void userinput_failsafe(struct userinput_result *capt) {
    portTickType now = xTaskGetTickCount();
    portTickType dt  = now - capt->time;
    if (dt > 150) {
        /* Failsafe values in case of lost signal: */
        capt->armed    = false;
        capt->throttle = 0.0f;
        capt->yaw      = 0.0f;
        capt->pitch    = 0.0f;
        capt->roll     = 0.0f;
    }
}

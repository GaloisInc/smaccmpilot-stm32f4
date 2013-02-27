
#include <FreeRTOS.h>
#include <task.h>

#include <smaccmpilot/ioar_relay.h>
#include <hwf4/gpio.h>

#define PATTERN_LEN 8
#define PATTERN_DT  125

#define IOAR_RELAY_PIN pin_b13

static volatile int shared_state = IOAR_RELAY_OFF;

static const uint8_t pattern[IOAR_RELAY_NUM_PATTERNS][PATTERN_LEN] =
    { { 0, 0, 0, 0, 0, 0, 0, 0 } /* OFF */
    , { 1, 1, 1, 1, 1, 1, 1, 1 } /* ON */
    , { 0, 0, 1, 1, 1, 1, 1, 1 } /* BLINK_SLOW  -- 75% duty */
    , { 0, 1, 1, 1, 0, 1, 1, 1 } /* BLINK_FAST  -- 75% duty */
    , { 0, 0, 0, 0, 0, 0, 0, 1 } /* PULSE_SLOW  -- 25% duty */
    , { 0, 0, 0, 1, 0, 0, 0, 1 } /* PULSE_FAST  -- 25% duty */
    , { 0, 1, 0, 1, 0, 1, 0, 1 } /* PULSE_EXTRA_FAST  -- 50% duty */
    };

static xTaskHandle ioar_relay_task_handle;
static void ioar_relay_task(void* args);

void ioar_relay_init(void) {
    pin_enable    (IOAR_RELAY_PIN);
    pin_set_otype (IOAR_RELAY_PIN, PIN_TYPE_PUSHPULL);
    pin_set_ospeed(IOAR_RELAY_PIN, PIN_SPEED_2MHZ);
    pin_set_pupd  (IOAR_RELAY_PIN, PIN_PUPD_NONE);
    pin_reset     (IOAR_RELAY_PIN);
    pin_set_mode  (IOAR_RELAY_PIN, PIN_MODE_OUTPUT);
}

void ioar_relay_set(int state) {
    shared_state = state;
}

void ioar_relay_start_task(void) {
    xTaskCreate(ioar_relay_task, (signed char *)"iore", 128, NULL, 0,
                &ioar_relay_task_handle);
}

void ioar_relay_task(void* args) {
    uint32_t last_wake_time = xTaskGetTickCount();
    int pattern_phase;
    for(;;) {
        int pnum = shared_state;
        if (pnum < 0 || pnum >= IOAR_RELAY_NUM_PATTERNS)
            pnum = 0;
        const bool out = pattern[pnum][pattern_phase];

        if (out) {
            pin_reset(IOAR_RELAY_PIN);
        } else {
            pin_set(IOAR_RELAY_PIN);
        }

        pattern_phase++;
        if (pattern_phase == PATTERN_LEN) {
            pattern_phase = 0;
        }

        vTaskDelayUntil(&last_wake_time, PATTERN_DT);
    }
}

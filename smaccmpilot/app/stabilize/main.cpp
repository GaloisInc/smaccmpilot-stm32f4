/*
 * main.cpp --- AP_HAL based helicopter stabilizer
 *
 * Copyright (C) 2012, Galois, Inc.
 * All Rights Reserved.
 *
 * This software is released under the "BSD3" license.  Read the file
 * "LICENSE" for more information.
 */

#include <stdint.h>

#include <FreeRTOS.h>
#include <task.h>

#include <hwf4/led.h>
#include <hwf4/gpio.h>
#include <hwf4/timer.h>

#if CONFIG_HAL_BOARD == HAL_BOARD_SMACCM
# include <AP_HAL_SMACCM.h>
#else
# error "Unsupported CONFIG_HAL_BOARD type."
#endif

#include <smaccmpilot/sensors.h>
#include <smaccmpilot/userinput.h>
#include <smaccmpilot/motorsoutput.h>

#include "gcs.h"

const AP_HAL::HAL& hal = AP_HAL_BOARD_DRIVER;

static xTaskHandle main_task_handle;

static void debug_sensors(void);
static void debug_userinput(void);

static void input_to_motors(const struct userinput_result *input,
                           struct motorsoutput_result *output) {
    output->armed    = input->armed;
    output->throttle = input->throttle;
    output->roll     = input->roll;
    output->pitch    = input->pitch;
    output->yaw      = input->yaw;
    output->time     = xTaskGetTickCount();
}

static void input_to_gcs(const struct userinput_result *input,
                           struct gcs_state *gcs) {
    gcs->armed = input->armed;
}

void main_task(void *args)
{
    vTaskSetApplicationTaskTag(xTaskGetIdleTaskHandle(), (pdTASK_HOOK_CODE)1);
    vTaskSetApplicationTaskTag(NULL, (pdTASK_HOOK_CODE)2);

    led_init();

    hal.init(0, NULL);
    hal.console->println("beginning stabilize app...");

    sensors_init();
    userinput_init();
    motorsoutput_init();
    gcs_init();

    for(;;) {
        // debug_userinput();
        // debug_sensors();
       
        struct userinput_result input;
        struct motorsoutput_result motors;
        struct gcs_state gcs;
       
        userinput_get(&input);
        input_to_motors(&input, &motors);
        input_to_gcs(&input, &gcs);
        motorsoutput_set(&motors);

        vTaskDelay(100);
    }

    for(;;);
}

extern "C" int main(void)
{
    xTaskCreate(main_task, (signed char *)"main", 1024, NULL, 0,
            &main_task_handle);
    vTaskStartScheduler();
    for (;;);
    return 0;
}

static float rad2deg(float rad) {
    return (rad * 180.0f / 3.14159f);
}

static void debug_sensors(void) {
    struct sensors_result sensors;
    sensors_get(&sensors);
    if (sensors.valid) {
        hal.console->printf(
                "sensors roll %f pitch %f yaw %f alt %f\r\n",
                rad2deg(sensors.roll),
                rad2deg(sensors.pitch),
                rad2deg(sensors.yaw),
                sensors.baro_alt
                );
    } else {
        hal.console->printf(
                "sensors calibrating...\r\n" 
                );
    }
}

static void debug_userinput(void) {
    struct userinput_result userin;
    userinput_get(&userin);
    hal.console->printf(
            "userinput %s; roll %f pitch %f yaw %f throttle %f\r\n",
            userin.armed ? "armed" : "disarmed",
            userin.roll, userin.pitch, userin.yaw, userin.throttle
            );
}

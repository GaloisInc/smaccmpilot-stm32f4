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

#include "sensors.h"
#include "userinput.h"

const AP_HAL::HAL& hal = AP_HAL_BOARD_DRIVER;

static xTaskHandle h_main_task;

static void debug_sensors(void);
static void debug_userinput(void);

void main_task(void *args)
{
    vTaskSetApplicationTaskTag(xTaskGetIdleTaskHandle(), (pdTASK_HOOK_CODE)1);
    vTaskSetApplicationTaskTag(NULL, (pdTASK_HOOK_CODE)2);

    led_init();

    hal.init(0, NULL);
    hal.console->println("beginning stabilize app...");

    sensors_init();
    userinput_init();


    for(;;) {
        // debug_userinput();
        debug_sensors();
        vTaskDelay(100);
    }

    for(;;);
}

extern "C" int main(void)
{
    xTaskCreate(main_task, (signed char *)"main", 1024, NULL, 0, &h_main_task);
    vTaskStartScheduler();
    for (;;);
    return 0;
}

static float rad2deg(float rad) {
    return (rad * 180.0f / 3.14159f);
}

static void debug_sensors(void) {
    struct sensors_result sensors;
    bool res = sensors_get(&sensors, 1);
    if (res) {
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

    } else {
        hal.console->println("ERROR: sensors get failed");
    }

}

static void debug_userinput(void) {
    struct userinput_result userin;
    bool res = userinput_get(&userin, 1);
    if (res) {
        hal.console->printf(
                "userinput %s; roll %f pitch %f yaw %f throttle %f\r\n",
                userin.armed ? "armed" : "disarmed",
                userin.roll, userin.pitch, userin.yaw, userin.throttle
                );
    } else {
        hal.console->println("ERROR: userinput get failed");
    }
}

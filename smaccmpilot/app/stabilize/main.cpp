// -*- Mode: C++; indent-tabs-mode: nil; c-basic-offset: 4 -*-
/*
 * main.cpp --- AP_HAL HIL based helicopter stabilizer
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

#include <AP_HAL_SMACCM.h>
#include <AP_Math.h>

#include <smaccmpilot/userinput.h>
#include <smaccmpilot/motorsoutput.h>
#include <smaccmpilot/stabilize.h>
#include <smaccmpilot/gcs_receive.h>
#include <smaccmpilot/gcs_transmit.h>
#include <smaccmpilot/sensors.h>

const AP_HAL::HAL& hal = AP_HAL_BOARD_DRIVER;

// Handle to the main thread.
static xTaskHandle g_main_task;

// Initialize the HAL and sub-tasks before the main loop.
void init(void)
{
    hal.init(0, NULL);

    userinput_init();
#ifndef USE_HIL
    sensors_init();
#endif
    motorsoutput_init();
    gcs_receive_init();
    gcs_transmit_init();

    userinput_start_task();
#ifndef USE_HIL
    sensors_start_task();
#endif
    motorsoutput_start_task();
    gcs_receive_start_task();
    gcs_transmit_start_task();
}

// Main thread.  Starts up the GCS thread to communicate with the
// host, then processes incoming sensor data and writes servo output
// back to MAVLink.
void main_task(void *arg)
{
    struct userinput_result input;
    struct sensors_result sensors;
    struct motorsoutput_result motors;
    struct position_result position;
    struct servo_result servos;

    init();
    memset(&position, 0, sizeof(position));
    portTickType last_wake_time = xTaskGetTickCount();

    for (;;) {
        userinput_get(&input);

#ifdef USE_HIL
        gcs_receive_get_hilstate(&sensors, &position);
#else
        sensors_get(&sensors);
#endif

        stabilize_motors(&input, &sensors, &motors);
        motorsoutput_set(&motors);

        motorsoutput_getservo(&servos);
        gcs_transmit_set_states(&sensors, &position, &motors, &servos, &input);

        vTaskDelayUntil(&last_wake_time, 10);
    }
}

extern "C"
int main()
{
    xTaskCreate(main_task, (signed char *)"main", 1024, NULL, 0, &g_main_task);
    vTaskStartScheduler();

    for (;;)
        ;

    return 0;
}

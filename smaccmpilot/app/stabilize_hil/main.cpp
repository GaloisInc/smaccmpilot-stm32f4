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

const AP_HAL::HAL& hal = AP_HAL_BOARD_DRIVER;

// Handle to the main thread.
static xTaskHandle g_main_task;

// Main thread.  Starts up the GCS thread to communicate with the
// host, then processes incoming sensor data and writes servo output
// back to MAVLink.
void main_task(void *arg)
{
    hal.init(0, NULL);
    userinput_init();
    motorsoutput_init();
    gcs_receive_init();
    gcs_transmit_init();

    struct userinput_result input;
    struct sensors_result sensors;
    struct position_result position;
    struct motorsoutput_result motors;
    struct servo_result servos;

    portTickType last_wake_time = xTaskGetTickCount();
    for (;;) {

        userinput_get(&input);

        gcs_receive_get_hilstate(&sensors, &position);

        stabilize_motors(&input, &sensors, &motors);

        motorsoutput_set(&motors);

        motorsoutput_getservo(&servos);

        gcs_transmit_set_states(&sensors, &position, &motors, &servos);

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

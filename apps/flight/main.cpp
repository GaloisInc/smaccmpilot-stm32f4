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
#include <ctype.h>

#include <FreeRTOS.h>
#include <task.h>

#include <AP_HAL_SMACCM/AP_HAL_SMACCM.h>
#include <AP_Math/AP_Math.h>

#include "tower.h"

const AP_HAL::HAL& hal = AP_HAL_SMACCM;

// Handle to the main thread.
static xTaskHandle g_main_task;

// Main task. Start the AP_HAL, then start tower, then delete self.
void main_task(void)
{
    hal.init(0, NULL);
    tower_entry();
    vTaskDelete(g_main_task);
    for (;;) {}
}

extern "C"
int main()
{
    xTaskCreate(main_task, (signed char *)"main", 1024, NULL,
            (configMAX_PRIORITIES - 1), &g_main_task);
    vTaskStartScheduler();

    for (;;)
        ;

    return 0;
}

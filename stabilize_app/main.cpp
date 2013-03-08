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

#include <smaccmpilot/sensors.h>
#include <smaccmpilot/motorsoutput.h>
#include <ctype.h>
#include <smaccmpilot/storage_eeprom.h>
#include <smaccmpilot/storage_partition.h>

#include <smaccmpilot/tower.h>

const AP_HAL::HAL& hal = AP_HAL_BOARD_DRIVER;

// Handle to the main thread.
static xTaskHandle g_main_task;

// Hex dump the contents of a partition to the console.
void dump_partition(int pid)
{
    static uint8_t buf[512];
    uint16_t size = partition_size(pid);

    if (size > sizeof(buf))
        size = sizeof(buf);

    if (!partition_read(pid, 0, buf, size)) {
        hal.console->printf("error: reading partition %d failed\r\n", pid);
        return;
    }

    hal.console->printf("\r\nDump of partition %d:\r\n", pid);

    for (uint16_t i = 0; i < size; i += 16) {
        hal.console->printf("%04x:", i);

        for (uint16_t j = 0; j < 16; ++j) {
            if ((j % 8) == 0) {
                hal.console->write(" ");
            }

            hal.console->printf("%02x ", buf[i+j]);
        }

        hal.console->write("  |");

        for (uint16_t j = 0; j < 16; ++j) {
            if (isprint(buf[i+j]))
                hal.console->printf("%c", buf[i+j]);
            else
                hal.console->write('.');
        }

        hal.console->write("|\r\n");
    }
}

// Initialize the HAL and sub-tasks before the main loop.
void init(void)
{
    hal.init(0, NULL);

    sensors_init();
    motorsoutput_init();

    param_load();               // XXX param_init

    sensors_start_task();
    motorsoutput_start_task();

    tower_entry();
}

// Main thread.  Starts up the GCS thread to communicate with the
// host, then processes incoming sensor data and writes servo output
// back to MAVLink.
void main_task(void *arg)
{
    init();
    for (;;) {}
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

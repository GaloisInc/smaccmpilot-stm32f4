// -*- Mode: C++; indent-tabs-mode: nil; c-basic-offset: 4 -*-
/*
 * main.c --- Tower/FreeRTOS Initializer
 *
 * Copyright (C) 2013, Galois, Inc.
 * All Rights Reserved.
 *
 * This software is released under the "BSD3" license.  Read the file
 * "LICENSE" for more information.
 */

#include <stdint.h>

#include <FreeRTOS.h>
#include <task.h>
#include <tower.h>

void main_task(void *arg)
{
    tower_entry();
    for (;;) {}
}

int main()
{
    xTaskCreate(main_task, (signed char *)"main", 256, NULL, 0, NULL);
    vTaskStartScheduler();

    for (;;)
        ;

    return 0;
}

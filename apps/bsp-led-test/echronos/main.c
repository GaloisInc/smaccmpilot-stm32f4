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

#include <eChronos.h>
#include <rtos-kochab.h>
#include <stdint.h>
#include <tower.h>


int main()
{
    tower_entry();
    vTaskStartScheduler();

    for (;;)
        ;

    return 0;
}




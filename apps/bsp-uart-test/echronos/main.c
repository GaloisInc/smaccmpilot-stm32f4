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
#include <tower.h>



void main_task(void *arg)
{
    tower_entry();
    while(1){
        rtos_sem_wait(SEM_ID_init_sem);
    }
}

extern void main_task(void *arg);

int main()
{
    vTaskStartScheduler();
    for (;;)
        ;

    return 0;
}


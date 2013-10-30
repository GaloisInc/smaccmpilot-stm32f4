/* -*- Mode: C++; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/*
 * Scheduler.cpp --- AP_HAL_SMACCM scheduler.
 *
 * Copyright (C) 2012, Galois, Inc.
 * All Rights Reserved.
 *
 * This software is released under the "BSD3" license.  Read the file
 * "LICENSE" for more information.
 *
 * Written by James Bielman <jamesjb@galois.com>, 20 December 2012
 */

#include <hwf4/gpio.h>
#include <hwf4/timer.h>

#include <FreeRTOS.h>
#include <task.h>
#include <semphr.h>

#include "Scheduler.h"

using namespace SMACCM;

extern const AP_HAL::HAL& hal;

/** Rate in milliseconds of timed process execution. (1kHz) */
#define SCHEDULER_TICKS  (1 / (portTICK_RATE_MS))

/** Stack size of the scheduler thread. */
#define SCHEDULER_STACK_SIZE 1536

/** Priority of the scheduler timer process task. */
#define SCHEDULER_PRIORITY (configMAX_PRIORITIES - 1)

/** Rate in milliseconds of the delay callback task. */
#define DELAY_CB_TICKS (1 / (portTICK_RATE_MS))

/** Stack size of the delay callback task. */
#define DELAY_CB_STACK_SIZE 512

/** Priority of the delay callback task. */
#define DELAY_CB_PRIORITY 0

/**
 * Recursive mutex used to block "scheduler_task" during atomic
 * sections.
 */
static xSemaphoreHandle g_atomic;

/** High-priority thread managing timer procedures. */
static void scheduler_task(void *arg)
{
  SMACCMScheduler *sched = (SMACCMScheduler *)arg;
  portTickType last_wake_time;
  portTickType now;

  vTaskSetApplicationTaskTag(NULL, (pdTASK_HOOK_CODE)3);
  last_wake_time = xTaskGetTickCount();

  for (;;) {
    /* If "vTaskDelayUntil" would return immediately without blocking,
     * call the failsafe callback to notify the client that we've
     * missed our deadline, and reset the wakeup time to the current
     * time. */
    now = xTaskGetTickCount();
    if (last_wake_time + SCHEDULER_TICKS <= now) {
      sched->run_failsafe_cb();
      last_wake_time = now;
    } else {
      vTaskDelayUntil(&last_wake_time, SCHEDULER_TICKS);

      xSemaphoreTakeRecursive(g_atomic, portMAX_DELAY);
      sched->run_callbacks();
      xSemaphoreGiveRecursive(g_atomic);
    }
  }
}

SMACCMScheduler::SMACCMScheduler()
  : m_task(NULL),
    m_failsafe_cb(NULL),
    m_num_procs(0),
    m_initializing(true)
{
}

void SMACCMScheduler::init(void *arg)
{
  timer_init();

  g_atomic = xSemaphoreCreateRecursiveMutex();

  xTaskCreate(scheduler_task, (signed char *)"scheduler",
              SCHEDULER_STACK_SIZE, this, SCHEDULER_PRIORITY,
              &m_task);
}

void SMACCMScheduler::delay(uint16_t ms)
{
  timer_msleep(ms);
}

uint32_t SMACCMScheduler::millis()
{
  return (uint32_t)(timer_get_ticks() / 1000ULL);
}

// XXX this is going to wrap every 1.1 hours
uint32_t SMACCMScheduler::micros()
{
  return (uint32_t)timer_get_ticks();
}

void SMACCMScheduler::register_timer_process(AP_HAL::TimedProc k)
{
  for (int i = 0; i < m_num_procs; ++i) {
    if (m_procs[i] == k)
      return;
  }

  if (m_num_procs < SMACCM_SCHEDULER_MAX_TIMER_PROCS) {
    portENTER_CRITICAL();
    m_procs[m_num_procs] = k;
    ++m_num_procs;
    portEXIT_CRITICAL();
  }
}

void SMACCMScheduler::register_io_process(AP_HAL::TimedProc k)
{
}

void SMACCMScheduler::register_timer_failsafe(AP_HAL::TimedProc k, uint32_t)
{
  m_failsafe_cb = k;
}

void SMACCMScheduler::suspend_timer_procs()
{
  xSemaphoreTakeRecursive(g_atomic, portMAX_DELAY);
}

void SMACCMScheduler::resume_timer_procs()
{
  xSemaphoreGiveRecursive(g_atomic);
}

void SMACCMScheduler::begin_atomic()
{
}

void SMACCMScheduler::end_atomic()
{
}

void SMACCMScheduler::panic(const prog_char_t *errormsg)
{
  hal.console->println_P(errormsg);

  // Try to grab "g_atomic" to suspend timer processes, but with a
  // timeout in case a timer proc is locked up.
  xSemaphoreTakeRecursive(g_atomic, 10);

  for(;;)
    ;
}

void SMACCMScheduler::reboot()
{
  for(;;)
    ;
}

void SMACCMScheduler::run_callbacks()
{
  uint32_t now = micros();

  // Run timer processes if not suspended.
  portENTER_CRITICAL();
  uint8_t num_procs = m_num_procs;
  portEXIT_CRITICAL();

  for (int i = 0; i < num_procs; ++i) {
    if (m_procs[i] != NULL) {
      m_procs[i](now);
    }
  }
}

void SMACCMScheduler::run_failsafe_cb()
{
  if (m_failsafe_cb)
    m_failsafe_cb(micros());
}

/** Return true if in the context of a timer process. */
bool SMACCMScheduler::in_timerprocess()
{
  return (xTaskGetCurrentTaskHandle() == m_task);
}

/** Return true if the system is initializing. */
bool SMACCMScheduler::system_initializing()
{
  return m_initializing;
}

/** Set the system initializing flag to false. */
void SMACCMScheduler::system_initialized()
{
  m_initializing = false;
}


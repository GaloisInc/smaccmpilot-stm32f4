/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
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

#include <hwf4/timer.h>

#include <FreeRTOS.h>
#include <task.h>

#include "Scheduler.h"

using namespace SMACCM;

extern const AP_HAL::HAL& hal;

/** Rate in milliseconds of timed process execution. (1kHz) */
#define SCHEDULER_TICKS  (1 / (portTICK_RATE_MS))

/** Stack size of the scheduler thread. */
#define SCHEDULER_STACK_SIZE 256

/** FreeRTOS thread managing timer procedures. */
static void scheduler_task(void *arg)
{
  SMACCMScheduler *sched = (SMACCMScheduler *)arg;
  portTickType last_wake_time;

  last_wake_time = xTaskGetTickCount();

  for (;;) {
    vTaskDelayUntil(&last_wake_time, SCHEDULER_TICKS);
    sched->run_callbacks();
  }
}

SMACCMScheduler::SMACCMScheduler()
  : m_delay_cb(NULL), m_delay_cb_ms(0), m_suspended(false),
    m_task(NULL), m_deferred_proc(NULL), m_num_procs(0)
{
}

void SMACCMScheduler::init(void *arg)
{
  timer_init();

  xTaskCreate(scheduler_task, (signed char *)"scheduler",
              SCHEDULER_STACK_SIZE, this, 0, &m_task);
}

void SMACCMScheduler::delay(uint32_t ms)
{
  while (ms > 0) {
    timer_msleep(1);
    --ms;

    if (m_delay_cb && m_delay_cb_ms <= ms)
      m_delay_cb();
  }
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

void SMACCMScheduler::delay_microseconds(uint16_t us)
{
  timer_usleep(us);
}

void SMACCMScheduler::register_delay_callback(AP_HAL::Proc k, uint16_t min_time_ms)
{
  m_delay_cb = k;
  m_delay_cb_ms = min_time_ms;
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

bool SMACCMScheduler::defer_timer_process(AP_HAL::TimedProc k)
{
  // It is possible that this might be atomic enough that we don't
  // need a critical section here.
  portENTER_CRITICAL();
  m_deferred_proc = k;
  portEXIT_CRITICAL();

  return true;
}

void SMACCMScheduler::register_timer_failsafe(AP_HAL::TimedProc, uint32_t)
{
}

void SMACCMScheduler::suspend_timer_procs()
{
  m_suspended = true;
}

void SMACCMScheduler::resume_timer_procs()
{
  m_suspended = false;
}

void SMACCMScheduler::begin_atomic()
{
  portENTER_CRITICAL();
}

void SMACCMScheduler::end_atomic()
{
  portEXIT_CRITICAL();
}

void SMACCMScheduler::panic(const prog_char_t *errormsg)
{
  hal.console->println_P(errormsg);
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

  // TODO: if it's been too long since the last time, call the
  // failsafe callback and return.

  // Run timer processes if not suspended.
  if (!m_suspended) {
    portENTER_CRITICAL();
    uint8_t num_procs = m_num_procs;
    portEXIT_CRITICAL();

    for (int i = 0; i < num_procs; ++i) {
      if (m_procs[i] != NULL)
        m_procs[i](now);
    }
  }

  // Run the deferred procedure, if it exists.
  portENTER_CRITICAL();
  AP_HAL::TimedProc deferred = m_deferred_proc;
  m_deferred_proc = NULL;
  portEXIT_CRITICAL();

  if (deferred != NULL) {
    deferred(now);
  }
}

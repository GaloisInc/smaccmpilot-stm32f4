/*
Unpublished copyright (c) 2013 National ICT Australia (NICTA),
ABN 62 102 206 173.  All rights reserved.

The contents of this document are proprietary to NICTA and you may not
use, copy, modify, sublicense or distribute the contents in any form
except as permitted under the terms of a separately executed licence
agreement with NICTA.

COMMERCIAL LICENSE RIGHTS
Agreement No.: FA8750-12-9-0179
Contractor's Name; Rockwell Collins, Inc.
Contractor's Address: 400 Collins Road N.E., Cedar Rapids, IA 52498

By accepting delivery of the RTOS Code and Documentation, the Licensee
agrees that the software is "commercial" computer software within the
meaning of the applicable acquisition regulations (e.g., FAR 2.101 or
DFARS 227.7202-3).  The terms and conditions of this License shall pertain
to the Licensee's use and disclosure of the software, and shall supersede
any conflicting contractual terms or conditions.

*/
#ifndef RTOS_KOCHAB_H
#define RTOS_KOCHAB_H
#include <stdint.h>



#include <stdbool.h>
#include <stdint.h>

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef uint8_t TaskId;

typedef uint8_t SignalId;
typedef uint8_t SignalSet;
typedef SignalId SignalIdOption;

typedef uint8_t IrqEventId;
typedef uint8_t MutexId;
typedef uint8_t SemId;

extern int _muxid_count;
extern int _semid_count;
extern int _taskid_count;


#define TASK_ID_C(x) ((TaskId) UINT8_C(x))
#define TASK_ID_ZERO TASK_ID_C(0)
#define TASK_ID_INVALID ((TaskId) TASK_ID_C(UINT8_MAX))
#define TASK_ID_MAX_NUM TASK_ID_C(_taskid_count)
#define TASK_ID_MAX TASK_ID_C(TASK_ID_MAX_NUM - 1)

#define SIGNAL_ID_C(x) ((SignalId) UINT8_C(x))
#define SIGNAL_ID_NONE ((SignalIdOption) SIGNAL_ID_C(0xff))
#define SIGNAL_SET_C(x) ((SignalSet) UINT8_C(x))
#define SIGNAL_SET_NONE SIGNAL_SET_C(0)
#define SIGNAL_SET_ALL SIGNAL_SET_C(UINT8_MAX)
#define SIGNAL_ID_TO_SET(signal) ((SignalSet) (1u << signal))

#define IRQ_EVENT_ID_C(x) ((IrqEventId) UINT8_C(x))

#define MUTEX_ID_C(x) ((MutexId) UINT8_C(x))
#define MUTEX_ID_ZERO MUTEX_ID_C(0)
#define MUTEX_MAX_NUM  MUTEX_ID_C(_muxid_count)
#define MUTEX_ID_MAX MUTEX_ID_C(MUTEX_MAX_NUM - 1)

#define SEM_ID_C(x) ((SemId) UINT8_C(x))
#define SEM_ID_ZERO SEM_ID_C(0)
#define SEM_MAX_NUM SEM_ID_C(_semid_count)
#define SEM_ID_MAX SEM_ID_C(SEM_MAX_NUM - 1)

#define rtos_signal_send(task_id, signal_id) \
    rtos_signal_send_set(task_id, SIGNAL_ID_TO_SET(signal_id))

#define rtos_signal_peek(signal_id) \
    rtos_signal_peek_set(SIGNAL_ID_TO_SET(signal_id))

#define rtos_signal_wait(signal_id) \
    (void) rtos_signal_wait_set(SIGNAL_ID_TO_SET(signal_id))

#define rtos_signal_poll(signal_id) \
    (rtos_signal_poll_set(SIGNAL_ID_TO_SET(signal_id)) == signal_id)


#ifdef __cplusplus
extern "C" {
#endif

void rtos_start(void);
void rtos_yield(void);



SignalId rtos_signal_wait_set(SignalSet signal_set);
void rtos_signal_send_set(TaskId task_id, SignalSet signal_set);
SignalIdOption rtos_signal_poll_set(SignalSet signal_set);
bool rtos_signal_peek_set(SignalSet signal_set);
void rtos_irq_event_raise(IrqEventId);

bool rtos_mutex_lock_delay(MutexId, unsigned long);
void rtos_mutex_lock(MutexId);
bool rtos_mutex_try_lock(MutexId);
void rtos_mutex_unlock(MutexId);
void rtos_sem_post(SemId);
bool rtos_sem_try_wait(SemId);
void rtos_sem_wait(SemId);
bool rtos_sem_wait_delay(SemId,unsigned long);
uint8_t rtos_get_sem_value(const SemId s);

/*  APIs added for SMACCMpilot */
TaskId rtos_get_mutex_holder(const MutexId s);
TaskId rtos_get_current_task(void);
void rtos_enable_interrupt(void);
void rtos_disable_interrupt(void);
bool rtos_tick_irq(void);
unsigned long rtos_get_sys_tick(void);
void rtos_enable_preempt(void);
void rtos_disable_preempt(void);

#ifdef __cplusplus
}
#endif


#endif /* RTOS_KOCHAB_H */

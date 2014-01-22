/*
 * task.c
 *
 *  Created on: Nov 18, 2013
 *      Author: jxie @ NICTA
 */

#include <stdlib.h>
#include "rtos-kochab.h"
#include "stm32f4xx.h"
#include "debug.h"
#include "mutex.h"
#include "task.h"
#include "port.h"
#include "config.h"


extern void *entry_fn[];

extern void UNIMPLEMENTED(void);
#define vNOP() UNIMPLEMENTED()
#define NOP(type) do { UNIMPLEMENTED(); return (type)0; } while(0)


#define	TASK_NUM_MAX	(TASK_ID_MAX + 1)

static int assignTskId = 0;

/* Now it is a dummy handler, used to identify tasks */
struct tsk_t{
	uint8_t taskid;
};

struct tsk_t * xTaskList;

void * eChronosCreateTask(void * pxTaskCode)
{
	uint8_t selectid = assignTskId;
	//assignTskId ++;
	bool entryfn = false;
	int i = 0;

    /* Initialise internal book keeping if required */
    if(xTaskList == NULL){
        xTaskList = malloc(sizeof(*xTaskList) * TASK_NUM_MAX);
    }

	for(i=0;i<TASK_NUM_MAX;i++){
		//debug_println("Checking fn @ ");
		//debug_printhex32(entry_fn[i]);
		//debug_println("\n");

		entryfn = ((void *)entry_fn[i] == pxTaskCode);
		if(entryfn){
			//debug_println("Found PxTaskcode!\n ");
			break;
		}
	}
	if(i < TASK_NUM_MAX){
		selectid = i;
	}else{
		//debug_println("PxTaskcode not found!\n ");
		//debug_printhex32(pxTaskCode);
		//debug_println("\n");
		return NULL;
	}
#ifdef ECHRONOS_DEBUG_ENABLE
	debug_println("eChronosCreateTaskId ");
	debug_printhex32(selectid);
	debug_println(" @ ");
	debug_printhex32((unsigned int)&xTaskList[selectid]);
	debug_println("\n");
#endif
	return (void*)&xTaskList[selectid];
}



void eChronosStartRTOS(void)
{
	rtos_start();
}

void * eChronosGetCurrentTaskHandler( void )
{
	uint8_t tskId = rtos_get_current_task();
	return (void*)&xTaskList[tskId];
}

void * eChronosGetTaskHandler(uint8_t tskId)
{
	return (void*)&xTaskList[tskId];
}

unsigned long eChronosGetSysTick(void)
{
	return rtos_get_sys_tick();
}



static void * BlockOnDelay_sem;
static int first_time_call_on_delay = 1;

void eChronosTaskDelayUntil(unsigned long * const pxPreviousWakeTime, unsigned long xTimeIncrement)
{
	unsigned long cur_time = rtos_get_sys_tick();
	unsigned long wake_time = *pxPreviousWakeTime + xTimeIncrement;

	if(first_time_call_on_delay || BlockOnDelay_sem == NULL){
		BlockOnDelay_sem = eChronosCreateSemaphoreCounting(16, 0);
		first_time_call_on_delay = 0;
	}

	//should be block from here
	while(wake_time > cur_time){
		//error, wait again
		eChronosMutexTake(BlockOnDelay_sem, wake_time - cur_time);
		cur_time = rtos_get_sys_tick();
	}
}

void eChronos_yield(void)
{
	rtos_yield();
}

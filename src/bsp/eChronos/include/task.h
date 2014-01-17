/*
 * task.h
 *
 *  Created on: Nov 18, 2013
 *      Author: jxie @ NICTA
 */

#ifndef TASK_H_
#define TASK_H_


void * eChronosCreateTask(void * pxTaskCode);

void * eChronosGetCurrentTaskHandler( void );

void * eChronosGetTaskHandler(uint8_t tskId);

unsigned long eChronosGetSysTick(void);

void eChronosStartRTOS(void);

void eChronos_yield(void);

void eChronosTaskDelayUntil(unsigned long * const pxPreviousWakeTime, unsigned long xTimeIncrement);



#endif /* TASK_H_ */

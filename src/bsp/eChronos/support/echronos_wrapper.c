/*
 * echronos_wrapper.c
 *
 *  Created on: Nov 18, 2013
 *      Author: jxie @ NICTA
 */

#include "eChronos.h"
#include "rtos-kochab.h"
#include "task.h"
#include "mutex.h"
#include "queue.h"
#include "port.h"
#include <assert.h>

#define UNIMPLEMENTED() assert(!"UNIMPLEMENETED");
#define vNOP() UNIMPLEMENTED()
#define NOP(type) do { UNIMPLEMENTED(); return (type)0; } while(0)

extern unsigned long rtos_get_sys_tick(void);


void portENTER_CRITICAL(void)
{
	vPortEnterCritical();
}
void portEXIT_CRITICAL(void)
{
	vPortExitCritical();
}

/* SEM/MUTEX APIs */
xSemaphoreHandle xSemaphoreCreateCounting( unsigned portBASE_TYPE uxCountValue, unsigned portBASE_TYPE uxInitialCount)
{
	xSemaphoreHandle x =  eChronosCreateSemaphoreCounting(uxCountValue, uxInitialCount);
	return x;
}

xSemaphoreHandle xSemaphoreCreateMutex(void)
{
	 return  eChronosCreateMutex();
}

xSemaphoreHandle xSemaphoreCreateRecursiveMutex(void)
{
	 return eChronosCreateRecursiveMutex();
}

signed portBASE_TYPE  xSemaphoreTake(xSemaphoreHandle xMuxId, portTickType xBlockTime )
{
	return eChronosMutexTake(xMuxId, xBlockTime);
}

signed portBASE_TYPE xSemaphoreGive(xSemaphoreHandle xMuxId)
{
	return eChronosMutexGive(xMuxId);
}

portBASE_TYPE xSemaphoreTakeRecursive(xSemaphoreHandle xMuxId,portTickType xBlockTime )
{
	return eChronosMutexTakeRecursive(xMuxId, xBlockTime);
}

portBASE_TYPE xSemaphoreGiveRecursive(xSemaphoreHandle xMuxId )
{
	return eChronosMutexGiveRecursive(xMuxId);
}


signed portBASE_TYPE  xSemaphoreGiveFromISR(xSemaphoreHandle xMuxId,signed portBASE_TYPE * pxHigherPriorityTaskWoken ){

	*pxHigherPriorityTaskWoken = 0;

	return xSemaphoreGive(xMuxId);;
}

void* xSemaphoreGetMutexHolder( xSemaphoreHandle xMuxId)
{
	return eChronosGetMutexHolder(xMuxId);
}

void vSemaphoreDelete( xSemaphoreHandle xSemaphore)
{
	//FIXME: need implementation
}



signed portBASE_TYPE xTaskGenericCreate( pdTASK_CODE pxTaskCode, const signed char * const pcName,
											unsigned short usStackDepth, void *pvParameters,
											unsigned portBASE_TYPE uxPriority,
											xTaskHandle *pxCreatedTask, portSTACK_TYPE *puxStackBuffer,
											const xMemoryRegion * const xRegions )
{
	if(pxCreatedTask != NULL){
		*pxCreatedTask = eChronosCreateTask(pxTaskCode);
		if(*pxCreatedTask){
			 return pdPASS;
		}else{
			return pdFALSE;
		}
	}
    return pdPASS;
}

signed portBASE_TYPE xTaskCreate( pdTASK_CODE pxTaskCode, const signed char * pcName,
									unsigned short usStackDepth, void *pvParameters,
									unsigned portBASE_TYPE uxPriority, xTaskHandle *pxCreatedTask){
	if(pxCreatedTask != NULL){
		*pxCreatedTask = eChronosCreateTask(pxTaskCode);
		if(*pxCreatedTask){
			 return pdPASS;
		}else{
			return pdFALSE;
		}
	}
    return pdPASS;
}

xTaskHandle xTaskGetCurrentTaskHandle( void )
{
	return eChronosGetCurrentTaskHandler();
}

void vTaskSetApplicationTaskTag( xTaskHandle xTask, pdTASK_HOOK_CODE pxHookFunction )
{
    //vNOP();
}


void vTaskStartScheduler(void)
{
	xPortStart();
    eChronosStartRTOS();
}

portTickType xTaskGetTickCount(void)
{
	portTickType xTicks;

	xTicks = eChronosGetSysTick();

	return xTicks;
}

void vTaskDelay( portTickType xMsToDelay )
{
	unsigned long curTime = rtos_get_sys_tick();
	eChronosTaskDelayUntil(&curTime, xMsToDelay);

}

void vTaskDelayUntil( portTickType * const pxPreviousWakeTime, portTickType xTimeIncrement )
{
	eChronosTaskDelayUntil(pxPreviousWakeTime, xTimeIncrement);
}

void taskYIELD(void)
{
	eChronos_yield();
}


/*
 * QUEUE.c
 */

xQueueHandle xQueueCreate( unsigned portBASE_TYPE uxQueueLength, unsigned portBASE_TYPE uxItemSize, unsigned char ucQueueType)
{
	uint32_t rval = eChronosQueueGenericCreate( uxQueueLength, uxItemSize, ucQueueType);
	return (xQueueHandle)rval;
}

signed portBASE_TYPE xQueueSend( xQueueHandle pxQueue, const void * const pvItemToQueue, portTickType xTicksToWait, portBASE_TYPE xCopyPosition)
{
	uint8_t q = (uint32_t)pxQueue;
	return eChronosQueueGenericSend((uint8_t)q, pvItemToQueue, xTicksToWait, xCopyPosition);
}


signed portBASE_TYPE xQueueReceive( xQueueHandle pxQueue, void * const pvBuffer, portTickType xTicksToWait, portBASE_TYPE xJustPeeking )
{
	uint8_t q = (uint32_t)pxQueue;
	return eChrononsQueueGenericReceive( (uint8_t)q, pvBuffer, xTicksToWait, xJustPeeking);
}
signed portBASE_TYPE xQueueSendFromISR( xQueueHandle pxQueue, const void * const pvItemToQueue, signed portBASE_TYPE *pxHigherPriorityTaskWoken, portBASE_TYPE xCopyPosition )
{
	uint8_t q = (uint32_t)pxQueue;
	return eChronosQueueGenericSendFromISR( (uint8_t)q, pvItemToQueue, pxHigherPriorityTaskWoken, xCopyPosition);
}
signed portBASE_TYPE xQueueReceiveFromISR( xQueueHandle pxQueue, void * const pvBuffer, signed portBASE_TYPE *pxHigherPriorityTaskWoken)
{
	uint8_t q = (uint32_t)pxQueue;
	return eChronosQueueReceiveFromISR( (uint8_t)q, pvBuffer, pxHigherPriorityTaskWoken);
}

unsigned portBASE_TYPE uxQueueMessagesWaiting( const xQueueHandle pxQueue )
{
	uint8_t q = (uint32_t)pxQueue;
	return eChronosQueueMessagesWaiting( (uint8_t)q);
}




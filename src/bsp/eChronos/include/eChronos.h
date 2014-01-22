/*
 * eChronos.h
 *
 *  Created on: Nov 12, 2013
 *      Author: jxie @ NICTA
 */
/* FREERTOS EMULATION LAYER */

#ifndef ECHRONOS_H_
#define ECHRONOS_H_

#include <stddef.h>
#include <stdint.h>
#include "stm32f4xx.h"
#include "core_cm4.h"
#include "port.h"
#include "config.h"


extern void vPortEnterCritical(void);
extern void vPortExitCritical(void);


#ifdef __cplusplus
extern "C" {
#endif



/* FREERTOS Type definitions. */
#define portCHAR		char
#define portFLOAT		float
#define portDOUBLE		double
#define portLONG		long
#define portSHORT		short
#define portSTACK_TYPE	unsigned long
#define portBASE_TYPE	long

#define pdTRUE			( 1 )
#define pdFALSE			( 0 )
#define pdPASS			( 1 )
#define pdFAIL			( 0 )


#if( configUSE_16_BIT_TICKS == 1 )
	typedef unsigned portSHORT portTickType;
	#define portMAX_DELAY ( portTickType ) 0xffff
#else
	typedef unsigned portLONG portTickType;
	#define portMAX_DELAY ( portTickType ) 0xffffffff
#endif

/* Defines the prototype to which the application task hook function must
conform. */
typedef portBASE_TYPE (*pdTASK_HOOK_CODE)	( void * );

/* FreeRTOS realted configuration */
#define configMAX_PRIORITIES		( ( unsigned portBASE_TYPE ) 5 )

/* Architecture specifics. */
#define portSTACK_GROWTH			( -1 )
#define portTICK_RATE_MS			1//( ( portTickType ) 1000 / configTICK_RATE_HZ )
#define portBYTE_ALIGNMENT			8
/*-----------------------------------------------------------*/


/* Defines the prototype to which task functions must conform. */
typedef void (*pdTASK_CODE)( void* );

//typedef uint8_t 			xTaskHandle;
typedef void * 				xTaskHandle;
typedef void *				xSemaphoreHandle;		/* echronos Semaphore Type */
typedef void * 				xQueueHandle;
typedef void 				xMemoryRegion;


void portEnable_IRQs(void);
void portDisable_IRQs(void);

void portENTER_CRITICAL(void);
void portEXIT_CRITICAL(void);

#define	portDISABLE_INTERRUPTS()	vPortEnterCritical()
#define	portENABLE_INTERRUPTS()		vPortExitCritical()

/* MUTEX */
xSemaphoreHandle xSemaphoreCreateMutex(void);
xSemaphoreHandle xSemaphoreCreateCounting( unsigned portBASE_TYPE uxCountValue, unsigned portBASE_TYPE uxInitialCount);
xSemaphoreHandle xSemaphoreCreateRecursiveMutex(void);
signed portBASE_TYPE  xSemaphoreTake(xSemaphoreHandle xMuxId, portTickType xBlockTime );

signed portBASE_TYPE xSemaphoreGive(xSemaphoreHandle xMuxId);
portBASE_TYPE xSemaphoreTakeRecursive(xSemaphoreHandle xMutex,portTickType xBlockTime );
portBASE_TYPE xSemaphoreGiveRecursive(xSemaphoreHandle xMutex );
signed portBASE_TYPE  xSemaphoreGiveFromISR(xSemaphoreHandle xMuxId,signed portBASE_TYPE * pxHigherPriorityTaskWoken );
void* xSemaphoreGetMutexHolder( xSemaphoreHandle xMuxId);
void vSemaphoreDelete( xSemaphoreHandle xSemaphore);
void _exit (int __status);

/* TASK */
signed portBASE_TYPE xTaskGenericCreate( pdTASK_CODE pxTaskCode, const signed char * const pcName,
											unsigned short usStackDepth, void *pvParameters,
											unsigned portBASE_TYPE uxPriority,
											xTaskHandle *pxCreatedTask, portSTACK_TYPE *puxStackBuffer,
											const xMemoryRegion * const xRegions );
signed portBASE_TYPE xTaskCreate( pdTASK_CODE pxTaskCode, const signed char * pcName,
									unsigned short usStackDepth, void *pvParameters,
									unsigned portBASE_TYPE uxPriority, xTaskHandle *pxCreatedTask);
xTaskHandle xTaskGetCurrentTaskHandle( void );
void vTaskSetApplicationTaskTag( xTaskHandle xTask, pdTASK_HOOK_CODE pxHookFunction );
void vTaskStartScheduler(void);
portTickType xTaskGetTickCount(void);
void vTaskDelay( portTickType xTicksToDelay );
void vTaskDelayUntil( portTickType * const pxPreviousWakeTime, portTickType xTimeIncrement);
void taskYIELD(void);

/* QUEUE */
xQueueHandle xQueueCreate( unsigned portBASE_TYPE uxQueueLength, unsigned portBASE_TYPE uxItemSize, unsigned char ucQueueType);
signed portBASE_TYPE xQueueSend( xQueueHandle pxQueue, const void * const pvItemToQueue, portTickType xTicksToWait, portBASE_TYPE xCopyPosition);
signed portBASE_TYPE xQueueReceive( xQueueHandle pxQueue, void * const pvBuffer, portTickType xTicksToWait, portBASE_TYPE xJustPeeking);
signed portBASE_TYPE xQueueSendFromISR( xQueueHandle pxQueue, const void * const pvItemToQueue, signed portBASE_TYPE *pxHigherPriorityTaskWoken, portBASE_TYPE xCopyPosition);
signed portBASE_TYPE xQueueReceiveFromISR( xQueueHandle pxQueue, void * const pvBuffer, signed portBASE_TYPE *pxHigherPriorityTaskWoken);
unsigned portBASE_TYPE uxQueueMessagesWaiting( const xQueueHandle pxQueue);


#define vSemaphoreCreateBinary( xSemaphore )																									\
	{																																			\
		( xSemaphore ) = xSemaphoreCreateCounting(1,0);																								\
		if( ( xSemaphore ) != NULL )																											\
		{																																		\
			xSemaphoreGive( ( xSemaphore ) );																									\
		}																																		\
	}


extern void debug_printhex32(const uint32_t val);
extern void debug_println(const char *msg);



#ifdef __cplusplus
}
#endif

#endif /* ECHRONOS_H_ */

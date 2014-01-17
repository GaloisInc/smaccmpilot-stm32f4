
#include "rtos-kochab.h"
#include "queue.h"

#include <eChronos.h>

//#define xQueueCreate( uxQueueLength, uxItemSize ) xQueueGenericCreate( uxQueueLength, uxItemSize, queueQUEUE_TYPE_BASE )
uint8_t eChronosQueueGenericCreate( unsigned long uxQueueLength, unsigned long uxItemSize, unsigned char ucQueueType)
{
	return 0;
}

//#define xQueueSend( xQueue, pvItemToQueue, xTicksToWait ) xQueueGenericSend( ( xQueue ), ( pvItemToQueue ), ( xTicksToWait ), queueSEND_TO_BACK )
signed long eChronosQueueGenericSend( uint8_t pxQueue, const void * const pvItemToQueue, unsigned long xTicksToWait, long xCopyPosition)
{
	return 0;
}

//#define xQueueReceive( xQueue, pvBuffer, xTicksToWait ) xQueueGenericReceive( ( xQueue ), ( pvBuffer ), ( xTicksToWait ), pdFALSE )
signed long eChrononsQueueGenericReceive( uint8_t pxQueue, void * const pvBuffer, unsigned long xTicksToWait, long xJustPeeking)
{
	return 0;
}

//#define xQueueSendFromISR( pxQueue, pvItemToQueue, pxHigherPriorityTaskWoken ) xQueueGenericSendFromISR( ( pxQueue ), ( pvItemToQueue ), ( pxHigherPriorityTaskWoken ), queueSEND_TO_BACK )
signed long eChronosQueueGenericSendFromISR( uint8_t pxQueue, const void * const pvItemToQueue, signed long *pxHigherPriorityTaskWoken, long xCopyPosition )
{
	return 0;
}

signed long eChronosQueueReceiveFromISR( uint8_t pxQueue, void * const pvBuffer, signed long *pxHigherPriorityTaskWoken)
{
	return 0;
}

unsigned long eChronosQueueMessagesWaiting( const uint8_t pxQueue)
{
	return 0;
}

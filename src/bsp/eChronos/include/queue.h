/*
 * queue.h
 *
 *  Created on: Nov 20, 2013
 *      Author: jxie @ NICTA
 */

#ifndef QUEUE_H_
#define QUEUE_H_

uint8_t eChronosQueueGenericCreate( unsigned long uxQueueLength, unsigned long uxItemSize, unsigned char ucQueueType);
signed long eChronosQueueGenericSend( uint8_t pxQueue, const void * const pvItemToQueue, unsigned long xTicksToWait, long xCopyPosition);
signed long eChrononsQueueGenericReceive( uint8_t pxQueue, void * const pvBuffer, unsigned long xTicksToWait, long xJustPeeking);
signed long eChronosQueueGenericSendFromISR( uint8_t pxQueue, const void * const pvItemToQueue, signed long *pxHigherPriorityTaskWoken, long xCopyPosition);
signed long eChronosQueueReceiveFromISR( uint8_t pxQueue, void * const pvBuffer, signed long *pxHigherPriorityTaskWoken);
unsigned long eChronosQueueMessagesWaiting( const uint8_t pxQueue);


#endif /* QUEUE_H_ */

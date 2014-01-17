/*
 * mutex.h
 *
 *  Created on: Nov 18, 2013
 *      Author: jxie @ NICTA
 */

#ifndef MUTEX_H_
#define MUTEX_H_

void* eChronosCreateSemaphoreCounting( unsigned long uxCountValue, unsigned long uxInitialCount);
void * eChronosCreateSemaphore( unsigned long uxCountValue, unsigned long uxInitialCount);
void * eChronosCreateMutex(void);
void * eChronosCreateRecursiveMutex(void);
signed long eChronosMutexTake(void * xMuxId, unsigned long xBlockTime);
signed long eChronosMutexGive(void * xMuxId);
long eChronosMutexTakeRecursive(void * xMutex, unsigned long xBlockTime);
long eChronosMutexGiveRecursive(void * xMutex);
void * eChronosGetMutexHolder(void * xMuxId);


#endif /* MUTEX_H_ */

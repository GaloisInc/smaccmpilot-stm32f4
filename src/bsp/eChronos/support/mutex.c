/*
 * mutex.c
 *
 *  Created on: Nov 14, 2013
 *      Author: jxie @ NICTA
 */
#include <stdio.h>
#include "rtos-kochab.h"
#include "mutex.h"
#include "task.h"
#include "port.h"
#include <stdlib.h>
#define pdTRUE		1
#define pdFALSE		0

extern bool rtos_sem_wait_delay(SemId,unsigned long);

//extern void debug_println(const char *msg);

//extern void debug_printhex32(uint32_t val);

enum{
	BINARY_MUTEX = 1,
	RECURSIVE_MUTEX = 2,
	SEMAPHORE = 3,
	COUNTING_SEMAPHORE = 4
};

struct MUX_t{
	int 		mux_type;								//type of semaphore / mutex
	uint8_t 	muxid;
	int 		TskHoldCnt;
};

struct SEM_t{
	int 		sem_type;
	uint8_t 	semid;
	int 		max_count;
	int 		initial_count;
};

struct xSemMux_t{
	int 	created;
	int 	type;
	void * 	priv;
};

#ifndef MUTEX_ID_m00
#define MUTEX_ID_m00 0
#endif

#ifndef SEM_ID_sem00
#define SEM_ID_sem00 0
#endif

#define xMUTEX_START			MUTEX_ID_m00
#define xMUTEX_END			MUTEX_ID_MAX
#define xMUTEX_NUM			(xMUTEX_END + 1 - xMUTEX_START)
#define xSEM_START			SEM_ID_sem00
#define xSEM_END			SEM_ID_MAX 
#define xSEM_NUM		 	(xSEM_END + 1 - xSEM_START)	

#define xMUTEX_NUM_MAX			xMUTEX_NUM
#define xSEM_NUM_MAX			xSEM_NUM
#define xSEM_MUX_NUM_MAX		(xMUTEX_NUM_MAX + xSEM_NUM_MAX)

static struct MUX_t* 			xMuxList;
static struct SEM_t* 			xSemList;
static struct xSemMux_t*		xSemMuxList;

static uint8_t xMuxAssignID = 0;	/* 	xMuxAssignID is used to keep track of assigned mutex ids */
static uint8_t xSemAssignID = 0;	/* 	xSemAssignID is used to keep track of assigned sem ids */

static int firstcall_init_need = 1;

static void xMux_init(void){
	int i, j = 0;

	for(i = 0; i < xMUTEX_NUM_MAX; i++){
		xMuxList[i].TskHoldCnt		= 0;
		xMuxList[i].mux_type		= BINARY_MUTEX;		//default
		xMuxList[i].muxid			= -1;
	}
	xMuxAssignID = xMUTEX_START; 
}


static void xSem_init(void){
	int i, j = 0;

	for(i = 0; i < xSEM_NUM_MAX; i++){
		xSemList[i].sem_type 		= SEMAPHORE;
		xSemList[i].max_count		= 0;
		xSemList[i].initial_count 	= 0;
		xSemList[i].semid			= -1;		//default
	}
	xSemAssignID = xSEM_START;
}


static void xSemMux_init(void)
{
	int i = 0;
    xMuxList = malloc(sizeof(*xMuxList) * _muxid_count);
    xSemList = malloc(sizeof(*xSemList) * _semid_count);
    xSemMuxList = malloc(sizeof(*xSemMuxList) * (_muxid_count + _semid_count));
	xMux_init();
	xSem_init();

	for(i = 0; i < xSEM_MUX_NUM_MAX; i++){
		xSemMuxList[i].created = 0;
		xSemMuxList[i].priv = NULL;
		xSemMuxList[i].type = BINARY_MUTEX;
	}
	return;
}

static void * alloc_xSemMux(uint32_t type, int arg1, int arg2){

	void * priv = NULL;
	int i = 0;

	switch(type){

	case BINARY_MUTEX:
	case RECURSIVE_MUTEX:

		if(xMuxAssignID < xMUTEX_NUM_MAX){
			xMuxList[xMuxAssignID].TskHoldCnt	= 0;
			xMuxList[xMuxAssignID].mux_type		= type;
			xMuxList[xMuxAssignID].muxid		= xMuxAssignID;
			priv = (void *) & xMuxList[xMuxAssignID];

			xMuxAssignID ++ ; //update xMuxAssignID
		}else{
#ifdef ECHRONOS_DEBUG_ENABLE
			debug_println("error: trying to allocate too many mutexes\n");
#endif
		}
		break;

	case SEMAPHORE:
	case COUNTING_SEMAPHORE:

		if(xSemAssignID < xSEM_NUM_MAX){
			xSemList[xSemAssignID].sem_type 		= type;
			xSemList[xSemAssignID].max_count		= arg1;
			xSemList[xSemAssignID].initial_count 	= arg2;
			xSemList[xSemAssignID].semid			= xSemAssignID;

			if(arg2 > 0){
				rtos_disable_preempt();
				while(rtos_get_sem_value(xSemAssignID) < arg2){
					rtos_sem_post(xSemAssignID);
				}
				rtos_enable_preempt();
			}

			priv = (void *) &xSemList[xSemAssignID];

			xSemAssignID ++; //update xSemAssignID
		}else{
#ifdef ECHRONOS_DEBUG_ENABLE
			debug_println("error: trying to allocate too many mutexes\n");
#endif
		}

		break;
	default:
		break;
	}

	if(priv == NULL){
#ifdef ECHRONOS_DEBUG_ENABLE
		debug_println("error in alloc_mux ! \n");
#endif
	}

	return priv;
}


void* eChronosCreateSemaphoreCounting( unsigned long uxCountValue, unsigned long uxInitialCount)
{
	uint8_t selectid = 0;
	int i = 0;

	if(firstcall_init_need){
		xSemMux_init();
		firstcall_init_need = 0;
	}

	void * priv = alloc_xSemMux(COUNTING_SEMAPHORE, uxCountValue, uxInitialCount);

	if(priv == NULL){
#ifdef ECHRONOS_DEBUG_ENABLE
		debug_println("error 1\n");
#endif
		return NULL;
	}

	for(i = 0; i < xSEM_MUX_NUM_MAX; i++){
		if(xSemMuxList[i].created == 0){
			xSemMuxList[i].priv = priv;
			xSemMuxList[i].type = COUNTING_SEMAPHORE;
			xSemMuxList[i].created = 1;
			break;
		}
	}

	if(i >= xSEM_MUX_NUM_MAX){
#ifdef ECHRONOS_DEBUG_ENABLE
		debug_println("error 2\n");
#endif
		return NULL;
	}

	return (void *)&xSemMuxList[i];
}


void* eChronosCreateMutex(void)
{
	if(firstcall_init_need){
		xSemMux_init();
		firstcall_init_need = 0;
	}

	void * priv = alloc_xSemMux(BINARY_MUTEX, 0, 0);

	if(priv == NULL){
#ifdef ECHRONOS_DEBUG_ENABLE
		debug_println("error 3\n");
#endif
		return NULL;
	}


	int i = 0;


	for(i = 0; i < xSEM_MUX_NUM_MAX; i++){
		if(xSemMuxList[i].created == 0){
			xSemMuxList[i].priv = priv;
			xSemMuxList[i].type = BINARY_MUTEX;
			xSemMuxList[i].created = 1;
			break;
		}
	}

	if(i >= xSEM_MUX_NUM_MAX){
#ifdef ECHRONOS_DEBUG_ENABLE
		debug_println("error 4\n");
#endif
		return NULL;
	}


	return (void *)&xSemMuxList[i];
}

void* eChronosCreateRecursiveMutex(void)
{
	if(firstcall_init_need){
		xSemMux_init();
		firstcall_init_need = 0;
	}

	void * priv = alloc_xSemMux(RECURSIVE_MUTEX, 0, 0);

	if(priv == NULL){
#ifdef ECHRONOS_DEBUG_ENABLE
		debug_println("error 5\n");
#endif
		return NULL;
	}


	int i = 0;

	for(i = 0; i < xSEM_MUX_NUM_MAX; i++){
		if(xSemMuxList[i].created == 0){
			xSemMuxList[i].priv = priv;
			xSemMuxList[i].type = RECURSIVE_MUTEX;
			xSemMuxList[i].created = 1;
			break;
		}
	}

	if(i >= xSEM_MUX_NUM_MAX){
#ifdef ECHRONOS_DEBUG_ENABLE
		debug_println("error 6\n");
#endif
		return NULL;
	}


	return (void *)&xSemMuxList[i];
}

static int _SemMux_Take(int type, void * priv, unsigned long max_delay_ms){

	struct MUX_t * mux;
	struct SEM_t * sem;

	bool	r = pdTRUE;

	uint8_t TaskHolderId = rtos_get_current_task();

	switch(type){

	case BINARY_MUTEX:
		mux = (struct MUX_t *) priv;

		rtos_disable_preempt();

		if(max_delay_ms){
			r = rtos_mutex_lock_delay(mux->muxid, _ms_to_ticks(max_delay_ms));
		}else{
			r = rtos_mutex_try_lock(mux->muxid);
		}

		rtos_enable_preempt();

		break;

	case RECURSIVE_MUTEX:
		mux = (struct MUX_t *) priv;

		rtos_disable_preempt();

		if( (mux->TskHoldCnt == 0 && rtos_get_mutex_holder(mux->muxid) == TASK_ID_INVALID) || (rtos_get_mutex_holder(mux->muxid) != TaskHolderId) ){
			//take the mutex and update holder Id

			if(max_delay_ms){
				r = rtos_mutex_lock_delay(mux->muxid, _ms_to_ticks(max_delay_ms));
			}else{
				r = rtos_mutex_try_lock(mux->muxid);
			}
			if(r == pdTRUE){
				//mutex already been taken
				//increment the counter
				mux->TskHoldCnt ++;
			}

		}else if(rtos_get_mutex_holder(mux->muxid) == TaskHolderId){
			//increment the counter
			mux->TskHoldCnt ++;
		}else{
#ifdef ECHRONOS_DEBUG_ENABLE
			debug_println(": RECURSIVE_MUTEX Take FAULT!\n");
#endif
		}

		rtos_enable_preempt();

		break;

	case SEMAPHORE:
	case COUNTING_SEMAPHORE:
		sem = (struct SEM_t *)priv;
		if(max_delay_ms){
			r = rtos_sem_wait_delay(sem->semid, _ms_to_ticks(max_delay_ms));
		}else{
			r = rtos_sem_try_wait(sem->semid);
		}
		break;
	default:
		break;
	}

	return r;
}


static void _SemMux_Give(int type, void * priv)
{
	struct MUX_t * mux;
	struct SEM_t * sem;

	uint8_t TaskHolderId = rtos_get_current_task();

	switch(type){

	case BINARY_MUTEX:

		mux = (struct MUX_t *) priv;

		rtos_disable_preempt();
		rtos_mutex_unlock(mux->muxid);
		rtos_enable_preempt();

		break;

	case RECURSIVE_MUTEX:
		mux = (struct MUX_t *) priv;

		rtos_disable_preempt();

		if(mux->TskHoldCnt != 0 && rtos_get_mutex_holder(mux->muxid) == TaskHolderId){
			//mutex has been taken more than one time
			mux->TskHoldCnt --;
			//see if we actually need to release the mutex
			if(mux->TskHoldCnt == 0){
				rtos_mutex_unlock(mux->muxid);
			}
		}else{
			//error!
#ifdef ECHRONOS_DEBUG_ENABLE
			debug_println(": RECURSIVE_MUTEX Give FAULT!\n");
#endif
		}

		rtos_enable_preempt();

		break;

	case SEMAPHORE:
	case COUNTING_SEMAPHORE:

		sem = (struct SEM_t *)priv;

		rtos_disable_preempt();

		if(rtos_get_sem_value(sem->semid) < sem->max_count){
			rtos_sem_post(sem->semid);
		}

		rtos_enable_preempt();

		break;

	default:
		break;
	}

	return;

}

signed long eChronosMutexTake(void * handler, unsigned long xBlockTime)
{
	struct xSemMux_t * x = (struct xSemMux_t *) handler;
	if(x->created){
		return _SemMux_Take(x->type, x->priv, xBlockTime);
	}
	return pdFALSE;
}

signed long eChronosMutexGive(void * handler){

	struct xSemMux_t * x = (struct xSemMux_t *) handler;
	if(x->created){
		_SemMux_Give(x->type, x->priv);
		return pdTRUE;
	}
	return pdFALSE;
}


long eChronosMutexTakeRecursive(void * handler, unsigned long xBlockTime)
{
	struct xSemMux_t * x = (struct xSemMux_t *) handler;
	if(x->created){
		return _SemMux_Take(x->type, x->priv, xBlockTime);
	}
	return pdFALSE;
}

long eChronosMutexGiveRecursive(void * handler)
{
	struct xSemMux_t * x = (struct xSemMux_t *) handler;
	if(x->created){
		_SemMux_Give(x->type, x->priv);
		return pdTRUE;
	}
	return pdFALSE;

}
void * eChronosGetMutexHolder(void * handler)
{
	struct xSemMux_t * x = (struct xSemMux_t *) handler;
	if(x->created == 1){
		struct MUX_t * mux = (struct MUX_t *) x->priv;
		if(rtos_get_mutex_holder(mux->muxid) == TASK_ID_INVALID){
			return NULL;
		}
		return eChronosGetTaskHandler(rtos_get_mutex_holder(mux->muxid));
	}else{
#ifdef ECHRONOS_DEBUG_ENABLE
		debug_println("eChronosGetMutexHolder fault\n");
#endif
		return NULL;
	}
}




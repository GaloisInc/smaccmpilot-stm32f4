/*
 * port.c
 *
 *  Created on: Nov 26, 2013
 *      Author: jxie @ NICTA
 */

#include "stm32f4xx.h"
#include "rtos-kochab.h"
#include "debug.h"
#include "port.h"

void NVIC_interrupt_enable(enum IRQn n)
{
    NVIC_EnableIRQ(n);
}

void NVIC_interrupt_disable(enum IRQn n)
{
    NVIC_DisableIRQ(n);
}



void SetPendSVPending(void)
{
    portNVIC_INT_CTRL_REG = portNVIC_PENDSVSET_BIT;
    asm volatile("isb");
}

void ClearPendSVPending(void)
{
    portNVIC_INT_CTRL_REG = portNVIC_PENDSVCLEAR_BIT;
}


long _ms_to_ticks(long ms){
    return ms * 1;
}


__attribute__(( naked )) unsigned long ulPortSetInterruptMask( void )
{
	__asm volatile														\
	(																	\
		"	mrs r0, basepri											\n" \
		"	mov r1, %0												\n"	\
		"	msr basepri, r1											\n" \
		"	bx lr													\n" \
		:: "i" ( configMAX_SYSCALL_INTERRUPT_PRIORITY ) : "r0", "r1"	\
	);

	/* This return will not be reached but is necessary to prevent compiler
	warnings. */
	return 0;
}
/*-----------------------------------------------------------*/

__attribute__(( naked )) void vPortClearInterruptMask( unsigned long ulNewMaskValue )
{
	__asm volatile													\
	(																\
		"	msr basepri, r0										\n"	\
		"	bx lr												\n" \
		:::"r0"														\
	);
}


static unsigned long uxCriticalNesting = 0;

void vPortEnterCritical( void )
{
    rtos_disable_preempt();
	//ulPortSetInterruptMask();
    uxCriticalNesting++;
}


void vPortExitCritical( void )
{
    --uxCriticalNesting;
    if( uxCriticalNesting == 0 ){
        rtos_enable_preempt();
    	//vPortClearInterruptMask(0);
    }
}


/* This is a naked function. */
static void vPortEnableVFP( void )
{
	__asm volatile
	(
		"	ldr.w r0, =0xE000ED88		\n" /* The FPU enable bits are in the CPACR. */
		"	ldr r1, [r0]				\n"
		"								\n"
		"	orr r1, r1, #( 0xf << 20 )	\n" /* Enable CP10 and CP11 coprocessors, then save back. */
		"	str r1, [r0]				\n"
	);
}


/*
 * Setup the systick timer to generate the tick interrupts at the required
 * frequency.
 */
void vPortSetupTimerInterrupt( void )
{
	/* Calculate the constants required to configure the tick interrupt. */
	/* Configure SysTick to interrupt at the requested rate. */
	portNVIC_SYSTICK_LOAD_REG = ( configSYSTICK_CLOCK_HZ / configTICK_RATE_HZ ) - 1UL;;
	portNVIC_SYSTICK_CTRL_REG = portNVIC_SYSTICK_CLK_BIT | portNVIC_SYSTICK_INT_BIT | portNVIC_SYSTICK_ENABLE_BIT;
}


void xPortStart( void )
{
	/* Make PendSV, CallSV and SysTick the same priroity as the kernel. */
	portNVIC_SYSPRI2_REG |= portNVIC_PENDSV_PRI;
	portNVIC_SYSPRI2_REG |= portNVIC_SYSTICK_PRI;
	/* Start the timer that generates the tick ISR.  Interrupts are disabled
	here already. */
	vPortSetupTimerInterrupt();
	/* Initialise the critical nesting count ready for the first task. */
	uxCriticalNesting = 0;
	/* Ensure the VFP is enabled - it should be anyway. */
	vPortEnableVFP();
	/* Lazy save always. */
	*( portFPCCR ) |= portASPEN_AND_LSPEN_BITS;

}
/*-----------------------------------------------------------*/

void vPortYieldFromISR( void )
{
	/* Set a PendSV to request a context switch. */
	//portNVIC_INT_CTRL_REG = portNVIC_PENDSVSET_BIT;
}



/*
 * port.h
 *
 *  Created on: Nov 26, 2013
 *      Author: jxie @ NICTA
 */

#ifndef PORT_H_
#define PORT_H_

#define configCPU_CLOCK_HZ			( ( unsigned long ) 168000000 )
#define configTICK_RATE_HZ			( ( unsigned long ) 1000 )


/* Constants required to manipulate the core.  Registers first... */
#define portNVIC_SYSTICK_CTRL_REG			( * ( ( volatile unsigned long * ) 0xe000e010 ) )
#define portNVIC_SYSTICK_LOAD_REG			( * ( ( volatile unsigned long * ) 0xe000e014 ) )
#define portNVIC_SYSTICK_CURRENT_VALUE_REG	( * ( ( volatile unsigned long * ) 0xe000e018 ) )
#define portNVIC_INT_CTRL_REG				( * ( ( volatile unsigned long * ) 0xe000ed04 ) )
#define portNVIC_SYSPRI2_REG				( * ( ( volatile unsigned long * ) 0xe000ed20 ) )
/* ...then bits in the registers. */
#define portNVIC_SYSTICK_CLK_BIT			( 1UL << 2UL )
#define portNVIC_SYSTICK_INT_BIT			( 1UL << 1UL )
#define portNVIC_SYSTICK_ENABLE_BIT			( 1UL << 0UL )
#define portNVIC_SYSTICK_COUNT_FLAG_BIT		( 1UL << 16UL )
#define portNVIC_PENDSVSET_BIT				( 1UL << 28UL )
#define portNVIC_PENDSVCLEAR_BIT 			( 1UL << 27UL )
#define portNVIC_PEND_SYSTICK_CLEAR_BIT		( 1UL << 25UL )

#define portNVIC_PENDSV_PRI				( ( ( unsigned long ) 255 ) << 16UL )
#define portNVIC_SYSTICK_PRI			( ( ( unsigned long ) 191 ) << 24UL )

/* Constants required to manipulate the VFP. */
#define portFPCCR					( ( volatile unsigned long * ) 0xe000ef34 ) /* Floating point context control register. */
#define portASPEN_AND_LSPEN_BITS	( 0x3UL << 30UL )

/* Constants required to set up the initial stack. */
#define portINITIAL_XPSR			( 0x01000000 )
#define portINITIAL_EXEC_RETURN		( 0xfffffffd )



#ifndef configSYSTICK_CLOCK_HZ
	#define configSYSTICK_CLOCK_HZ configCPU_CLOCK_HZ
#endif



/* This is the raw value as per the Cortex-M3 NVIC.  Values can be 255
(lowest) to 0 (1?) (highest). */
#define configKERNEL_INTERRUPT_PRIORITY 		255
/* !!!! configMAX_SYSCALL_INTERRUPT_PRIORITY must not be set to zero !!!!
See http://www.FreeRTOS.org/RTOS-Cortex-M3-M4.html. */
#define configMAX_SYSCALL_INTERRUPT_PRIORITY 	191 /* equivalent to 0xb0, or priority 11. */


/* This is the value being used as per the ST library which permits 16
priority values, 0 to 15.  This must correspond to the
configKERNEL_INTERRUPT_PRIORITY setting.  Here 15 corresponds to the lowest
NVIC value of 255. */
#define configLIBRARY_KERNEL_INTERRUPT_PRIORITY	15


//__attribute__(( naked ))void cotexM4_enable_irqs (void);
//__attribute__(( naked ))void cotexM4_disable_irqs (void);

void vPortEnterCritical(void);
void vPortExitCritical(void);

long _ms_to_ticks(long ms);

void xPortStart( void );
void vPortYieldFromISR( void );


#endif /* PORT_H_ */

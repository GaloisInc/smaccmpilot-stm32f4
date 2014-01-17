
#include <stdint.h>
#include "led.h"
#include "debug.h"

#define CFSR 0xE000ED28 
#define MMSR 0xE000ED28 
#define BFSR 0xE000ED29 
#define UFSR 0xE000ED2A 
#define HFSR 0xE000ED2C 
#define MMAR 0xE000ED34 
#define BFAR 0xE000ED38 
#define AFSR 0xE000ED3C 

#define REG(x) *((uint32_t*)(x))

void fault_get_regs(uint32_t *sp)
{
  /* These are volatile to try and prevent the compiler/linker
   * optimising them away as the variables never actually get used.
   * If the debugger won't show the values of the variables, make them
   * global by moving their declaration outside of this function. */
  volatile uint32_t r0;
  volatile uint32_t r1;
  volatile uint32_t r2;
  volatile uint32_t r3;
  volatile uint32_t r12;
  volatile uint32_t lr;
  volatile uint32_t pc;
  volatile uint32_t psr;
  uint32_t v;
  /* The useless expressions after the assignments are to prevent GCC
   * from warning us that these variables are assigned but not
   * used. */
  r0  = sp[0]; r0;
  r1  = sp[1]; r1;
  r2  = sp[2]; r2;
  r3  = sp[3]; r3;
  r12 = sp[4]; r12;
  lr  = sp[5]; lr;
  pc  = sp[6]; pc;
  psr = sp[7]; psr;

  debug_println("r0 r1 r2 r3 r12 lr pc psr\n");

  debug_printhex32(r0);
  debug_println("\n");
  debug_printhex32(r1);
  debug_println("\n");
  debug_printhex32(r2);
  debug_println("\n");
  debug_printhex32(r3);  debug_println("\n");
  debug_printhex32(r12);  debug_println("\n");
  debug_printhex32(lr);  debug_println("\n");
  debug_printhex32(pc);  debug_println("\n");
  debug_printhex32(psr);  debug_println("\n");


  v = REG(CFSR);
  v = REG(UFSR);

  v = REG(HFSR);
  debug_print("hard fault status register: ");
  debug_printhex32(v);
  debug_println("\n");

  v = REG(MMAR);
  debug_print("mem fault address register: ");
  debug_printhex32(v);
  v = REG(MMSR);
  debug_print(" mem fault status register: ");
  debug_printhex32(v);
  debug_println("\n");


  v = REG(BFSR);
  debug_print("bus fault status register: ");
  debug_printhex32(v);
  v = REG(BFAR);
  debug_print(" bus fault address register: ");
  debug_printhex32(v);
  debug_println("\n");

  v = REG(AFSR);
  debug_print("aux fault status register: ");
  debug_printhex32(v);
  debug_println("\n");

  debug_println("End of HardFaultHandler\n");
  led_set(1,true);
  for(;;){

  }
}

//void eHardFault_Handler(void) __attribute__((naked));

void eHardFault_Handler(void)
{

  __asm volatile (
    "tst lr, #4\n"
    "ite eq\n"
    "mrseq r0, msp\n"
    "mrsne r0, psp\n"
    "ldr r1, [r0, #24]\n"
    "ldr r2, handler2_address_const\n"
    "bx r2\n"
    "handler2_address_const: .word fault_get_regs\n"
    );
}



void debug_exception(int val){
	debug_println("debugger exception:");
	debug_printhex32(val);
	debug_println("\n");
}


#define DEBUG_EXCEPTION(arg) void debug_##arg (void){ debug_exception(arg); }

DEBUG_EXCEPTION(1)
DEBUG_EXCEPTION(2)
DEBUG_EXCEPTION(3)
DEBUG_EXCEPTION(4)
DEBUG_EXCEPTION(5)
DEBUG_EXCEPTION(6)
DEBUG_EXCEPTION(7)
DEBUG_EXCEPTION(8)
DEBUG_EXCEPTION(9)
DEBUG_EXCEPTION(10)
DEBUG_EXCEPTION(11)
DEBUG_EXCEPTION(12)
DEBUG_EXCEPTION(13)
DEBUG_EXCEPTION(14)
DEBUG_EXCEPTION(15)
DEBUG_EXCEPTION(16)
DEBUG_EXCEPTION(17)



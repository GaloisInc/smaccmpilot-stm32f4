
#ifndef __IVORY_STM32F4_INTERRUPT_H__
#define __IVORY_STM32F4_INTERRUPT_H__

#include <stdint.h>

void interrupt_num_enable(int16_t irqn);
void interrupt_num_disable(int16_t irqn);
void interrupt_num_set_priority(int16_t irqn, uint8_t priority);

#endif // __IVORY_STM32F4_INTERRUPT_H__

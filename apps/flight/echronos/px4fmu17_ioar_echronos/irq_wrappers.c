
#include <stdint.h>
#include <ctype.h>

#include <eChronos.h>
#include <rtos-kochab.h>

#define IRQ_WRAPPER(x) \
        bool eChronos_##x##_IRQHandler(void)              \
        {                                                 \
            rtos_irq_event_raise(IRQ_EVENT_ID_##x);       \
            NVIC_DisableIRQ(x##_IRQn);                    \
            return true;                                  \
        }                                                 \
        void x##_IRQHandler_wrapper(void)                 \
        {                                                 \
            extern void x##_IRQHandler(void);             \
            while (1) {                                   \
                NVIC_EnableIRQ(x##_IRQn);                 \
                rtos_signal_wait_set(SIGNAL_SET_IRQ_##x); \
                x##_IRQHandler();                         \
            }                                             \
        }


IRQ_WRAPPER(I2C2_EV)
IRQ_WRAPPER(I2C2_ER)
IRQ_WRAPPER(SPI1)
IRQ_WRAPPER(UART5)
IRQ_WRAPPER(USART1)
IRQ_WRAPPER(USART2)
IRQ_WRAPPER(USART6)
IRQ_WRAPPER(TIM1_UP_TIM10)
IRQ_WRAPPER(TIM1_CC)

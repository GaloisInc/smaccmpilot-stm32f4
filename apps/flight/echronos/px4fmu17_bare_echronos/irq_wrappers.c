
#include <stdint.h>
#include <ctype.h>

#include <eChronos.h>
#include <rtos-kochab.h>

#define IRQ_WRAPPER_NAKED(x) \
        extern void x##_IRQHandler(void);                 \
        bool eChronos_##x##_IRQHandler(void)              \
        {                                                 \
            x##_IRQHandler();                         	  \
            return true;                                  \
        }                                                 \
        void x##_IRQHandler_wrapper(void)                 \
        {                                                 \
            while (1) {                                   \
                rtos_signal_wait_set(SIGNAL_SET_IRQ_##x); \
            }                                             \
        }





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


IRQ_WRAPPER_NAKED(I2C2_EV)
IRQ_WRAPPER_NAKED(I2C2_ER)
IRQ_WRAPPER_NAKED(SPI1)
IRQ_WRAPPER_NAKED(UART5)
IRQ_WRAPPER_NAKED(USART1)
// IRQ_WRAPPER_NAKED(USART2)
IRQ_WRAPPER_NAKED(USART6)
IRQ_WRAPPER_NAKED(TIM1_UP_TIM10)
IRQ_WRAPPER_NAKED(TIM1_CC)

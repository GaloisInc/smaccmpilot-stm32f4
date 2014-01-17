
#include <rtos-kochab.h>
#include <eChronos.h>

#define UART_IRQn       UART5_IRQn
#define UART_IRQHandler UART5_IRQHandler

bool eChronos_UART_IRQHandler(void)
{
    rtos_irq_event_raise(IRQ_EVENT_ID_UART);
    NVIC_DisableIRQ(UART_IRQn);
    return true;
}

void UART_IRQHandler_task(void)
{
    while (1) {
        extern void UART_IRQHandler(void);
        rtos_signal_wait_set(SIGNAL_SET_IRQ_UART);
        UART_IRQHandler();
        NVIC_EnableIRQ(UART_IRQn);
    }
}


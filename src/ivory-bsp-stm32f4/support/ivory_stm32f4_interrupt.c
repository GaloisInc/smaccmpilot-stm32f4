
#include "ivory_stm32f4_interrupt.h"
#include <stm32f4xx.h>

static enum IRQn from_int(int16_t i);

void interrupt_num_enable(int16_t irqn) {
    NVIC_EnableIRQ(from_int(irqn));
}

void interrupt_num_disable(int16_t irqn) {
    NVIC_DisableIRQ(from_int(irqn));
}

void interrupt_num_set_priority(int16_t irqn, uint8_t priority) {
    NVIC_SetPriority(from_int(irqn), (uint32_t) priority);
}

/* is there some other way to do this trivial change?? on a plane w/o
 * stackoverflow... -pch */
static enum IRQn from_int(int16_t i) {
    switch (i) {
    case -14: return NonMaskableInt_IRQn;
    case -12: return MemoryManagement_IRQn;
    case -11: return BusFault_IRQn;
    case -10: return UsageFault_IRQn;
    case -5: return  SVCall_IRQn;
    case -4: return  DebugMonitor_IRQn;
    case -2: return  PendSV_IRQn;
    case -1: return  SysTick_IRQn;
    case 0: return   WWDG_IRQn;
    case 1: return   PVD_IRQn;
    case 2: return   TAMP_STAMP_IRQn;
    case 3: return   RTC_WKUP_IRQn;
    case 4: return   FLASH_IRQn;
    case 5: return   RCC_IRQn;
    case 6: return   EXTI0_IRQn;
    case 7: return   EXTI1_IRQn;
    case 8: return   EXTI2_IRQn;
    case 9: return   EXTI3_IRQn;
    case 10: return  EXTI4_IRQn;
    case 11: return  DMA1_Stream0_IRQn;
    case 12: return  DMA1_Stream1_IRQn;
    case 13: return  DMA1_Stream2_IRQn;
    case 14: return  DMA1_Stream3_IRQn;
    case 15: return  DMA1_Stream4_IRQn;
    case 16: return  DMA1_Stream5_IRQn;
    case 17: return  DMA1_Stream6_IRQn;
    case 18: return  ADC_IRQn;
    case 19: return  CAN1_TX_IRQn;
    case 20: return  CAN1_RX0_IRQn;
    case 21: return  CAN1_RX1_IRQn;
    case 22: return  CAN1_SCE_IRQn;
    case 23: return  EXTI9_5_IRQn;
    case 24: return  TIM1_BRK_TIM9_IRQn;
    case 25: return  TIM1_UP_TIM10_IRQn;
    case 26: return  TIM1_TRG_COM_TIM11_IRQn;
    case 27: return  TIM1_CC_IRQn;
    case 28: return  TIM2_IRQn;
    case 29: return  TIM3_IRQn;
    case 30: return  TIM4_IRQn;
    case 31: return  I2C1_EV_IRQn;
    case 32: return  I2C1_ER_IRQn;
    case 33: return  I2C2_EV_IRQn;
    case 34: return  I2C2_ER_IRQn;
    case 35: return  SPI1_IRQn;
    case 36: return  SPI2_IRQn;
    case 37: return  USART1_IRQn;
    case 38: return  USART2_IRQn;
    case 39: return  USART3_IRQn;
    case 40: return  EXTI15_10_IRQn;
    case 41: return  RTC_Alarm_IRQn;
    case 42: return  OTG_FS_WKUP_IRQn;
    case 43: return  TIM8_BRK_TIM12_IRQn;
    case 44: return  TIM8_UP_TIM13_IRQn;
    case 45: return  TIM8_TRG_COM_TIM14_IRQn;
    case 46: return  TIM8_CC_IRQn;
    case 47: return  DMA1_Stream7_IRQn;
    case 48: return  FSMC_IRQn;
    case 49: return  SDIO_IRQn;
    case 50: return  TIM5_IRQn;
    case 51: return  SPI3_IRQn;
    case 52: return  UART4_IRQn;
    case 53: return  UART5_IRQn;
    case 54: return  TIM6_DAC_IRQn;
    case 55: return  TIM7_IRQn;
    case 56: return  DMA2_Stream0_IRQn;
    case 57: return  DMA2_Stream1_IRQn;
    case 58: return  DMA2_Stream2_IRQn;
    case 59: return  DMA2_Stream3_IRQn;
    case 60: return  DMA2_Stream4_IRQn;
    case 61: return  ETH_IRQn;
    case 62: return  ETH_WKUP_IRQn;
    case 63: return  CAN2_TX_IRQn;
    case 64: return  CAN2_RX0_IRQn;
    case 65: return  CAN2_RX1_IRQn;
    case 66: return  CAN2_SCE_IRQn;
    case 67: return  OTG_FS_IRQn;
    case 68: return  DMA2_Stream5_IRQn;
    case 69: return  DMA2_Stream6_IRQn;
    case 70: return  DMA2_Stream7_IRQn;
    case 71: return  USART6_IRQn;
    case 72: return  I2C3_EV_IRQn;
    case 73: return  I2C3_ER_IRQn;
    case 74: return  OTG_HS_EP1_OUT_IRQn;
    case 75: return  OTG_HS_EP1_IN_IRQn;
    case 76: return  OTG_HS_WKUP_IRQn;
    case 77: return  OTG_HS_IRQn;
    case 78: return  DCMI_IRQn;
    case 79: return  CRYP_IRQn;
    case 80: return  HASH_RNG_IRQn;
    case 81: return  FPU_IRQn;
    default: return  FPU_IRQn; // Impossible
    }
}

/*
 * main.c
 *
 * Copyright (C) 2012, Galois, Inc.
 * All Rights Reserved.
 *
 * This software is released under the "BSD3" license.  Read the file
 * "LICENSE" for more information.
 */

#include <FreeRTOS.h>
#include <task.h>

#include <stm32f4xx.h>
#include <hwf4/spi.h>
#include <hwf4/usart.h>

#include <stdio.h>
#include <string.h>

xTaskHandle main_task_handle;

/* LIS302DL register definitions. */
#define LIS302DL_REG_WHO_AM_I      0x0F
#define LIS302DL_REG_CTRL_REG_1    0x20
#define LIS302DL_REG_OUT_X         0x29
#define LIS302DL_REG_OUT_Y         0x2B
#define LIS302DL_REG_OUT_Z         0x2D

/* Expected device ID of the LIS302DL. */
#define LIS302DL_DEVICE_ID         0x3B

/** LIS302DL on the STM32F4 discovery board. */
struct spi_device lis302dl = {
  pin_e3,
  false,
  SPI_BAUD_DIV_128,
  SPI_CLOCK_POLARITY_HIGH,
  SPI_CLOCK_PHASE_2,
  SPI_BIT_ORDER_MSB_FIRST
};

void usart_puts(const char *s)
{
  usart_write(usart1, (uint8_t *) s, strlen(s));
  usart_write(usart1, (uint8_t *) "\r\n", 2);
}

/** Write a register to the LIS302DL. */
void lis302dl_write_reg(uint8_t addr, uint8_t value)
{
  uint8_t buf[2];

  buf[0] = addr;
  buf[1] = value;

  spi_transfer(spi1, &lis302dl, portMAX_DELAY, buf, buf, sizeof(buf));
}

/** Read a register from the LIS302DL. */
uint8_t lis302dl_read_reg(uint8_t addr)
{
  uint8_t buf[2];

  buf[0] = addr | 0x80;
  buf[1] = 0x00;

  spi_transfer(spi1, &lis302dl, portMAX_DELAY, buf, buf, sizeof(buf));
  return buf[1];
}

void main_task(void *args)
{
  char buf[64];

  usart_init(usart1, 115200);
  usart_enable(usart1);
  spi_init(spi1);
  spi_device_init(&lis302dl);

  /* Read the WHO_AM_I register to get the device ID. */
  uint8_t dev_id = lis302dl_read_reg(LIS302DL_REG_WHO_AM_I);
  if (dev_id == LIS302DL_DEVICE_ID)
    usart_puts("lis302dl: valid device id");
  else
    usart_puts("lis302dl: invalid device id");

  /* Configure the accelerometer for normal power mode, 100Hz data
   * rate, and XYZ axes enabled. */
  lis302dl_write_reg(LIS302DL_REG_CTRL_REG_1, 0x47);

  /* Poll the XYZ values once per second and print them to the
   * UART. */
  for(;;) {
    int8_t x = lis302dl_read_reg(LIS302DL_REG_OUT_X);
    int8_t y = lis302dl_read_reg(LIS302DL_REG_OUT_Y);
    int8_t z = lis302dl_read_reg(LIS302DL_REG_OUT_Z);

    snprintf(buf, sizeof(buf), "%d %d %d", x, y, z);
    usart_puts(buf);

    vTaskDelay(250);
  }
}

int main(void)
{
  xTaskCreate(main_task, (signed char *)"main_task", 1000, NULL, 0,
              &main_task_handle);

  vTaskStartScheduler();

  for(;;)
    ;

  return 0;
}

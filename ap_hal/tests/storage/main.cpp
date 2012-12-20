/*
 * main.cpp --- AP_HAL storage driver test.
 *
 * Copyright (C) 2012, Galois, Inc.
 * All Rights Reserved.
 *
 * This software is released under the "BSD3" license.  Read the file
 * "LICENSE" for more information.
 */

#include <stdint.h>

#include <FreeRTOS.h>
#include <task.h>

#if CONFIG_HAL_BOARD == HAL_BOARD_SMACCM
# include <AP_HAL_SMACCM.h>
#else
# error "Unsupported CONFIG_HAL_BOARD type."
#endif

// There is apparently a dependency within the HAL implementation on
// the HAL being in a global variable named "hal".
const AP_HAL::HAL& hal = AP_HAL_BOARD_DRIVER;

static xTaskHandle htask;

struct TestData {
  uint8_t  byte;
  uint16_t word;
  uint32_t dword;
  uint8_t  buf[32];
};

bool operator==(const TestData& lhs, const TestData& rhs)
{
  if (lhs.byte != rhs.byte)
    return false;
  if (lhs.word != rhs.word)
    return false;
  if (lhs.dword != rhs.dword)
    return false;
  if (memcmp(lhs.buf, rhs.buf, sizeof(lhs.buf)) != 0)
    return false;

  return true;
}

bool operator!=(const TestData& lhs, const TestData& rhs)
{
  return !(lhs == rhs);
}

static const TestData g_test_data = {
  0xAA,
  0xC1D2,
  0x0F1E2D3C,
  {
    0x00, 0x10, 0x20, 0x30,  0x40, 0x50, 0x60, 0x70,
    0x01, 0x11, 0x21, 0x31,  0x41, 0x51, 0x61, 0x71,
    0x02, 0x12, 0x22, 0x32,  0x42, 0x52, 0x62, 0x72,
    0x03, 0x13, 0x23, 0x33,  0x43, 0x53, 0x63, 0x73
  }
};

void write_test_data(uint16_t addr, const TestData *data)
{
  hal.storage->write_byte(addr, data->byte);
  addr += sizeof(data->byte);
  hal.storage->write_word(addr, data->word);
  addr += sizeof(data->word);
  hal.storage->write_dword(addr, data->dword);
  addr += sizeof(data->dword);
  hal.storage->write_block(addr, (uint8_t*)data->buf, sizeof(data->buf));
}

void read_test_data(uint16_t addr, TestData *data)
{
  data->byte = hal.storage->read_byte(addr);
  addr += sizeof(data->byte);
  data->word = hal.storage->read_word(addr);
  addr += sizeof(data->word);
  data->dword = hal.storage->read_dword(addr);
  addr += sizeof(data->dword);
  hal.storage->read_block(data->buf, addr, sizeof(data->buf));
}

void main_task(void *args)
{
  hal.init(0, NULL);
  hal.console->printf("AP_HAL Storage Test\r\n");

  for (uint16_t addr = 0; addr < 0x1000; addr += (sizeof(TestData) + 1)) {
    write_test_data(addr, &g_test_data);

    TestData data;
    memset(&data, 0, sizeof(data));
    read_test_data(addr, &data);

    if (data != g_test_data) {
      hal.console->printf("!BAD test data at 0x%04X\r\n", addr);
    } else {
      hal.console->printf("good test data at 0x%04X\r\n", addr);
    }
  }

  for (;;) {
    vTaskDelay(1000);
  }
}

extern "C" int main(void)
{
  xTaskCreate(main_task, (signed char *)"main", 1024, NULL, 0, &htask);

  vTaskStartScheduler();

  for (;;)
    ;

  return 0;
}

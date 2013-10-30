/*
 * UARTDriver.cpp --- AP_HAL_SMACCM UART driver.
 *
 * Copyright (C) 2012, Galois, Inc.
 * All Rights Reserved.
 *
 * This software is released under the "BSD3" license.  Read the file
 * "LICENSE" for more information.
 */

#include "UARTDriver.h"

#include <stdio.h>              // for vsnprintf

using namespace SMACCM;

// XXX the AVR driver enables itself in the constructor.  This seems
// like a very bad idea, since it will run somewhere in the startup
// code before our clocks are all set up and such.
SMACCMUARTDriver::SMACCMUARTDriver()
{
}

void SMACCMUARTDriver::begin(uint32_t baud)
{
}

// XXX buffer sizes ignored
void SMACCMUARTDriver::begin(uint32_t baud, uint16_t rxS, uint16_t txS)
{
  begin(baud);
}

// XXX hwf4 doesn't support de-initializing a USART
void SMACCMUARTDriver::end()
{
}

// XXX hwf4 doesn't support flushing, could be tricky to get the
// synchronization right.  Would we just force the TX/RX queues to
// empty?
void SMACCMUARTDriver::flush()
{
}

bool SMACCMUARTDriver::is_initialized()
{
  return false;
}

void SMACCMUARTDriver::set_blocking_writes(bool blocking)
{
}

bool SMACCMUARTDriver::tx_pending()
{
  return false;
}

/* SMACCM implementations of BetterStream virtual methods */
void SMACCMUARTDriver::print_P(const prog_char_t *pstr)
{
  while (*pstr)
    write(*pstr++);
}

void SMACCMUARTDriver::println_P(const prog_char_t *pstr)
{
  print_P(pstr);
  println();
}

// XXX this will be changing, putting this on the stack hurts but
// allows us to be easily re-entrant
void SMACCMUARTDriver::printf(const char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  vprintf(fmt, ap);
  va_end(ap);
}

void SMACCMUARTDriver::_printf_P(const prog_char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  vprintf(fmt, ap);
  va_end(ap);
}

void SMACCMUARTDriver::vprintf(const char *pstr, va_list ap)
{
  char buf[128];
  vsnprintf(buf, sizeof(buf), pstr, ap);
  print(buf);
}

void SMACCMUARTDriver::vprintf_P(const prog_char *pstr, va_list ap)
{
  vprintf(pstr, ap);
}

/* SMACCM implementations of Stream virtual methods */
int16_t SMACCMUARTDriver::available()
{
  return 0;
}

int16_t SMACCMUARTDriver::txspace()
{
  return 0;
}

// It looks like this should always be a non-blocking read, so return
// -1 if there is nothing to receive immediately.
int16_t SMACCMUARTDriver::read()
{
  return -1;
}

/* SMACCM implementations of Print virtual methods */
size_t SMACCMUARTDriver::write(uint8_t c)
{
  return 1;
}


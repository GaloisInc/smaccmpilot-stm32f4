/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * console_prim.c --- C string primitives.
 *
 * Copyright (C) 2013, Galois, Inc.
 * All Rights Reserved.
 */

#include "smaccmpilot/console_prim.h"

#include <AP_HAL.h>

extern const AP_HAL::HAL& hal;

void console_write_u8(uint8_t x)
{
    hal.console->print((uint16_t)x); /* prevent 'char' overload */
}

void console_write_s8(int8_t x)
{
    hal.console->print((int16_t)x); /* prevent 'char' overload */
}

void console_write_u16(uint16_t x)
{
    hal.console->print(x);
}

void console_write_s16(int16_t x)
{
    hal.console->print(x);
}

void console_write_u32(uint32_t x)
{
    hal.console->print(x);
}

void console_write_s32(int32_t x)
{
    hal.console->print(x);
}

/* 64-bit printers not supported by AP_HAL apparently. */
#if 0
void console_write_u64(uint64_t x)
{
    hal.console->print(x);
}

void console_write_s64(int64_t x)
{
    hal.console->print(x);
}
#endif

void console_write_float(float x)
{
    hal.console->print(x);
}

void console_write_double(double x)
{
    hal.console->print(x);
}

void console_write_string(const char *x)
{
    hal.console->write(x);
}

void console_write_string_n(const char *x, int32_t len)
{
    while (len > 0 && *x) {
        hal.console->write(*x);
        ++x;
        --len;
    }
}

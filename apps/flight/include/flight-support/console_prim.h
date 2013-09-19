/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * console_prim.h --- SMACCMPilot console primitives.
 *
 * Copyright (C) 2013, Galois, Inc.
 * All Rights Reserved.
 */

#ifndef __FLIGHT_SUPPORT_CONSOLE_PRIM_H__
#define __FLIGHT_SUPPORT_CONSOLE_PRIM_H__

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

void console_write_u8(uint8_t x);
void console_write_s8(int8_t x);
void console_write_u16(uint16_t x);
void console_write_s16(int16_t x);
void console_write_u32(uint32_t x);
void console_write_s32(int32_t x);
void console_write_u64(uint64_t x);
void console_write_s64(int64_t x);
void console_write_float(float x);
void console_write_double(double x);
void console_write_string(const char *x);
void console_write_string_n(const char *x, int32_t len);

#ifdef __cplusplus
}
#endif

#endif  /* !defined __FLIGHT_SUPPORT_CONSOLE_PRIM_H__ */

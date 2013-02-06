/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * ivory_string_prim.h --- C string primitives.
 *
 * Copyright (C) 2013, Galois, Inc.
 * All Rights Reserved.
 */

#ifndef __SMACCMPILOT_IVORY_STRING_PRIM_H__
#define __SMACCMPILOT_IVORY_STRING_PRIM_H__

#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

size_t ivory_strlcpy(char *dest, const char *src, size_t size);

#ifdef __cplusplus
}
#endif

#endif  /* !defined __SMACCMPILOT_IVORY_STRING_PRIM_H__ */

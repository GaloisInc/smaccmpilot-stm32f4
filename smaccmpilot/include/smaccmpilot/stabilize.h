// -*- Mode: C++; indent-tabs-mode: nil; c-basic-offset: 4 -*-
/*
 * stabilize.h --- Simple stabilizer for SMACCMPilot.
 *
 * Copyright (C) 2012, Galois, Inc.
 * All Rights Reserved.
 *
 * This software is released under the "BSD3" license.  Read the file
 * "LICENSE" for more information.
 */

#ifndef __APP_STABILIZE_HIL_STABILIZE_H__
#define __APP_STABILIZE_HIL_STABILIZE_H__

#ifdef __cplusplus
extern "C" {
#endif

/* Forward declarations. */
struct userinput_result;
struct sensors_result;
struct motorsoutput_result;

/**
 * Write output to the motors given input from the user and sensors,
 * stabilizing the roll, pitch, and yaw.
 */
void stabilize_motors(const struct userinput_result *user,
                      const struct sensors_result *sensors,
                      struct motorsoutput_result *motors);

#ifdef __cplusplus
}
#endif

#endif  /* !defined __APP_STABILIZE_HIL_STABILIZE_H__ */

// -*- Mode: C++; indent-tabs-mode: nil; c-basic-offset: 4 -*-
/*
 * stabilize.c --- Simple stabilizer for SMACCMPilot.
 *
 * Copyright (C) 2012, Galois, Inc.
 * All Rights Reserved.
 *
 * This software is released under the "BSD3" license.  Read the file
 * "LICENSE" for more information.
 */

#include <stdbool.h>
#include <stdint.h>

#include <smaccmpilot/userinput.h>
#include <smaccmpilot/sensors.h>
#include <smaccmpilot/stabilize.h>
#include <smaccmpilot/motorsoutput.h>

/* ivory generated pid code: */
#include <smaccmpilot/pid_stabilize.h>

#define MAX_INPUT_ROLL   45.0f  /* deg */
#define MAX_INPUT_PITCH  45.0f  /* deg */
#define MAX_INPUT_YAW    180.0f  /* deg/sec */

/* These numbers are from ArduPilot.  Do they still make sense? */
#define MAX_OUTPUT_ROLL  50.0f  /* deg/sec */
#define MAX_OUTPUT_PITCH 50.0f  /* deg/sec */
#define MAX_OUTPUT_YAW   45.0f  /* deg/sec */

void stabilize_motors(const struct userinput_result *in,
                      const struct sensors_result *sensors,
                      struct motorsoutput_result *out)
{
    out->roll  = stabilize_from_angle(&g_pid_roll_stabilize, &g_pid_roll_rate,
                                      in->roll, MAX_INPUT_ROLL,
                                      sensors->roll, sensors->omega_x,
                                      MAX_OUTPUT_ROLL);

    out->pitch = stabilize_from_angle(&g_pid_pitch_stabilize, &g_pid_pitch_rate,
                                      -in->pitch, MAX_INPUT_PITCH,
                                      sensors->pitch, sensors->omega_y,
                                      MAX_OUTPUT_PITCH);

    out->yaw   = stabilize_from_rate(&g_pid_yaw_rate, in->yaw, MAX_INPUT_YAW,
                                     sensors->omega_z, MAX_OUTPUT_YAW);

    out->armed    = in->armed;
    out->throttle = in->throttle;
    out->time     = in->time;
}

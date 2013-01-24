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
#define MAX_INPUT_YAW    45.0f  /* deg */

/* These numbers are from ArduPilot.  Do they still make sense? */
#define MAX_OUTPUT_ROLL  50.0f  /* deg/sec */
#define MAX_OUTPUT_PITCH 50.0f  /* deg/sec */
#define MAX_OUTPUT_YAW   45.0f  /* deg/sec */

// These PID gains are tuned experiementally for the "pysim" simulator
// and will surely be wrong for any real copter.  We will need
// something more dynamic than hardcoding these here.

static struct PID g_pi_roll_stabilize = {
    1.3f,                       // p_gain
    0.0f,                       // i_gain
    0.0f,                       // i_state
    -8.0f,                      // i_min
    8.0f,                       // i_max
};

static struct PID g_pi_roll_rate = {
    0.05f,                      // p_gain
    0.015f,                     // i_gain
    0.0f,                       // i_state
    -5.0f,                      // i_min
    5.0f,                       // i_max
};

static struct PID g_pi_pitch_stabilize = {
    1.3f,                       // p_gain
    0.0f,                       // i_gain
    0.0f,                       // i_state
    -8.0f,                      // i_min
    8.0f,                       // i_max
};

static struct PID g_pi_pitch_rate = {
    0.05f,                      // p_gain
    0.015f,                     // i_gain
    0.0f,                       // i_state
    -5.0f,                      // i_min
    5.0f,                       // i_max
};

#if 0                           // not used currently
static struct PID g_pi_yaw_stabilize = {
    1.3f,                       // p_gain
    0.0f,                       // i_gain
    0.0f,                       // i_state
    -8.0f,                      // i_min
    8.0f,                       // i_max
};
#endif

static struct PID g_pi_yaw_rate = {
    0.05f,                      // p_gain
    0.015f,                     // i_gain
    0.0f,                       // i_state
    -8.0f,                      // i_min
    8.0f,                       // i_max
};

void stabilize_motors(const struct userinput_result *in,
                      const struct sensors_result *sensors,
                      struct motorsoutput_result *out)
{
    out->roll  = stabilize_from_angle(&g_pi_roll_stabilize, &g_pi_roll_rate,
                                      in->roll, MAX_INPUT_ROLL,
                                      sensors->roll, sensors->omega_x,
                                      MAX_OUTPUT_ROLL);

    out->pitch = stabilize_from_angle(&g_pi_pitch_stabilize, &g_pi_pitch_rate,
                                      -in->pitch, MAX_INPUT_PITCH,
                                      sensors->pitch, sensors->omega_y,
                                      MAX_OUTPUT_PITCH);

    out->yaw   = stabilize_from_rate(&g_pi_yaw_rate, in->yaw, MAX_INPUT_YAW,
                                     sensors->omega_z, MAX_OUTPUT_YAW);

    out->armed    = in->armed;
    out->throttle = in->throttle;
    out->time     = in->time;
}

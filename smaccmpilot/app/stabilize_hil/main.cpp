// -*- Mode: C++; indent-tabs-mode: nil; c-basic-offset: 4 -*-
/*
 * main.cpp --- AP_HAL HIL based helicopter stabilizer
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

#include <AP_HAL_SMACCM.h>
#include <AP_Math.h>

#include <smaccmpilot/userinput.h>
#include <smaccmpilot/motorsoutput.h>
#include "gcs.h"

const AP_HAL::HAL& hal = AP_HAL_BOARD_DRIVER;

// Handle to the main thread.
static xTaskHandle g_main_task;

// Print user input to the console.
static void userinput_show(const struct userinput_result *data)
{
    hal.console->printf("user: %s roll %.2f  pitch %.2f  yaw %.2f  throttle %.2f\r\n",
                        data->armed ? "armed" : "disarmed",
                        data->roll, data->pitch, data->yaw, data->throttle);
}

// Print sensor data to the console.
static void sensors_show(const struct sensors_result *data)
{
    hal.console->printf("sensors: roll %.2f  pitch %.2f  yaw %.2f  alt %.2f\r\n",
                        degrees(data->roll), degrees(data->pitch),
                        degrees(data->yaw), data->baro_alt);
}

//////////////////////////////////////////////////////////////////////
// PI Controller Stabilization
//
// Once this is working and tested, convert it to Ivory!

struct pi_controller
{
    float p_gain;               // proportional gain
    float i_gain;               // integral gain
    float i_state;              // integrator state
    float i_min;                // min integrator state
    float i_max;                // max integrator state
};

static float pi_update(struct pi_controller *pi, float error)
{
    float p_term, i_term;

    p_term = pi->p_gain * error;

    pi->i_state += error;
    if (pi->i_state < pi->i_min)
        pi->i_state = pi->i_min;
    else if (pi->i_state > pi->i_max)
        pi->i_state = pi->i_max;
    i_term = pi->i_gain * pi->i_state;

    return p_term + i_term;
}

static struct pi_controller g_pi_roll_stabilize = {
    1.5f,                       // p_gain
    0.0f,                       // i_gain
    0.0f,                       // i_state
    -8.0f,                      // i_min
    8.0f,                       // i_max
};

static struct pi_controller g_pi_roll_rate = {
    0.06f,                      // p_gain
    0.01f,                      // i_gain
    0.0f,                       // i_state
    -5.0f,                      // i_min
    5.0f,                       // i_max
};

#define MAX_INPUT_ROLL  45.0f    // deg
#define MAX_INPUT_PITCH 45.0f    // deg
#define MAX_INPUT_YAW   45.0f    // deg

static float stabilize_roll(float desired_roll, float measured_roll,
                            float measured_rate)
{
    float desired_roll_deg  = desired_roll * MAX_INPUT_ROLL;
    float measured_roll_deg = degrees(measured_roll);
    float roll_error        = desired_roll_deg - measured_roll_deg;
    float rate              = pi_update(&g_pi_roll_stabilize, roll_error);
    float measured_rate_deg = degrees(measured_rate);
    float rate_error        = rate - measured_rate_deg;
    float result            = pi_update(&g_pi_roll_rate, rate_error);

    result = constrain(result, -50.0, 50.0);

    // hal.console->printf("\r\nroll:  d %.3fdeg  m %.3fdeg  result %.3fdeg/s\r\n",
    //                     desired_roll_deg, measured_roll_deg, result);

    return result / 50.0f;      // scale to [-1.0f, 1.0f]
}

static struct pi_controller g_pi_pitch_stabilize = {
    1.5f,                       // p_gain
    0.0f,                       // i_gain
    0.0f,                       // i_state
    -8.0f,                      // i_min
    8.0f,                       // i_max
};

static struct pi_controller g_pi_pitch_rate = {
    0.06f,                      // p_gain
    0.01f,                      // i_gain
    0.0f,                       // i_state
    -5.0f,                      // i_min
    5.0f,                       // i_max
};

static float stabilize_pitch(float desired_pitch, float measured_pitch,
                             float measured_rate)
{
    float desired_pitch_deg  = desired_pitch * MAX_INPUT_PITCH;
    float measured_pitch_deg = degrees(measured_pitch);
    float pitch_error        = desired_pitch_deg - measured_pitch_deg;
    float rate               = pi_update(&g_pi_pitch_stabilize, pitch_error);
    float measured_rate_deg  = degrees(measured_rate);
    float rate_error         = rate - measured_rate_deg;
    float result             = pi_update(&g_pi_pitch_rate, rate_error);

    result = constrain(result, -50.0, 50.0);

    // hal.console->printf("pitch: d %.3fdeg  m %.3fdeg  result %.3fdeg/s\r\n",
    //                     desired_pitch_deg, measured_pitch_deg, result);

    return result / 50.0f;      // scale to [-1.0f, 1.0f]
}

static struct pi_controller g_pi_yaw_stabilize = {
    1.5f,                       // p_gain
    0.0f,                       // i_gain
    0.0f,                       // i_state
    -8.0f,                      // i_min
    8.0f,                       // i_max
};

static struct pi_controller g_pi_yaw_rate = {
    0.06f,                      // p_gain
    0.01f,                      // i_gain
    0.0f,                       // i_state
    -8.0f,                      // i_min
    8.0f,                       // i_max
};

static float stabilize_yaw(float desired_yaw, float measured_yaw,
                           float measured_rate)
{
    float desired_yaw_deg   = desired_yaw * MAX_INPUT_YAW;
    float measured_yaw_deg  = degrees(measured_yaw);
    float yaw_error         = desired_yaw_deg - measured_yaw_deg;
    float rate              = pi_update(&g_pi_yaw_stabilize, yaw_error);
    float measured_rate_deg = degrees(measured_rate);
    float rate_error        = rate - measured_rate_deg;
    float result            = pi_update(&g_pi_yaw_rate, rate_error);

    result = constrain(result, -45.0, 45.0);

    // hal.console->printf("yaw:   d %.3fdeg  m %.3fdeg  result %.3fdeg/s\r\n",
    //                     desired_yaw_deg, measured_yaw_deg, result);

    return result / 50.0f;      // scale to [-1.0f, 1.0f]
}

static void stabilize(const struct userinput_result *in,
                      const struct sensors_result *sensors,
                      struct motorsoutput_result *out)
{
    out->roll  = stabilize_roll (in->roll,  sensors->roll,  sensors->omega_x);
    out->pitch = stabilize_pitch(in->pitch, sensors->pitch, sensors->omega_y);
    out->yaw   = stabilize_yaw  (in->yaw,   sensors->yaw,   sensors->omega_z);
}

// Process incoming user input and fill in the appropriate motor
// output.  This will be replaced with a PID controller.
static void userinput_to_motorsoutput(const struct userinput_result *in,
                                      struct motorsoutput_result *out)
{
    out->armed    = true; // in->armed;
    out->throttle = in->throttle;
//    out->roll     = in->roll;
//    out->pitch    = in->pitch;
//    out->yaw      = in->yaw;
    out->time     = in->time;
}

// Main thread.  Starts up the GCS thread to communicate with the
// host, then processes incoming sensor data and writes servo output
// back to MAVLink.
void main_task(void *arg)
{
    hal.init(0, NULL);
    userinput_init();
    motorsoutput_init();
    gcs_init();

    for (;;) {
        struct userinput_result input;
        struct sensors_result sensors;
        struct motorsoutput_result motors;

        userinput_get(&input);
        gcs_sensors_get(&sensors);
        sensors_show(&sensors);
        stabilize(&input, &sensors, &motors);
        userinput_to_motorsoutput(&input, &motors);
        motorsoutput_set(&motors);

        vTaskDelay(100);
    }
}

extern "C"
int main()
{
    xTaskCreate(main_task, (signed char *)"main", 1024, NULL, 0, &g_main_task);
    vTaskStartScheduler();

    for (;;)
        ;

    return 0;
}

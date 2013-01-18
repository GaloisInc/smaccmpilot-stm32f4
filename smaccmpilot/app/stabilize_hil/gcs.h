// -*- Mode: C++; indent-tabs-mode: nil; c-basic-offset: 4 -*-
/*
 * gcs.h --- GCS communication with MAVLink.
 *
 * Copyright (C) 2012, Galois, Inc.
 * All Rights Reserved.
 *
 * This software is released under the "BSD3" license.  Read the file
 * "LICENSE" for more information.
 */

#ifndef __APP_STABILIZE_HIL_GCS_H__
#define __APP_STABILIZE_HIL_GCS_H__

#ifdef __cplusplus
extern "C" {
#endif

// Forward declaration---defined in "motorsoutput.h".
struct servo_result;

// Sensor data received from the host via MAVLink.
struct sensors_result {
    bool valid;
    /* roll, pitch, and yaw in radians */
    float roll;
    float pitch;
    float yaw;
    /* omega in rad/s */
    float omega_x;
    float omega_y;
    float omega_z;
    /* altitude in meters */
    float baro_alt;
    /* latitude and longitude (*1E7) */
    int32_t lat;
    int32_t lon;
    int32_t gps_alt;            // alt in m*1000
    /* ground speed in m/s*100 */
    int16_t vx;
    int16_t vy;
    int16_t vz;
    /* acceleration in mg */
    int16_t xacc;
    int16_t yacc;
    int16_t zacc;
};

// Initialize the GCS, starting a task for MAVLink.
void gcs_init();

// Read HIL sensor data from the GCS.  Returns true if valid data was
// available.
bool gcs_sensors_get(struct sensors_result *sensors);

// Write servo data to the GCS.
void gcs_servos_set(const struct servo_result *servo);

#ifdef __cplusplus
}
#endif

#endif   // !defined __APP_STABILIZE_HIL_GCS_H__

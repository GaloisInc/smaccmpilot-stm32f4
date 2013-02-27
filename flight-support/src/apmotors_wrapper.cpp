
#include "apmotors_wrapper.h"

#include <FreeRTOS.h>
#include <task.h>
#include <semphr.h>

#include <hwf4/timer.h>

#include <AP_HAL.h>
#include <RC_Channel.h>
#include <AP_Motors.h>

extern const AP_HAL::HAL& hal;


/* copying arducopter's minimum throttle of 130. dont really know why? */
#define THROTTLE_MINIMUM 130
#define THROTTLE_MAXIMUM 1000

/* I don't approve of RC_Channels being hooked up to rcin/rcout IO, so
 * we're giving them all bogus channel values so they can do no harm. */
static RC_Channel s_roll     (255);
static RC_Channel s_pitch    (255);
static RC_Channel s_throttle (255);
static RC_Channel s_yaw      (255);

static AP_MotorsQuad motors(&s_roll, &s_pitch, &s_throttle, &s_yaw);

static xSemaphoreHandle apmotors_mutex;

static void angular_channel_setup(RC_Channel* ch) {
    ch->set_angle(4500);
    ch->set_type(RC_CHANNEL_TYPE_ANGLE_RAW);
}

static void throttle_channel_setup(RC_Channel* ch) {
    ch->set_range(THROTTLE_MINIMUM, THROTTLE_MAXIMUM);
    ch->set_range_out(0,1000);
}

void apmotors_output_init(void) {

    apmotors_mutex = xSemaphoreCreateMutex();

    angular_channel_setup(&s_roll);
    angular_channel_setup(&s_pitch);
    angular_channel_setup(&s_yaw);
    throttle_channel_setup(&s_throttle);

    motors.set_update_rate(50); /* Is this appropriate with IOAR?
                                     Otherwise? */
    motors.set_frame_orientation(AP_MOTORS_X_FRAME);
    motors.Init();
    motors.set_min_throttle(THROTTLE_MINIMUM);
    motors.set_max_throttle(THROTTLE_MAXIMUM);

    motors.enable();
    motors.output_min();
    motors.auto_armed(true);
}


static int16_t angular_scale(float input) {
    /* Angular channels expect a value from -4500 to +4500
     * input should be from -1.0f to 1.0f */
    if (input < -1.0f) input = -1.0f;
    if (input >  1.0f) input =  1.0f;
    return (int16_t) (4500.0f * input);
}

static int16_t throttle_scale(float input) {
    /* Throttle expects a value from 0 to 1000
     * input should be from 0.0f to 1.0f */ 
    if (input < 0.0f) input = 0.0f;
    if (input > 1.0f) input = 1.0f;
    return (int16_t) (1000.0f * input);
}

void apmotors_output_get(struct servo_result *servo) {
    /* wait 2 ticks - sometimes motors.output() can take some time.*/
    if (xSemaphoreTake(apmotors_mutex, 2)) {
        servo->valid = true;
        servo->servo1 = motors.motor_out[0];
        servo->servo2 = motors.motor_out[1];
        servo->servo3 = motors.motor_out[2];
        servo->servo4 = motors.motor_out[3];
        servo->time = (uint32_t)timer_get_ticks();
        xSemaphoreGive(apmotors_mutex);
    } else {
        hal.scheduler->panic("PANIC: apmotors_output_get took too long to grab "
                "memory barrier (should never happen).");
    }
}

void apmotors_output_set(const struct motorsoutput_result *state) {

    if (motors.armed() && !(state->armed)) {
        motors.armed(false);
    } else if (!motors.armed() && (state->armed)) {
        motors.armed(true);
    }

    if (xSemaphoreTake(apmotors_mutex, 1)) {
        s_roll.servo_out     = angular_scale(state->roll);
        s_pitch.servo_out    = angular_scale(state->pitch);
        s_yaw.servo_out      = angular_scale(state->yaw);
        s_throttle.servo_out = throttle_scale(state->throttle);
        motors.output();
        xSemaphoreGive(apmotors_mutex);
    } else {
        hal.scheduler->panic("PANIC: apmotors_output_set took too long to grab "
                "memory barrier (should never happen).");

    }
}


#include "sensors.h"

#include <FreeRTOS.h>
#include <task.h>
#include <semphr.h>

#include <hwf4/led.h>

#include <AP_AHRS.h>
#include <AP_Baro.h>
#include <AP_Compass_HMC5843.h>
#include <AP_InertialSensor_MPU6000.h>
#include <AP_Param.h>
#include <AP_HAL.h>
extern const AP_HAL::HAL& hal;

static void sensors_task(void* arg);
static void flash_leds(bool on);

static void sensors_begin(); 
static void sensors_update();
static void sensors_getstate(struct sensors_result *state);
static void sensors_share(const struct sensors_result *state);

// The rate we run the inertial sensor at.
static const AP_InertialSensor::Sample_rate INS_SAMPLE_RATE =
    AP_InertialSensor::RATE_200HZ;

static AP_InertialSensor_MPU6000 g_ins;
static AP_Compass_HMC5843 g_compass;
static AP_Baro_MS5611 g_baro(&AP_Baro_MS5611::i2c);
static GPS *g_gps;
static AP_AHRS_DCM g_ahrs(&g_ins, g_gps);

static xTaskHandle sensors_task_handle;
static xSemaphoreHandle sensors_mutex;

static struct sensors_result sensors_shared_state;

void sensors_init(void) {
    sensors_mutex = xSemaphoreCreateMutex();
    xTaskCreate(sensors_task, (signed char *)"sens", 1024, NULL, 0,
            &sensors_task_handle);
}

/* sensors_get: for external threads to grab the shared state */
void sensors_get(struct sensors_result *input) {
    if (xSemaphoreTake(sensors_mutex, 1)) {
        memcpy(input, &sensors_shared_state, sizeof(struct sensors_result));
        xSemaphoreGive(sensors_mutex);
    } else {
        hal.scheduler->panic("PANIC: sensors_get took too long to take "
                "memory barrier");
    }
}

/* sensors_get: for internal thread to update the shared state */
static void sensors_share(const struct sensors_result *capt) {
    if (xSemaphoreTake(sensors_mutex, 1)) {
        memcpy(&sensors_shared_state, capt, sizeof(struct sensors_result));
        xSemaphoreGive(sensors_mutex);
    } else {
        hal.scheduler->panic("PANIC: sensors_share took too long to take "
                "memory barrier");
    }
}

static void sensors_task(void* arg) {
    struct sensors_result state = {0};
    sensors_share(&state);
    sensors_begin();

    portTickType last_wake = xTaskGetTickCount();

    for (;;) {
        // Delay to run this loop at 100Hz.
        vTaskDelayUntil(&last_wake, 10);
        sensors_update();
        sensors_getstate(&state);
        sensors_share(&state);
    }

}

static void sensors_begin(void) {

    hal.console->printf("init AP_InertialSensor: ");
    g_ins.init(AP_InertialSensor::COLD_START, INS_SAMPLE_RATE, flash_leds);
    g_ins.init_accel(flash_leds);
    hal.console->println();
    led_set(0, false);
    hal.console->printf("done\r\n");

    hal.console->printf("init AP_Compass: ");
    g_compass.init();
    g_compass.set_orientation(AP_COMPASS_COMPONENTS_DOWN_PINS_BACK);
    g_compass.set_offsets(0,0,0);
    g_compass.set_declination(ToRad(0.0));
    hal.console->printf("done\r\n");

    hal.console->printf("init AP_Baro: ");
    g_baro.init();
    g_baro.calibrate();
    hal.console->printf("done\r\n");

    hal.console->printf("init AP_AHRS: ");
    g_ahrs.init();
    g_ahrs.set_compass(&g_compass);
    g_ahrs.set_barometer(&g_baro);
    hal.console->printf("sensors init done\r\n");

}

static void sensors_update() {
    static portTickType last_compass = 0;

    portTickType now = xTaskGetTickCount();

    if (last_compass == 0 || now - last_compass > 100) {
        last_compass = now;
        g_compass.read();
        g_baro.read();
    }

    g_ahrs.update();
    g_compass.null_offsets();

}

static void sensors_getstate(struct sensors_result *capt) {
    capt->valid   = true;      
    capt->roll    = g_ahrs.roll;
    capt->pitch   = g_ahrs.pitch;
    capt->yaw     = g_ahrs.yaw;
    /* ahrs.get_gyro gives a smoothed (gyro, drift & offset compensated)
     * omega (body frame rate) output */
    const Vector3f omega = g_ahrs.get_gyro();
    capt->omega_x = omega.x;
    capt->omega_y = omega.y;
    capt->omega_z = omega.z;
    /* altitude is only filtered by AP_Baro, no inertial compensation */
    capt->baro_alt = g_baro.get_altitude();
}

static void sensors_debug() {
    static int divider = 0;
    if (divider++ == 20) {
        divider = 0;
        float heading = g_compass.calculate_heading(g_ahrs.get_dcm_matrix());

        hal.console->printf("ahrs: roll %4.1f pitch %4.1f "
                            "yaw %4.1f hdg %.1f\r\n",
                            ToDeg(g_ahrs.roll), ToDeg(g_ahrs.pitch),
                            ToDeg(g_ahrs.yaw),
                            g_compass.use_for_yaw() ? ToDeg(heading):0.0f);

        Vector3f accel(g_ins.get_accel());
        Vector3f gyro(g_ins.get_gyro());
        hal.console->printf("mpu6000: accel %.2f %.2f %.2f "
                            "gyro %.2f %.2f %.2f\r\n",
                            accel.x, accel.y, accel.z,
                            gyro.x, gyro.y, gyro.z);

        hal.console->printf("compass: heading %.2f deg\r\n",
                            ToDeg(g_compass.calculate_heading(0, 0)));
    }
}

static void flash_leds(bool on) {
  led_set(0, on);
}

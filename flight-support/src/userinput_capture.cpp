
#include "smaccmpilot/userinput_capture.h"

#include <AP_HAL.h>
#include <AP_HAL_SMACCM.h>

#define NUM_CAPTURED_CHANNELS 8

extern const AP_HAL::HAL& hal;

bool userinput_capture(uint16_t *chs) {
    if (hal.rcin->valid()) {
        uint8_t count = hal.rcin->read(chs, NUM_CAPTURED_CHANNELS);
        if (count != NUM_CAPTURED_CHANNELS) {
            return false;
        }
        return true;
    } else {
        return false;
    }
}


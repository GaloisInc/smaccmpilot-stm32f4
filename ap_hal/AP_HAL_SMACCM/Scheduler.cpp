
#include "Scheduler.h"

using namespace SMACCM;

extern const AP_HAL::HAL& hal;

SMACCMScheduler::SMACCMScheduler()
{}

void SMACCMScheduler::init(void* machtnichts)
{}

void SMACCMScheduler::delay(uint32_t ms)
{}

uint32_t SMACCMScheduler::millis() {
    return 10000;
}

uint32_t SMACCMScheduler::micros() {
    return 200000;
}

void SMACCMScheduler::delay_microseconds(uint16_t us)
{}

void SMACCMScheduler::register_delay_callback(AP_HAL::Proc k,
            uint16_t min_time_ms)
{}

void SMACCMScheduler::register_timer_process(AP_HAL::TimedProc k)
{}

bool SMACCMScheduler::defer_timer_process(AP_HAL::TimedProc k) {
    if (k) k(5000);
    return true;
}

void SMACCMScheduler::register_timer_failsafe(AP_HAL::TimedProc,
            uint32_t period_us)
{}

void SMACCMScheduler::suspend_timer_procs()
{}

void SMACCMScheduler::resume_timer_procs()
{}

void SMACCMScheduler::begin_atomic()
{}

void SMACCMScheduler::end_atomic()
{}

void SMACCMScheduler::panic(const prog_char_t *errormsg) {
    hal.console->println_P(errormsg);
    for(;;);
}

void SMACCMScheduler::reboot() {
    for(;;);
}

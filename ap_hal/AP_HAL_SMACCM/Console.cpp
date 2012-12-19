#include <stdarg.h>
#include "Console.h"

using namespace SMACCM;

SMACCMConsoleDriver::SMACCMConsoleDriver(AP_HAL::BetterStream* delegate) :
    _d(delegate)
{}

void SMACCMConsoleDriver::init(void* machtnichts)
{}

void SMACCMConsoleDriver::backend_open()
{}

void SMACCMConsoleDriver::backend_close()
{}

size_t SMACCMConsoleDriver::backend_read(uint8_t *data, size_t len) {
    return 0;
}

size_t SMACCMConsoleDriver::backend_write(const uint8_t *data, size_t len) {
    return 0;
}

void SMACCMConsoleDriver::print_P(const prog_char_t *pstr) {
    _d->print_P(pstr);
}

void SMACCMConsoleDriver::println_P(const prog_char_t *pstr) {
    _d->println_P(pstr);
}

void SMACCMConsoleDriver::printf(const char *pstr, ...) {
    va_list ap;
    va_start(ap, pstr);
    _d->printf(pstr, ap);
    va_end(ap);
}

void SMACCMConsoleDriver::_printf_P(const prog_char *pstr, ...) {
    va_list ap;
    va_start(ap, pstr);
    _d->printf_P(pstr, ap);
    va_end(ap);
}

int16_t SMACCMConsoleDriver::available() {
    return _d->available();
}

int16_t SMACCMConsoleDriver::txspace() {
    return _d->txspace();
}

int16_t SMACCMConsoleDriver::read() {
    return _d->read();
}

int16_t SMACCMConsoleDriver::peek() {
    return _d->peek();
}

size_t SMACCMConsoleDriver::write(uint8_t c) {
    return _d->write(c);
}


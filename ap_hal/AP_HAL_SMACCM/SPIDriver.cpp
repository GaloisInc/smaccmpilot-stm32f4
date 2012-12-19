
#include "SPIDriver.h"

#include <hwf4/spi.h>

using namespace SMACCM;

SMACCMSPIDeviceDriver::SMACCMSPIDeviceDriver()
{}

void SMACCMSPIDeviceDriver::init()
{}

AP_HAL::Semaphore* SMACCMSPIDeviceDriver::get_semaphore()
{
    return &_semaphore;
}

void SMACCMSPIDeviceDriver::transaction(const uint8_t *tx, uint8_t *rx, uint16_t len)
{}


void SMACCMSPIDeviceDriver::cs_assert()
{}

void SMACCMSPIDeviceDriver::cs_release()
{}

uint8_t SMACCMSPIDeviceDriver::transfer (uint8_t data)
{
    return 0;
}

SMACCMSPIDeviceManager::SMACCMSPIDeviceManager()
{}

void SMACCMSPIDeviceManager::init(void *)
{}

AP_HAL::SPIDeviceDriver* SMACCMSPIDeviceManager::device(enum AP_HAL::SPIDevice)
{
    return &_device;
}


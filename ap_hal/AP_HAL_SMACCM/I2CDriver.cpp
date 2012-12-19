
#include <AP_HAL.h>
#include "I2CDriver.h"

using namespace SMACCM;

void SMACCMI2CDriver::begin() {}
void SMACCMI2CDriver::end() {}
void SMACCMI2CDriver::setTimeout(uint16_t ms) {}
void SMACCMI2CDriver::setHighSpeed(bool active) {}

uint8_t SMACCMI2CDriver::write(uint8_t addr, uint8_t len, uint8_t* data)
{return 0;} 
uint8_t SMACCMI2CDriver::writeRegister(uint8_t addr, uint8_t reg, uint8_t val)
{return 0;}
uint8_t SMACCMI2CDriver::writeRegisters(uint8_t addr, uint8_t reg,
                               uint8_t len, uint8_t* data)
{return 0;}

uint8_t SMACCMI2CDriver::read(uint8_t addr, uint8_t len, uint8_t* data)
{return 0;}
uint8_t SMACCMI2CDriver::readRegister(uint8_t addr, uint8_t reg, uint8_t* data)
{return 0;}
uint8_t SMACCMI2CDriver::readRegisters(uint8_t addr, uint8_t reg,
                              uint8_t len, uint8_t* data)
{return 0;}

uint8_t SMACCMI2CDriver::lockup_count() {return 0;}

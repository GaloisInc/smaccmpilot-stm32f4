
#ifndef __AP_HAL_SMACCM_SEMAPHORE_H__
#define __AP_HAL_SMACCM_SEMAPHORE_H__

#include "AP_HAL_SMACCM.h"
#ifdef ECHRONOS
#include <eChronos.h>
#else
#include <FreeRTOS.h>
#include <semphr.h>
#endif

class SMACCM::SMACCMSemaphore : public AP_HAL::Semaphore {
public:
    SMACCMSemaphore();

    void init();
    virtual bool take(uint32_t timeout_ms);
    virtual bool take_nonblocking();
    virtual bool give();

private:
    xSemaphoreHandle m_semaphore;
};

#endif // __AP_HAL_SMACCM_SEMAPHORE_H__

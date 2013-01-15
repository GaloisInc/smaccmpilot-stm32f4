
#ifndef __SMAVLINK_SYSTEM_H__
#define __SMAVLINK_SYSTEM_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

struct smavlink_system {
    uint8_t sysid;
    uint8_t compid;
};

#ifdef __cplusplus
}
#endif

#endif // __SMAVLINK_SYSTEM_H__


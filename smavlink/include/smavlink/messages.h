
#ifndef __SMAVLINK_MESSAGES_H__
#define __SMAVLINK_MESSAGES_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

struct smavlink_heartbeat {
    uint8_t hb_type;
    uint8_t hb_autopilot;
    uint8_t hb_base_mode;
    uint32_t hb_custom_mode;
    uint8_t hb_system_status;
};


#ifdef __cplusplus
}
#endif

#endif // __SMAVLINK_MESSAGES_H__


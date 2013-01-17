#ifndef __SMAVLINK_MESSAGE_MANUAL_SETPOINT_H__
#define __SMAVLINK_MESSAGE_MANUAL_SETPOINT_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct manual_setpoint_msg {
    uint32_t time_boot_ms;
    float roll;
    float pitch;
    float yaw;
    float thrust;
    uint8_t mode_switch;
    uint8_t manual_override_switch;
};
void smavlink_send_manual_setpoint(struct manual_setpoint_msg* var0,
                                   struct smavlink_out_channel* var1,
                                   struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_MANUAL_SETPOINT_H__ */
#ifndef __SMAVLINK_MESSAGE_SET_LOCAL_POSITION_SETPOINT_H__
#define __SMAVLINK_MESSAGE_SET_LOCAL_POSITION_SETPOINT_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct set_local_position_setpoint_msg {
    float x;
    float y;
    float z;
    float yaw;
    uint8_t target_system;
    uint8_t target_component;
    uint8_t coordinate_frame;
};
void smavlink_send_set_local_position_setpoint(struct set_local_position_setpoint_msg* var0,
                                               struct smavlink_out_channel* var1,
                                               struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_SET_LOCAL_POSITION_SETPOINT_H__ */
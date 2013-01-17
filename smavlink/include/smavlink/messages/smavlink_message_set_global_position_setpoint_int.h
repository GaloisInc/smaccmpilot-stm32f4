#ifndef __SMAVLINK_MESSAGE_SET_GLOBAL_POSITION_SETPOINT_INT_H__
#define __SMAVLINK_MESSAGE_SET_GLOBAL_POSITION_SETPOINT_INT_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct set_global_position_setpoint_int_msg {
    int32_t latitude;
    int32_t longitude;
    int32_t altitude;
    int16_t yaw;
    uint8_t coordinate_frame;
};
void smavlink_send_set_global_position_setpoint_int(struct set_global_position_setpoint_int_msg* var0,
                                                    struct smavlink_out_channel* var1,
                                                    struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_SET_GLOBAL_POSITION_SETPOINT_INT_H__ */
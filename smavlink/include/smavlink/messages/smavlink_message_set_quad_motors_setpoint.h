#ifndef __SMAVLINK_MESSAGE_SET_QUAD_MOTORS_SETPOINT_H__
#define __SMAVLINK_MESSAGE_SET_QUAD_MOTORS_SETPOINT_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct set_quad_motors_setpoint_msg {
    uint16_t motor_front_nw;
    uint16_t motor_right_ne;
    uint16_t motor_back_se;
    uint16_t motor_left_sw;
    uint8_t target_system;
};
void smavlink_send_set_quad_motors_setpoint(struct set_quad_motors_setpoint_msg* var0,
                                            struct smavlink_out_channel* var1,
                                            struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_SET_QUAD_MOTORS_SETPOINT_H__ */
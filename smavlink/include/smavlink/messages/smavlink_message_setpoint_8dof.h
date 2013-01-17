#ifndef __SMAVLINK_MESSAGE_SETPOINT_8DOF_H__
#define __SMAVLINK_MESSAGE_SETPOINT_8DOF_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct setpoint_8dof_msg {
    float val1;
    float val2;
    float val3;
    float val4;
    float val5;
    float val6;
    float val7;
    float val8;
    uint8_t target_system;
};
void smavlink_send_setpoint_8dof(struct setpoint_8dof_msg* var0,
                                 struct smavlink_out_channel* var1,
                                 struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_SETPOINT_8DOF_H__ */
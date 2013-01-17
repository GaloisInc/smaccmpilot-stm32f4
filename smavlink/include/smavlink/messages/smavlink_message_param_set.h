#ifndef __SMAVLINK_MESSAGE_PARAM_SET_H__
#define __SMAVLINK_MESSAGE_PARAM_SET_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct param_set_msg {
    float param_value;
    uint8_t target_system;
    uint8_t target_component;
    uint8_t param_type;
    uint8_t param_id[16U];
};
void smavlink_send_param_set(struct param_set_msg* var0,
                             struct smavlink_out_channel* var1,
                             struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_PARAM_SET_H__ */
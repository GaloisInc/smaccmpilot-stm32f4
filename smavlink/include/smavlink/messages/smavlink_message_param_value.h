#ifndef __SMAVLINK_MESSAGE_PARAM_VALUE_H__
#define __SMAVLINK_MESSAGE_PARAM_VALUE_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct param_value_msg {
    float param_value;
    uint16_t param_count;
    uint16_t param_index;
    uint8_t param_type;
    uint8_t param_id[16U];
};
void smavlink_send_param_value(struct param_value_msg* var0,
                               struct smavlink_out_channel* var1,
                               struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_PARAM_VALUE_H__ */
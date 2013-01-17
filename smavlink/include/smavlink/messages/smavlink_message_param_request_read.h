#ifndef __SMAVLINK_MESSAGE_PARAM_REQUEST_READ_H__
#define __SMAVLINK_MESSAGE_PARAM_REQUEST_READ_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct param_request_read_msg {
    int16_t param_index;
    uint8_t target_system;
    uint8_t target_component;
    uint8_t param_id[16U];
};
void smavlink_send_param_request_read(struct param_request_read_msg* var0,
                                      struct smavlink_out_channel* var1,
                                      struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_PARAM_REQUEST_READ_H__ */
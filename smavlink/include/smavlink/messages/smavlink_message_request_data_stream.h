#ifndef __SMAVLINK_MESSAGE_REQUEST_DATA_STREAM_H__
#define __SMAVLINK_MESSAGE_REQUEST_DATA_STREAM_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct request_data_stream_msg {
    uint16_t req_message_rate;
    uint8_t target_system;
    uint8_t target_component;
    uint8_t req_stream_id;
    uint8_t start_stop;
};
void smavlink_send_request_data_stream(struct request_data_stream_msg* var0,
                                       struct smavlink_out_channel* var1,
                                       struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_REQUEST_DATA_STREAM_H__ */
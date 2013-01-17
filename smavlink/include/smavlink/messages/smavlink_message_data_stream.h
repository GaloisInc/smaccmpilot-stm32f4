#ifndef __SMAVLINK_MESSAGE_DATA_STREAM_H__
#define __SMAVLINK_MESSAGE_DATA_STREAM_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct data_stream_msg {
    uint16_t message_rate;
    uint8_t stream_id;
    uint8_t on_off;
};
void smavlink_send_data_stream(struct data_stream_msg* var0,
                               struct smavlink_out_channel* var1,
                               struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_DATA_STREAM_H__ */
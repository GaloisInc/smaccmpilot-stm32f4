#ifndef __SMAVLINK_MESSAGE_OPTICAL_FLOW_H__
#define __SMAVLINK_MESSAGE_OPTICAL_FLOW_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct optical_flow_msg {
    uint64_t time_usec;
    float flow_comp_m_x;
    float flow_comp_m_y;
    float ground_distance;
    int16_t flow_x;
    int16_t flow_y;
    uint8_t sensor_id;
    uint8_t quality;
};
void smavlink_send_optical_flow(struct optical_flow_msg* var0,
                                struct smavlink_out_channel* var1,
                                struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_OPTICAL_FLOW_H__ */
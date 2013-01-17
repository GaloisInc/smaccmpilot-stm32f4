#ifndef __SMAVLINK_MESSAGE_GLOBAL_POSITION_INT_H__
#define __SMAVLINK_MESSAGE_GLOBAL_POSITION_INT_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct global_position_int_msg {
    uint32_t time_boot_ms;
    int32_t lat;
    int32_t lon;
    int32_t alt;
    int32_t relative_alt;
    int16_t vx;
    int16_t vy;
    int16_t vz;
    uint16_t hdg;
};
void smavlink_send_global_position_int(struct global_position_int_msg* var0,
                                       struct smavlink_out_channel* var1,
                                       struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_GLOBAL_POSITION_INT_H__ */
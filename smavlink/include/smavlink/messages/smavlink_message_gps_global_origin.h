#ifndef __SMAVLINK_MESSAGE_GPS_GLOBAL_ORIGIN_H__
#define __SMAVLINK_MESSAGE_GPS_GLOBAL_ORIGIN_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct gps_global_origin_msg {
    int32_t latitude;
    int32_t longitude;
    int32_t altitude;
};
void smavlink_send_gps_global_origin(struct gps_global_origin_msg* var0,
                                     struct smavlink_out_channel* var1,
                                     struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_GPS_GLOBAL_ORIGIN_H__ */
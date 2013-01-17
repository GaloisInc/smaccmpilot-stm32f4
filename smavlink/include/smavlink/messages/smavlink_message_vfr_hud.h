#ifndef __SMAVLINK_MESSAGE_VFR_HUD_H__
#define __SMAVLINK_MESSAGE_VFR_HUD_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct vfr_hud_msg {
    float airspeed;
    float groundspeed;
    float alt;
    float climb;
    int16_t heading;
    uint16_t throttle;
};
void smavlink_send_vfr_hud(struct vfr_hud_msg* var0,
                           struct smavlink_out_channel* var1,
                           struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_VFR_HUD_H__ */
#ifndef __SMAVLINK_MESSAGE_STATE_CORRECTION_H__
#define __SMAVLINK_MESSAGE_STATE_CORRECTION_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
#include <smavlink/channel.h>
#include <smavlink/system.h>
struct state_correction_msg {
    float xErr;
    float yErr;
    float zErr;
    float rollErr;
    float pitchErr;
    float yawErr;
    float vxErr;
    float vyErr;
    float vzErr;
};
void smavlink_send_state_correction(struct state_correction_msg* var0,
                                    struct smavlink_out_channel* var1,
                                    struct smavlink_system* var2);

#ifdef __cplusplus
}
#endif
#endif /* __SMAVLINK_MESSAGE_STATE_CORRECTION_H__ */
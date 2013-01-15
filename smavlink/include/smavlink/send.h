
#ifndef __SMAVLINK_SEND_H__
#define __SMAVLINK_SEND_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "smavlink/channel.h"
#include "smavlink/system.h"
#include "smavlink/messages.h"

void smavlink_send_heartbeat(struct smavlink_heartbeat *msg,
        struct smavlink_out_channel *ch,
        struct smavlink_system *sys);

#ifdef __cplusplus
}
#endif

#endif // __SMAVLINK_SEND_H__

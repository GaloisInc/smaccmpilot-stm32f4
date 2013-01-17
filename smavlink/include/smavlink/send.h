
#ifndef __SMAVLINK_SEND_H__
#define __SMAVLINK_SEND_H__

#ifdef __cplusplus
extern "C" {
#endif

#include "smavlink/channel.h"
#include "smavlink/system.h"

#define smavlink_send_ivory(ch, sys, msgid, bodyref, bodylen, crc) \
    smavlink_send(ch, sys, msgid, (const uint8_t*)(*bodyref), bodylen, crc)

void smavlink_send( struct smavlink_out_channel *ch,
        struct smavlink_system *sys,
        uint8_t msgid,
        const uint8_t *body,
        size_t bodylen,
        int8_t crc_extra );

#ifdef __cplusplus
}
#endif

#endif // __SMAVLINK_SEND_H__

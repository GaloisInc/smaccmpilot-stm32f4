
#include "smavlink/send.h"
#include "pack.h"
#include "crc.h"

/* magic number */
#define MAVLINK_STX 254

#define MAVLINK_CORE_HEADER_LEN 5
#define MAVLINK_NUM_HEADER_BYTES (MAVLINK_CORE_HEADER_LEN + 1)
#define MAVLINK_NUM_CHECKSUM_BYTES 2
#define MAVLINK_NUM_NON_PAYLOAD_BYTES (MAVLINK_NUM_HEADER_BYTES + \
                                       MAVLINK_NUM_CHECKSUM_BYTES)


/* Steal these definitions from the generated c mavlink implementation. You'll
 * have to dig around for the call to mavlink_finalize_message, where the last
 * parameter is CRCEXTRA.
 */
#define MAVLINK_MESSAGE_ID_HEARTBEAT 0
#define MAVLINK_MESSAGE_ID_HEARTBEAT_LEN 9
#define MAVLINK_MESSAGE_ID_HEARTBEAT_CRCEXTRA 50

static void smavlink_send( struct smavlink_out_channel *ch,
        struct smavlink_system *sys,
        uint8_t msgid,
        const uint8_t *body,
        size_t bodylen,
        int8_t crc_extra )
{
    uint8_t head[MAVLINK_NUM_HEADER_BYTES];
    head[0] = MAVLINK_STX;
    head[1] = (uint8_t) bodylen;
    head[2] = ch->tx_seq++;
    head[3] = sys->sysid;
    head[4] = sys->compid;
    head[5] = msgid;
    
    uint16_t crc = crc_calculate(&head[1], MAVLINK_CORE_HEADER_LEN);
    crc_accumulate_buffer(body, bodylen, &crc);
    crc_accumulate(crc_extra, &crc);

    uint8_t ck[MAVLINK_NUM_CHECKSUM_BYTES];
    ck[0] = (uint8_t)(crc & 0xff);
    ck[1] = (uint8_t)(crc >> 8);

    size_t total_len = MAVLINK_NUM_NON_PAYLOAD_BYTES + bodylen;
    if (smavlink_channel_begin_atomic(ch, total_len)) {
        smavlink_channel_write(ch, head, MAVLINK_NUM_HEADER_BYTES);
        smavlink_channel_write(ch, body, bodylen);
        smavlink_channel_write(ch, ck, MAVLINK_NUM_CHECKSUM_BYTES);
        smavlink_channel_end_atomic(ch);
    }
}


void smavlink_send_heartbeat(struct smavlink_heartbeat *msg,
        struct smavlink_out_channel *ch,
        struct smavlink_system *sys) 
{
    uint8_t buf[MAVLINK_MESSAGE_ID_HEARTBEAT_LEN];
    const uint8_t three = 3; /* needed for last value, no idea why */
    smavlink_pack_uint32_t(buf, 0, msg->hb_custom_mode);
    smavlink_pack_uint8_t(buf,  4, msg->hb_type);
    smavlink_pack_uint8_t(buf,  5, msg->hb_autopilot);
    smavlink_pack_uint8_t(buf,  6, msg->hb_base_mode);
    smavlink_pack_uint8_t(buf,  7, msg->hb_system_status);
    smavlink_pack_uint8_t(buf,  8, three);

    smavlink_send(ch, sys, MAVLINK_MESSAGE_ID_HEARTBEAT, buf,
            MAVLINK_MESSAGE_ID_HEARTBEAT_LEN,
            MAVLINK_MESSAGE_ID_HEARTBEAT_CRCEXTRA);
}

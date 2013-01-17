
#include "smavlink/send.h"
#include "crc.h"

/* magic number */
#define MAVLINK_STX 254

#define MAVLINK_CORE_HEADER_LEN 5
#define MAVLINK_NUM_HEADER_BYTES (MAVLINK_CORE_HEADER_LEN + 1)
#define MAVLINK_NUM_CHECKSUM_BYTES 2
#define MAVLINK_NUM_NON_PAYLOAD_BYTES (MAVLINK_NUM_HEADER_BYTES + \
                                       MAVLINK_NUM_CHECKSUM_BYTES)

void smavlink_send( struct smavlink_out_channel *ch,
        struct smavlink_system *sys,
        uint8_t msgid,
        const uint8_t *body,
        size_t bodylen,
        int8_t crc_extra )
{

    /* Allocate header on the stack */
    uint8_t head[MAVLINK_NUM_HEADER_BYTES];
    head[0] = MAVLINK_STX;           /* Magic start byte */
    head[1] = (uint8_t) bodylen;     /* bodylen should always be <255 */
    head[2] = ch->tx_seq++;
    head[3] = sys->sysid;
    head[4] = sys->compid;
    head[5] = msgid;
   
    /* Calculate CRC over head, body, and finally crc_extra */
    uint16_t crc = crc_calculate(&head[1], MAVLINK_CORE_HEADER_LEN);
    crc_accumulate_buffer(body, bodylen, &crc);
    crc_accumulate(crc_extra, &crc);

    /* Allocate checksum on the stack */
    uint8_t ck[MAVLINK_NUM_CHECKSUM_BYTES];
    ck[0] = (uint8_t)(crc & 0xff);
    ck[1] = (uint8_t)(crc >> 8);

    size_t total_len = MAVLINK_NUM_NON_PAYLOAD_BYTES + bodylen;
    /* */
    if (smavlink_channel_begin_atomic(ch, total_len)) {
        smavlink_channel_write(ch, head, MAVLINK_NUM_HEADER_BYTES);
        smavlink_channel_write(ch, body, bodylen);
        smavlink_channel_write(ch, ck, MAVLINK_NUM_CHECKSUM_BYTES);
        smavlink_channel_end_atomic(ch);
    }
}


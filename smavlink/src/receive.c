
#include "smavlink/receive.h"
#include "protocol.h"
#include "crc.h"

/* Helper function to read from a channel, expecting exactly "len"
 * bytes.  Returns false on failure. */
static bool smavlink_channel_read(struct smavlink_in_channel *ch,
                                  uint8_t *buf, size_t len)
{
    if (smavlink_in_channel_read(ch, buf, len) != len)
        return false;
    return true;
}

bool smavlink_receive(struct smavlink_in_channel *ch,
                      struct smavlink_system *sys,
                      uint8_t *msgid_out,
                      uint8_t *buf, size_t len)
{
    uint8_t header[MAVLINK_NUM_HEADER_BYTES];
    uint8_t crc[MAVLINK_NUM_CHECKSUM_BYTES];
    uint16_t crc_calc;

    crc_init(&crc_calc);

    /* Read single bytes until we receive a start marker. */
    do {
        if (!smavlink_channel_read(ch, &header[0], 1))
            return false;
    } while (header[0] != MAVLINK_STX);

    /* Read the remaining header fields. */
    if (!smavlink_channel_read(ch, &header[1], sizeof(header) - 1))
        return false;

    crc_accumulate_buffer(&header[1], sizeof(header) - 1, &crc_calc);

    /* Validate the header fields.
     *
     * TODO: Do something with the sequence number to detect
     * dropped messages? */
    if (header[1] > len)        /* message too large */
        return false;

    /* Read the message body into caller's buffer. */
    if (!smavlink_channel_read(ch, buf, header[1]))
        return false;

    crc_accumulate_buffer(buf, header[1], &crc_calc);
    /* TODO: Need to accumulate CRC_EXTRA here. */

    /* Read the CRC and compare it to the calculated CRC.
     *
     * TODO: Implement actually checking the CRC.  This is harder than
     * it looks because we have to know the CRC_EXTRA byte based on
     * the message ID.  Bleh. */
    if (!smavlink_channel_read(ch, crc, sizeof(crc)))
        return false;

    *msgid_out = header[5];
    return true;
}

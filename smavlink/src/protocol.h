
#ifndef __SMAVLINK_PROTOCOL_H__
#define __SMAVLINK_PROTOCOL_H__

#ifdef __cplusplus
extern "C" {
#endif

/* magic number */
#define MAVLINK_STX 254

#define MAVLINK_CORE_HEADER_LEN 5
#define MAVLINK_NUM_HEADER_BYTES (MAVLINK_CORE_HEADER_LEN + 1)
#define MAVLINK_NUM_CHECKSUM_BYTES 2
#define MAVLINK_NUM_NON_PAYLOAD_BYTES (MAVLINK_NUM_HEADER_BYTES + \
                                       MAVLINK_NUM_CHECKSUM_BYTES)

#ifdef __cplusplus
}
#endif

#endif // __SMAVLINK_PROTOCOL_H__

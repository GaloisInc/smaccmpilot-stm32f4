
#ifndef __SMAVLINK_CRC_H__
#define __SMAVLINK_CRC_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stddef.h>

void crc_init(uint16_t *crc);
void crc_accumulate(uint8_t byte, uint16_t *crc);
void crc_accumulate_buffer(const uint8_t *buf, size_t len,
        uint16_t *crc);
uint16_t crc_calculate(const uint8_t *buf, size_t len);

#ifdef __cplusplus
}
#endif

#endif // __SMAVLINK_CRC_H__


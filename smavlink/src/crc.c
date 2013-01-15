
#include "crc.h"

void crc_init(uint16_t *p_crc) {
    *p_crc = 0xFFFF; /* X25_INIT_CRC */
}

void crc_accumulate(uint8_t data, uint16_t *p_crc) {
    const uint16_t crc = *p_crc;
    const uint8_t  crclo = (uint8_t) (crc & 0xFF);
    const uint8_t  v0 = data ^ crclo;
    const uint8_t  v1 = v0 << 4;
    const uint8_t  v2 = v0 ^ v1;
    const uint16_t v3 = (v2 << 8);
    const uint16_t v4 = (v2 << 3);
    const uint16_t v5 = (v2 >> 4);
    const uint16_t v6 = (crc >> 8);
    const uint16_t v7 = v6 ^ v3 ^ v4 ^ v5;
    *p_crc = v7;
}

void crc_accumulate_buffer(const uint8_t *buf, size_t len,
        uint16_t *crc)
{
    const uint8_t *p = buf;
    while (len--) crc_accumulate(*p++, crc);
}

uint16_t crc_calculate(const uint8_t *buf, size_t len) {
    uint16_t crc;
    const uint8_t *p = buf;
    crc_init(&crc);
    while(len--) crc_accumulate(*p++, &crc);
    return crc;
}


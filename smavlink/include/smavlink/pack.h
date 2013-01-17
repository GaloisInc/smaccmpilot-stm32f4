
#ifndef __SMAVLINK_PACK_H__
#define __SMAVLINK_PACK_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
/* We don't need send.h for this but ivory deps are weird and i need it to be
 * included by the message c files, which only include pack.h at the moment */
#include <smavlink/send.h>

static inline void smavlink_pack_swap2(uint8_t *dst, const uint8_t *src) {
    dst[0] = src[1];
    dst[1] = src[0];
}

static inline void smavlink_pack_swap4(uint8_t *dst, const uint8_t *src) {
    dst[0] = src[3];
    dst[1] = src[2];
    dst[2] = src[1];
    dst[3] = src[0];
}

static inline void smavlink_pack_swap8(uint8_t *dst, const uint8_t *src) {
    dst[0] = src[7];
    dst[1] = src[6];
    dst[2] = src[5];
    dst[3] = src[4];
    dst[4] = src[3];
    dst[5] = src[2];
    dst[6] = src[1];
    dst[7] = src[0];
}

static inline void smavlink_pack_copy2(uint8_t *dst, const uint8_t *src) {
    dst[0] = src[0];
    dst[1] = src[1];
}

static inline void smavlink_pack_copy4(uint8_t *dst, const uint8_t *src) {
    dst[0] = src[0];
    dst[1] = src[1];
    dst[2] = src[2];
    dst[3] = src[3];
}

static inline void smavlink_pack_copy8(uint8_t *dst, const uint8_t *src) {
    dst[0] = src[0];
    dst[1] = src[1];
    dst[2] = src[2];
    dst[3] = src[3];
    dst[4] = src[4];
    dst[5] = src[5];
    dst[6] = src[6];
    dst[7] = src[7];
}

static inline void smavlink_pack_copy1(uint8_t *dst, const uint8_t *src) {
    dst[0] = src[0];
}

/** smavlink_pack_$type functions: these take a uint8_t *[] type and turn them
 * to a uint8_t [] type. Ivory forces us to pass arrays by reference rather
 * than value. */

/* 1 byte values are always just copied...*/
#define smavlink_pack_uint8_t(buf, offs, b) smavlink_pack_copy1(&(*buf)[offs], (const uint8_t *)&b)
#define smavlink_pack_int8_t(buf, offs, b) smavlink_pack_copy1(&(*buf)[offs], (const uint8_t *)&b)

#if SMAVLINK_PACK_BYTE_SWAP
#define smavlink_pack_uint16_t(buf, offs, b) smavlink_pack_swap2(&(*buf)[offs], (const uint8_t *)&b)
#define smavlink_pack_int16_t(buf, offs, b)  smavlink_pack_swap2(&(*buf)[offs], (const uint8_t *)&b)
#define smavlink_pack_uint32_t(buf, offs, b) smavlink_pack_swap4(&(*buf)[offs], (const uint8_t *)&b)
#define smavlink_pack_int32_t(buf, offs, b)  smavlink_pack_swap4(&(*buf)[offs], (const uint8_t *)&b)
#define smavlink_pack_uint64_t(buf, offs, b) smavlink_pack_swap8(&(*buf)[offs], (const uint8_t *)&b)
#define smavlink_pack_int64_t(buf, offs, b)  smavlink_pack_swap8(&(*buf)[offs], (const uint8_t *)&b)
#define smavlink_pack_float(buf, offs, b)    smavlink_pack_swap4(&(*buf)[offs], (const uint8_t *)&b)
#define smavlink_pack_double(buf, offs, b)   smavlink_pack_swap8(&(*buf)[offs], (const uint8_t *)&b)
#else 
#define smavlink_pack_uint16_t(buf, offs, b) smavlink_pack_copy2(&(*buf)[offs], (const uint8_t *)&b)
#define smavlink_pack_int16_t(buf, offs, b)  smavlink_pack_copy2(&(*buf)[offs], (const uint8_t *)&b)
#define smavlink_pack_uint32_t(buf, offs, b) smavlink_pack_copy4(&(*buf)[offs], (const uint8_t *)&b)
#define smavlink_pack_int32_t(buf, offs, b)  smavlink_pack_copy4(&(*buf)[offs], (const uint8_t *)&b)
#define smavlink_pack_uint64_t(buf, offs, b) smavlink_pack_copy8(&(*buf)[offs], (const uint8_t *)&b)
#define smavlink_pack_int64_t(buf, offs, b)  smavlink_pack_copy8(&(*buf)[offs], (const uint8_t *)&b)
#define smavlink_pack_float(buf, offs, b)    smavlink_pack_copy4(&(*buf)[offs], (const uint8_t *)&b)
#define smavlink_pack_double(buf, offs, b)   smavlink_pack_copy8(&(*buf)[offs], (const uint8_t *)&b)
#endif

#ifdef __cplusplus
}
#endif

#endif // __SMAVLINK_PACK_H__


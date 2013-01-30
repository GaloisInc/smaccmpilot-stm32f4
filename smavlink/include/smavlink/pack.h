
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
#define smavlink_pack_uint8_t(buf, offs, b) smavlink_pack_copy1(((buf) + (offs)), (const uint8_t *)&b)
#define smavlink_pack_int8_t(buf, offs, b) smavlink_pack_copy1(((buf) + (offs)), (const uint8_t *)&b)

#if SMAVLINK_PACK_BYTE_SWAP
#define smavlink_pack_uint16_t(buf, offs, b) smavlink_pack_swap2(((buf) + (offs)), (const uint8_t *)&b)
#define smavlink_pack_int16_t(buf, offs, b)  smavlink_pack_swap2(((buf) + (offs)), (const uint8_t *)&b)
#define smavlink_pack_uint32_t(buf, offs, b) smavlink_pack_swap4(((buf) + (offs)), (const uint8_t *)&b)
#define smavlink_pack_int32_t(buf, offs, b)  smavlink_pack_swap4(((buf) + (offs)), (const uint8_t *)&b)
#define smavlink_pack_uint64_t(buf, offs, b) smavlink_pack_swap8(((buf) + (offs)), (const uint8_t *)&b)
#define smavlink_pack_int64_t(buf, offs, b)  smavlink_pack_swap8(((buf) + (offs)), (const uint8_t *)&b)
#define smavlink_pack_float(buf, offs, b)    smavlink_pack_swap4(((buf) + (offs)), (const uint8_t *)&b)
#define smavlink_pack_double(buf, offs, b)   smavlink_pack_swap8(((buf) + (offs)), (const uint8_t *)&b)
#else 
#define smavlink_pack_uint16_t(buf, offs, b) smavlink_pack_copy2(((buf) + (offs)), (const uint8_t *)&b)
#define smavlink_pack_int16_t(buf, offs, b)  smavlink_pack_copy2(((buf) + (offs)), (const uint8_t *)&b)
#define smavlink_pack_uint32_t(buf, offs, b) smavlink_pack_copy4(((buf) + (offs)), (const uint8_t *)&b)
#define smavlink_pack_int32_t(buf, offs, b)  smavlink_pack_copy4(((buf) + (offs)), (const uint8_t *)&b)
#define smavlink_pack_uint64_t(buf, offs, b) smavlink_pack_copy8(((buf) + (offs)), (const uint8_t *)&b)
#define smavlink_pack_int64_t(buf, offs, b)  smavlink_pack_copy8(((buf) + (offs)), (const uint8_t *)&b)
#define smavlink_pack_float(buf, offs, b)    smavlink_pack_copy4(((buf) + (offs)), (const uint8_t *)&b)
#define smavlink_pack_double(buf, offs, b)   smavlink_pack_copy8(((buf) + (offs)), (const uint8_t *)&b)
#endif

/*
 * Unpacking
 *
 * MAVLink passes multi-byte fields in little-endian byte order, so
 * the non-swapping versions of unpack decode the LSB-first.
 */

static inline uint8_t smavlink_unpack_copy1(const uint8_t *src) {
    return *src;
}

static inline uint16_t smavlink_unpack_copy2(const uint8_t *src) {
    return (((uint16_t)src[0] << 0 ) |
            ((uint16_t)src[1] << 8 ));
}

static inline uint32_t smavlink_unpack_copy4(const uint8_t *src) {
    return (((uint32_t)src[0] << 0 ) |
            ((uint32_t)src[1] << 8 ) |
            ((uint16_t)src[2] << 16) |
            ((uint16_t)src[3] << 24));
}

static inline uint64_t smavlink_unpack_copy8(const uint8_t *src) {
    return (((uint64_t)src[0] << 0 ) |
            ((uint64_t)src[1] << 8 ) |
            ((uint64_t)src[2] << 16) |
            ((uint64_t)src[3] << 24) |
            ((uint64_t)src[4] << 32) |
            ((uint64_t)src[5] << 40) |
            ((uint64_t)src[6] << 48) |
            ((uint64_t)src[7] << 56));
}


static inline uint16_t smavlink_unpack_swap2(const uint8_t *src) {
    return (((uint16_t)src[1] << 0 ) |
            ((uint16_t)src[0] << 8 ));
}

static inline uint32_t smavlink_unpack_swap4(const uint8_t *src) {
    return (((uint32_t)src[3] << 0 ) |
            ((uint32_t)src[2] << 8 ) |
            ((uint16_t)src[1] << 16) |
            ((uint16_t)src[0] << 24));
}

static inline uint64_t smavlink_unpack_swap8(const uint8_t *src) {
    return (((uint64_t)src[7] << 0 ) |
            ((uint64_t)src[6] << 8 ) |
            ((uint64_t)src[5] << 16) |
            ((uint64_t)src[4] << 24) |
            ((uint64_t)src[3] << 32) |
            ((uint64_t)src[2] << 40) |
            ((uint64_t)src[1] << 48) |
            ((uint64_t)src[0] << 56));
}

#define smavlink_unpack_uint8_t(buf, offs) ((uint8_t)smavlink_unpack_copy1(&(*buf)[offs]))
#define smavlink_unpack_int8_t(buf, offs)  ((int8_t)smavlink_unpack_copy1(&(*buf)[offs]))

#if SMAVLINK_PACK_BYTE_SWAP
#define smavlink_unpack_uint16_t(buf, offs) ((uint16_t)smavlink_unpack_swap2(&(*buf)[offs]))
#define smavlink_unpack_int16_t(buf, offs)  ((int16_t)smavlink_unpack_swap2(&(*buf)[offs]))
#define smavlink_unpack_uint32_t(buf, offs) ((uint32_t)smavlink_unpack_swap4(&(*buf)[offs]))
#define smavlink_unpack_int32_t(buf, offs)  ((int32_t)smavlink_unpack_swap4(&(*buf)[offs]))
#define smavlink_unpack_uint64_t(buf, offs) ((uint64_t)smavlink_unpack_swap8(&(*buf)[offs]))
#define smavlink_unpack_int64_t(buf, offs)  ((int64_t)smavlink_unpack_swap8(&(*buf)[offs]))

static inline float smavlink_unpack_float(uint8_t *buf[], uint8_t offset) {
    uint32_t raw = smavlink_unpack_swap4(&(*buf)[offset]);
    return *(float *)(&raw);
}

static inline double smavlink_unpack_double(uint8_t *buf[], uint8_t offset) {
    uint64_t raw = smavlink_unpack_swap8(&(*buf)[offset]);
    return *(double *)(&raw);
}

#else
#define smavlink_unpack_uint16_t(buf, offs) ((uint16_t)smavlink_unpack_copy2(&(*buf)[offs]))
#define smavlink_unpack_int16_t(buf, offs)  ((int16_t)smavlink_unpack_copy2(&(*buf)[offs]))
#define smavlink_unpack_uint32_t(buf, offs) ((uint32_t)smavlink_unpack_copy4(&(*buf)[offs]))
#define smavlink_unpack_int32_t(buf, offs)  ((int32_t)smavlink_unpack_copy4(&(*buf)[offs]))
#define smavlink_unpack_uint64_t(buf, offs) ((uint64_t)smavlink_unpack_copy8(&(*buf)[offs]))
#define smavlink_unpack_int64_t(buf, offs)  ((int64_t)smavlink_unpack_copy8(&(*buf)[offs]))

static inline float smavlink_unpack_float(const uint8_t *buf[], uint8_t offset) {
    uint32_t raw = smavlink_unpack_copy4(&(*buf)[offset]);
    return *(float *)(&raw);
}

static inline double smavlink_unpack_double(const uint8_t *buf[], uint8_t offset) {
    uint64_t raw = smavlink_unpack_copy8(&(*buf)[offset]);
    return *(double *)(&raw);
}

#endif

#ifdef __cplusplus
}
#endif

#endif // __SMAVLINK_PACK_H__



#ifndef __IVORY_SERIALIZE_PRIM_H__
#define __IVORY_SERIALIZE_PRIM_H__

#include <stdint.h>

// This code depends on pointer alignment tricks
#pragma GCC diagnostic ignored "-Wstrict-aliasing"

// Packing primitives:

static inline void ivory_serialize_pack_prim_1(uint8_t *dst, const uint8_t *src) {
	dst[0] = src[0];
}

static inline void ivory_serialize_pack_prim_2(uint8_t *dst, const uint8_t *src) {
#ifndef CONFIG_IVORY_SERIALIZE_ENDIAN_SWAP
	dst[0] = src[0];
	dst[1] = src[1];
#else
	dst[0] = src[1];
	dst[1] = src[0];
#endif
}

static inline void ivory_serialize_pack_prim_4(uint8_t *dst, const uint8_t *src) {
#ifndef CONFIG_IVORY_SERIALIZE_ENDIAN_SWAP
	dst[0] = src[0];
	dst[1] = src[1];
	dst[2] = src[2];
	dst[3] = src[3];
#else
	dst[0] = src[3];
	dst[1] = src[2];
	dst[2] = src[1];
	dst[3] = src[0];
#endif
}

static inline void ivory_serialize_pack_prim_8(uint8_t *dst, const uint8_t *src) {
#ifndef CONFIG_IVORY_SERIALIZE_ENDIAN_SWAP
	dst[0] = src[0];
	dst[1] = src[1];
	dst[2] = src[2];
	dst[3] = src[3];
	dst[4] = src[4];
	dst[5] = src[5];
	dst[6] = src[6];
	dst[7] = src[7];
#else
	dst[0] = src[7];
	dst[1] = src[6];
	dst[2] = src[5];
	dst[3] = src[4];
	dst[4] = src[3];
	dst[5] = src[2];
	dst[6] = src[1];
	dst[7] = src[0];
#endif
}

// Macros to cast any source type to a uint8_t*.
#define ivory_serialize_pack_1(dst, offs, src) ivory_serialize_pack_prim_1((dst) + (offs), (const uint8_t *)(&src))
#define ivory_serialize_pack_2(dst, offs, src) ivory_serialize_pack_prim_2((dst) + (offs), (const uint8_t *)(&src))
#define ivory_serialize_pack_4(dst, offs, src) ivory_serialize_pack_prim_4((dst) + (offs), (const uint8_t *)(&src))
#define ivory_serialize_pack_8(dst, offs, src) ivory_serialize_pack_prim_8((dst) + (offs), (const uint8_t *)(&src))

// Unpacking primitives:

static inline uint8_t ivory_serialize_unpack_prim_1(const uint8_t *src){
	return ((uint8_t) src[0] << 0);
}

static inline uint16_t ivory_serialize_unpack_prim_2(const uint8_t *src){
#ifndef CONFIG_IVORY_SERIALIZE_ENDIAN_SWAP
	return (((uint16_t) src[0] << 0) |
			((uint16_t) src[1] << 8));
#else
	return (((uint16_t) src[1] << 0) |
			((uint16_t) src[0] << 8));
#endif
}

static inline uint32_t ivory_serialize_unpack_prim_4(const uint8_t *src){
#ifndef CONFIG_IVORY_SERIALIZE_ENDIAN_SWAP
	return (((uint32_t) src[0] << 0)  |
			((uint32_t) src[1] << 8)  |
			((uint32_t) src[2] << 16) |
			((uint32_t) src[3] << 24));
#else
	return (((uint32_t) src[3] << 0)  |
			((uint32_t) src[2] << 8)  |
			((uint32_t) src[1] << 16) |
			((uint32_t) src[0] << 24));
#endif
}

static inline uint64_t ivory_serialize_unpack_prim_8(const uint8_t *src){
#ifndef CONFIG_IVORY_SERIALIZE_ENDIAN_SWAP
	return (((uint64_t) src[0] << 0)  |
			((uint64_t) src[1] << 8)  |
			((uint64_t) src[2] << 16) |
			((uint64_t) src[3] << 24) |
			((uint64_t) src[4] << 32) |
			((uint64_t) src[5] << 40) |
			((uint64_t) src[6] << 48) |
			((uint64_t) src[7] << 56));
#else
	return (((uint64_t) src[7] << 0)  |
			((uint64_t) src[6] << 8)  |
			((uint64_t) src[5] << 16) |
			((uint64_t) src[4] << 24) |
			((uint64_t) src[3] << 32) |
			((uint64_t) src[2] << 40) |
			((uint64_t) src[1] << 48) |
			((uint64_t) src[0] << 56));
#endif
}

// Functions to cast unpacked result to specific destination types:
static inline uint8_t ivory_serialize_unpack_uint8(const uint8_t *src, uint32_t offs) {
	uint8_t t = ivory_serialize_unpack_prim_1(src+offs);
	return *(uint8_t *)(&t);
}

static inline int8_t ivory_serialize_unpack_int8(const uint8_t *src, uint32_t offs) {
	uint8_t t = ivory_serialize_unpack_prim_1(src+offs);
	return *(int8_t *)(&t);
}

static inline uint16_t ivory_serialize_unpack_uint16(const uint8_t *src, uint32_t offs) {
	uint16_t t = ivory_serialize_unpack_prim_2(src+offs);
	return *(uint16_t *)(&t);
}

static inline int16_t ivory_serialize_unpack_int16(const uint8_t *src, uint32_t offs) {
	uint16_t t = ivory_serialize_unpack_prim_2(src+offs);
	return *(int16_t *)(&t);
}

static inline uint32_t ivory_serialize_unpack_uint32(const uint8_t *src, uint32_t offs) {
	uint32_t t = ivory_serialize_unpack_prim_4(src+offs);
	return *(uint32_t *)(&t);
}

static inline int32_t ivory_serialize_unpack_int32(const uint8_t *src, uint32_t offs) {
	uint32_t t = ivory_serialize_unpack_prim_4(src+offs);
	return *(int32_t *)(&t);
}

static inline float ivory_serialize_unpack_float(const uint8_t *src, uint32_t offs) {
	uint32_t t = ivory_serialize_unpack_prim_4(src+offs);
	return *(float *)(&t);
}

static inline uint64_t ivory_serialize_unpack_uint64(const uint8_t *src, uint32_t offs) {
	uint64_t t = ivory_serialize_unpack_prim_8(src+offs);
	return *(uint64_t *)(&t);
}

static inline int64_t ivory_serialize_unpack_int64(const uint8_t *src, uint32_t offs) {
	uint64_t t = ivory_serialize_unpack_prim_8(src+offs);
	return *(int64_t *)(&t);
}

static inline double ivory_serialize_unpack_double(const uint8_t *src, uint32_t offs) {
	uint64_t t = ivory_serialize_unpack_prim_8(src+offs);
	return *(double *)(&t);
}

#endif // __IVORY_SERIALIZE_PRIM_H__


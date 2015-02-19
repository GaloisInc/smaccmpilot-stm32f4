#ifndef __COMMSEC_FOR_MAVLINK_H
#define __COMMSEC_FOR_MAVLINK_H

#define COMMSEC_SUCCEED 0
#define COMMSEC_FAIL_DUP_CTR 2
#define COMMSEC_FAIL_CTR_ROLLOVER 3
#define COMMSEC_FAIL_GCM 6

#include <stdint.h>
#include "gcm.h"

// GCM auth tag length in bytes
// (do not adjust unless you are crypto savy)
#define TAG_LEN 16

// IV length in bytes.  The IV is [ Salt (64 bit) | Counter (32 bit)]
#define IV_LEN 12

// Transmitted header len (the [ Counter ] portion of the IV)
#define HEADER_LEN 4

#define PLAINTEXT_LEN 80
#define CIPHERTEXT_LEN (PLAINTEXT_LEN + HEADER_LEN + TAG_LEN)

struct commsec_encode {
    gcm_ctx encCtx;
    uint64_t encSalt;
    uint32_t myCounter;
};

struct commsec_decode {
    gcm_ctx decCtx;
    uint64_t decSalt;
    uint32_t mostRecentCounter;
};

void securePkg_init_enc( struct commsec_encode *ctx
                       , uint64_t encSalt, const uint8_t rawEncKey[16]);

void securePkg_init_dec( struct commsec_decode *ctx
                       , uint64_t decSalt, const uint8_t rawDecKey[16]);

uint32_t securePkg_encode(struct commsec_encode *ctx, const uint8_t plaintext[PLAINTEXT_LEN], uint8_t ciphertext[CIPHERTEXT_LEN]);
uint32_t securePkg_decode(struct commsec_decode *ctx, const uint8_t ciphertext_immutable[CIPHERTEXT_LEN], uint8_t plaintext[PLAINTEXT_LEN]);
void securePkg_zero_enc(struct commsec_encode *ctx);
void securePkg_zero_dec(struct commsec_decode *ctx);

int securePkg_size_of_message(int pkgLen);
int securePkg_size_of_package(int msgLen);

#endif /* __COMMSEC_FOR_MAVLINK_H */

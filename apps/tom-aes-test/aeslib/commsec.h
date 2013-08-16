#ifndef __COMMSEC_FOR_MAVLINK_H
#define __COMMSEC_FOR_MAVLINK_H

#define COMMSEC_SUCCEED 0
#define COMMSEC_FAIL_BAD_BASE_STATION 1
#define COMMSEC_FAIL_DUP_CTR 2
#define COMMSEC_FAIL_CTR_ROLLOVER 3
#define COMMSEC_FAIL_BAD_INPUT 4
#define COMMSEC_FAIL_MSG_LENGTH_VIOLATES_ASSUMPTIONS 5
#define COMMSEC_FAIL_GCM 6

#include <stdint.h>
#include "gcm.h"

#ifndef MAX_BASE_STATIONS
// Adjust this freely.
// It costs 4 bytes on the heap per additonal station.
#define MAX_BASE_STATIONS 16
#endif

// GCM auth tag length in bytes
// (do not adjust unless you are crypto savy)
#define TAG_LEN 8

// IV length in bytes.  The IV is [ Salt | StationId | Counter]
#define IV_LEN 12

// Transmitted header len (the [ StationId | Counter ] portion of the IV)
#define HEADER_LEN 8

// This is tied to TAG_LEN - do not adjust unless you are GCM savy
#define MAX_MESSAGE_LEN 512

typedef struct {
    gcm_ctx encCtx;
    uint32_t encSalt;
    uint32_t myId;
    uint32_t myCounter;

    gcm_ctx decCtx;
    uint32_t decSalt;
    uint32_t mostRecentCounter[MAX_BASE_STATIONS];
} commsec_ctx;

uint32_t securePkg_init( commsec_ctx *ctx, uint32_t myID
                       , uint32_t decSalt, const uint8_t *rawDecKey
                       , uint32_t encSalt, const uint8_t *rawEncKey);
uint32_t securePkg_enc_in_place(commsec_ctx *ctx, uint8_t *msg, uint32_t msgStartIdx, uint32_t msgLength);
uint32_t securePkg_enc( commsec_ctx *ctx, uint8_t *msg, uint32_t msgLen
                      , uint8_t *ivStorage, uint8_t *tag); // Tag must be 64 bits
uint32_t securePkg_dec(commsec_ctx *ctx, uint8_t *msg, uint32_t msgLen);
void securePkg_zero(commsec_ctx *ctx);

int securePkg_size_of_message(int pkgLen);
int securePkg_size_of_package(int msgLen);

#endif /* __COMMSEC_FOR_MAVLINK_H */

#include "commsec.h"
#include "gcm.h"
#include <string.h>

// Initialize a commsec context.  Each raw key must be 16 bytes long (excess entropy is ignored).
uint32_t securePkg_init( commsec_ctx *ctx, uint32_t myID
                       , uint32_t decSalt, const uint8_t *rawDecKey
                       , uint32_t encSalt, const uint8_t *rawEncKey)
{
    int i;
    gcm_init_and_key((const unsigned char *)rawDecKey, 16, &ctx->decCtx);
    gcm_init_and_key((const unsigned char *)rawEncKey, 16, &ctx->encCtx);
    ctx->encSalt = encSalt;
    ctx->decSalt = decSalt;
    ctx->myCounter = 1;
    ctx->myId      = myID;
    for(i = 0; i < MAX_BASE_STATIONS ; i++)
        ctx->mostRecentCounter[i] = 0;
    return COMMSEC_SUCCEED;
}

uint32_t securePkg_enc_in_place(commsec_ctx *ctx, uint8_t *msg, uint32_t msgStartIdx, uint32_t msgLen)
{
    uint32_t ret;
    if(msgStartIdx < 8) {
        ret = COMMSEC_FAIL_BAD_INPUT;
    } else {
        ret = securePkg_enc(ctx, msg+msgStartIdx, msgLen
                           , msg+msgStartIdx-HEADER_LEN
                           , msg+msgStartIdx+msgLen);
    }
    return ret;
}

uint32_t securePkg_enc(commsec_ctx *ctx, uint8_t *msg, uint32_t msgLen
                                       , uint8_t *ivStorage // ivStorage buffer must be at least HEADER_LEN bytes
                                       , uint8_t *tag) // Tag buffer must TAG_LEN bytes
{
    uint32_t ret=0;
    if(msgLen > MAX_MESSAGE_LEN) {
        ret = COMMSEC_FAIL_MSG_LENGTH_VIOLATES_ASSUMPTIONS;
    } else if(ctx->myCounter == UINT32_MAX) {
        ret = COMMSEC_FAIL_CTR_ROLLOVER;
    } else {
        uint8_t iv[IV_LEN];
        ret_type gcmRet;
        iv[0]  = (ctx->encSalt   >> 24) & 0xFF;
        iv[1]  = (ctx->encSalt   >> 16) & 0xFF;
        iv[2]  = (ctx->encSalt   >> 8 ) & 0xFF;
        iv[3]  = (ctx->encSalt        ) & 0xFF;
        iv[4]  = (ctx->myId      >> 24) & 0xFF;
        iv[5]  = (ctx->myId      >> 16) & 0xFF;
        iv[6]  = (ctx->myId      >> 8 ) & 0xFF;
        iv[7]  = (ctx->myId           ) & 0xFF;
        iv[8]  = (ctx->myCounter >> 24) & 0xFF;
        iv[9]  = (ctx->myCounter >> 16) & 0xFF;
        iv[10] = (ctx->myCounter >> 8 ) & 0xFF;
        iv[11] = (ctx->myCounter      ) & 0xFF;
        ctx->myCounter++;
        gcmRet = gcm_encrypt_message( (const unsigned char *)iv, 12
                                    , NULL, 0
                                    , msg, msgLen
                                    , tag, TAG_LEN
                                    , &ctx->encCtx);
        if(RETURN_GOOD == gcmRet) ret = COMMSEC_SUCCEED;
        else ret = COMMSEC_FAIL_GCM;
        memcpy(ivStorage, &iv[4], HEADER_LEN);
        memset(iv,0,IV_LEN);
    }
    return ret;
}

// Decrypt a package that is in the form [ StaID | Counter | CT | TAG ]
uint32_t securePkg_dec(commsec_ctx *ctx, uint8_t *pkg, uint32_t pkgLen)
{
    uint8_t iv[IV_LEN];
    uint32_t theirCounter=0, theirID=0;
    uint32_t ret = 0;

    if(pkgLen < HEADER_LEN + TAG_LEN + 1) {
        ret = COMMSEC_FAIL_MSG_LENGTH_VIOLATES_ASSUMPTIONS;
    } else {
        // Extract the ID and counter
        theirID      += ((uint32_t)pkg[0]) << 24;
        theirID      += ((uint32_t)pkg[1]) << 16;
        theirID      += ((uint32_t)pkg[2]) << 8;
        theirID      += ((uint32_t)pkg[3]);
        theirCounter += ((uint32_t)pkg[4]) << 24;
        theirCounter += ((uint32_t)pkg[5]) << 16;
        theirCounter += ((uint32_t)pkg[6]) << 8;
        theirCounter += ((uint32_t)pkg[7]);
        if(theirID >= MAX_BASE_STATIONS) {
            // the ID is invalid
            ret = COMMSEC_FAIL_BAD_BASE_STATION;
        } else if(ctx->mostRecentCounter[theirID] >= theirCounter) {
            // the counter is too old
            ret = COMMSEC_FAIL_DUP_CTR;
        } else {
            ret_type gcmRet;
            // Perform the decryption and update the recorded counter
            iv[0]  = (ctx->decSalt   >> 24) & 0xFF;
            iv[1]  = (ctx->decSalt   >> 16) & 0xFF;
            iv[2]  = (ctx->decSalt   >> 8 ) & 0xFF;
            iv[3]  = (ctx->decSalt        ) & 0xFF;
            iv[4]  = (theirID        >> 24) & 0xFF;
            iv[5]  = (theirID        >> 16) & 0xFF;
            iv[6]  = (theirID        >> 8 ) & 0xFF;
            iv[7]  = (theirID             ) & 0xFF;
            iv[8]  = (theirCounter   >> 24) & 0xFF;
            iv[9]  = (theirCounter   >> 16) & 0xFF;
            iv[10] = (theirCounter   >> 8 ) & 0xFF;
            iv[11] = (theirCounter        ) & 0xFF;

            gcmRet = gcm_decrypt_message( (const unsigned char *)iv, IV_LEN
                                        , NULL, 0
                                        , pkg + HEADER_LEN // msg ptr
                                        , pkgLen - HEADER_LEN - TAG_LEN // msg len
                                        , pkg + pkgLen - TAG_LEN // Tag ptr
                                        , TAG_LEN
                                        , &ctx->decCtx);
            if(RETURN_GOOD == gcmRet) {
                ret = COMMSEC_SUCCEED;
                ctx->mostRecentCounter[theirID] = theirCounter;
            }
            else ret = COMMSEC_FAIL_GCM;
        }
    }
    return ret;
}

// Zero's the cryptographic data.  The raw key data used
// for initialization must be cleared separately (should be done
// immediately after initilization).
void securePkg_zero(commsec_ctx *ctx)
{
    int i;
    gcm_end(&ctx->decCtx);
    gcm_end(&ctx->encCtx);
    ctx->encSalt=0;
    ctx->myId=0;
    ctx->myCounter = UINT32_MAX;
    ctx->decSalt=0;

    for(i=0; i < MAX_BASE_STATIONS; i++)
        ctx->mostRecentCounter[i] = UINT32_MAX;

    // Plenty of time, let's re-clear
    memset(&ctx->decCtx, 0xFF, sizeof(gcm_ctx));
    memset(&ctx->encCtx, 0xFF, sizeof(gcm_ctx));
    memset(&ctx->decCtx, 0x00, sizeof(gcm_ctx));
    memset(&ctx->encCtx, 0x00, sizeof(gcm_ctx));
}

int securePkg_size_of_message(int pkgLen)
{
    return (pkgLen - TAG_LEN - HEADER_LEN);
}

int securePkg_size_of_package(int msgLen)
{
    return (msgLen + TAG_LEN + HEADER_LEN);
}

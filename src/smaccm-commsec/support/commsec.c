#include <string.h>
#include "commsec.h"
#include "gcm.h"

// Initialize a commsec context.  Each raw key must be 16 bytes long (excess entropy is ignored).
void securePkg_init_enc( struct commsec_encode *ctx
                       , uint64_t encSalt, const uint8_t rawEncKey[16])
{
    gcm_init_and_key((const unsigned char *)rawEncKey, 16, &ctx->encCtx);
    ctx->encSalt = encSalt;
    ctx->myCounter = 1;
}

void securePkg_init_dec( struct commsec_decode *ctx
                       , uint64_t decSalt, const uint8_t rawDecKey[16])
{
    gcm_init_and_key((const unsigned char *)rawDecKey, 16, &ctx->decCtx);
    ctx->decSalt = decSalt;
    ctx->mostRecentCounter = 0;
}

// Ciphertext must be allocated CIPHERTEXT_LEN bytes while plaintext is assumed to be PLAINTEXT_LEN bytes
// Plaintext and ciphertext buffers must not overlap.
uint32_t securePkg_encode(struct commsec_encode *ctx, const uint8_t plaintext[PLAINTEXT_LEN], uint8_t ciphertext[CIPHERTEXT_LEN])
{
    uint8_t *msg       = ciphertext + HEADER_LEN;
    uint8_t *ivStorage = ciphertext;
    uint8_t *tag       = ciphertext + sizeof(ciphertext) - TAG_LEN;

    uint32_t ret=0;
    if(ctx->myCounter == UINT32_MAX) {
        ret = COMMSEC_FAIL_CTR_ROLLOVER;
    } else {
        uint8_t iv[IV_LEN];
        ret_type gcmRet;

        // The GCM encryption routine is in-place.  Copy the message over.
        memcpy(msg, plaintext, sizeof(plaintext));

        iv[0]  = (ctx->encSalt   >> 56) & 0xFF;
        iv[1]  = (ctx->encSalt   >> 48) & 0xFF;
        iv[2]  = (ctx->encSalt   >> 40) & 0xFF;
        iv[3]  = (ctx->encSalt   >> 32) & 0xFF;
        iv[4]  = (ctx->encSalt   >> 24) & 0xFF;
        iv[5]  = (ctx->encSalt   >> 16) & 0xFF;
        iv[6]  = (ctx->encSalt   >> 8 ) & 0xFF;
        iv[7]  = (ctx->encSalt        ) & 0xFF;
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
        memcpy(ivStorage, &iv[8], HEADER_LEN);
        memset(iv,0,IV_LEN);
    }
    return ret;
}

// Ciphertext must be allocated CIPHERTEXT_LEN bytes while plaintext is assumed to be PLAINTEXT_LEN bytes
// Plaintext and ciphertext buffers must not overlap.
//
// Decrypts a package that is in the form [ Counter | CT | TAG ]
uint32_t securePkg_decode( struct commsec_decode *ctx
                         , const uint8_t ciphertext_immutable[CIPHERTEXT_LEN]
                         , uint8_t plaintext[PLAINTEXT_LEN])
{
    uint8_t  iv[IV_LEN];
    uint32_t theirCounter = 0;
    uint32_t ret = 0;
    uint8_t ciphertext[CIPHERTEXT_LEN];
    uint8_t *transmitted_iv = ciphertext;
    uint8_t *msg = ciphertext + HEADER_LEN;
    uint8_t *tag = ciphertext + sizeof(ciphertext) - TAG_LEN;

    memcpy(ciphertext, ciphertext_immutable, sizeof(ciphertext));

    // Extract the ID and counter
    theirCounter += ((uint32_t)transmitted_iv[0]) << 24;
    theirCounter += ((uint32_t)transmitted_iv[1]) << 16;
    theirCounter += ((uint32_t)transmitted_iv[2]) << 8;
    theirCounter += ((uint32_t)transmitted_iv[3]);
    if(ctx->mostRecentCounter[theirID] >= theirCounter) {
        // the counter is too old
        ret = COMMSEC_FAIL_DUP_CTR;
    } else {
        ret_type gcmRet;
        // Perform the decryption and update the recorded counter
        iv[0]  = (ctx->decSalt   >> 56) & 0xFF;
        iv[1]  = (ctx->decSalt   >> 48) & 0xFF;
        iv[2]  = (ctx->decSalt   >> 40) & 0xFF;
        iv[3]  = (ctx->decSalt   >> 32) & 0xFF;
        iv[4]  = (ctx->decSalt   >> 24) & 0xFF;
        iv[5]  = (ctx->decSalt   >> 16) & 0xFF;
        iv[6]  = (ctx->decSalt   >> 8 ) & 0xFF;
        iv[7]  = (ctx->decSalt        ) & 0xFF;
        iv[8]  = (theirCounter   >> 24) & 0xFF;
        iv[9]  = (theirCounter   >> 16) & 0xFF;
        iv[10] = (theirCounter   >> 8 ) & 0xFF;
        iv[11] = (theirCounter        ) & 0xFF;

        gcmRet = gcm_decrypt_message( (const unsigned char *)iv, IV_LEN
                                    , NULL, 0          // AAD
                                    , msg , MSG_LEN
                                    , tag , TAG_LEN
                                    , &ctx->decCtx);
        if(RETURN_GOOD == gcmRet) {
            ret = COMMSEC_SUCCEED;
            ctx->mostRecentCounter[theirID] = theirCounter;
        }
        else ret = COMMSEC_FAIL_GCM;
    }
    return ret;
}

// Zero's the cryptographic data.  The raw key data used
// for initialization must be cleared separately (should be done
// immediately after initilization).
void securePkg_zero_enc( struct commsec_encode *ctx )
{
    int i;
    gcm_end(&ctx->encCtx);
    ctx->encSalt=0;
    ctx->myCounter = UINT32_MAX;

    // Plenty of time, let's re-clear
    memset(&ctx->encCtx, 0xFF, sizeof(gcm_ctx));
    memset(&ctx->encCtx, 0x00, sizeof(gcm_ctx));
}

// Zero out a context.
void securePkg_zero_dec( struct commsec_decode *ctx )
{
    int i;
    gcm_end(&ctx->decCtx);
    ctx->decSalt=0;

    ctx->mostRecentCounter = UINT32_MAX;

    // Plenty of time, let's re-clear
    memset(&ctx->decCtx, 0xFF, sizeof(gcm_ctx));
    memset(&ctx->decCtx, 0x00, sizeof(gcm_ctx));
}

// In the event we decide to allow variable-size packages we can compute the
// size of the plaintext for a given ciphertext using the below function.
int securePkg_size_of_message(int pkgLen)
{
    return (pkgLen - TAG_LEN - HEADER_LEN);
}

// In the event we decide to allow variable-size packages we can compute the
// size of the ciphertext for a given plaintext using the below function.
int securePkg_size_of_package(int msgLen)
{
    return (msgLen + TAG_LEN + HEADER_LEN);
}

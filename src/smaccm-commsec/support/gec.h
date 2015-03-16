#ifndef _GEC_H
#define _GEC_H

#include "aes/aes.h"
#include "curve25519/curve25519.h"
#include "ed25519/ed25519.h"

#define _GEC_SALT_LEN 8
#define _GEC_CTR_LEN 8
#define _GEC_TAG_LEN 16            /* Tags are full GCM tags */
#define _GEC_SYM_CIPHER_KEY_LEN 16 /* AES 128, hardcoded, should fix */
#define _IV_LEN 16

#if (_IV_LEN != _GEC_SALT_LEN + _GEC_CTR_LEN)
#error "IV Len should be equal to salt plus counter."
#endif

#define GEC_RAW_KEY_LEN (_GEC_SALT_LEN + _GEC_SYM_CIPHER_KEY_LEN)
#define GEC_PT_LEN              /* FIXME this depends on HACMS needs */
#define GEC_CT_LEN ((GEC_PTR_LEN) + _GEC_CTR_LEN + _GEC_TAG_LEN)

#define GEC_PUB_KEY_LEN  32            /* from curve25519-donna */
#define GEC_PRIV_KEY_LEN 32            /* from curve25519-donna*/
#define GEC_SECRET_BYTES_LEN 32        /* from curve25519_donna */

#define GEC_SIG_LEN 64          /* XXX floodberry's code uses a constant, no other CPP to leverage */
#define GEC_PUB_EPHEMERAL_KEY_LEN  32  /* from ed25519-donna */
#define GEC_PRIV_EPHEMERAL_KEY_LEN 32  /* from ed25519-donna*/
#define GEC_HASH_LEN HASH_DIGEST_SIZE  /* from ed25519-donna */

#define GEC_SUCCESS 0
#define GEC_ERROR_INVALID (-1)
#define GEC_ERROR_DUPLICATE_COUNTER (-2)
#define GEC_ERROR_DECRYPT_AUTH_FAILED (-3)
#define GEC_ERROR_COUNTER_ROLLOVER (-4)
#define GEC_ERROR_DECRYPT_AUTH_FAILED (-5)

// #define SUFFIX _LOW_LEVEL
// #define GEC_FN(fn) fn##SUFFIX
#define GEC_FN(fn) fn

struct gec_sym_key {
    gcm_ctx  gctx;
    uint8_t  salt[_GEC_SALT_LEN];
    uint64_t ctr;
};

/* A symmetric key structure for confidentiallity only encryption */
struct gec_sym_key_conf {
    aes_encrypt_ctx cctx;
    uint8_t         salt[_GEC_SALT_LEN];
    uint64_t        ctr;
};

struct gec_privkey {
    ed25519_secret_key priv;
    ed25519_public_key pub;
};

struct gec_pubkey {
    ed25519_public_key pub;
};

int GEC_FN(gec_init_sym_key_conf_auth)(struct gec_sym_key *k, const uint8_t rawkey[GEC_RAW_KEY_LEN]);
int GEC_FN(gec_init_sym_key_conf)(struct gec_sym_key_conf *k, const uint8_t rawkey[GEC_RAW_KEY_LEN]);

// Authenticated encryption mode over statically sized messages.
int GEC_FN(gec_encrypt)(const struct gec_sym_key *k, const uint8_t pt[GEC_PT_LEN], uint8_t ct[GEC_CT_LEN]);
int GEC_FN(gec_decrypt)(const struct gec_sym_key *k, const uint8_t ct[GEC_CT_LEN], uint8_t pt[GEC_PT_LEN]);

int GEC_FN(gec_encrypt_conf)(const struct gec_sym_key *k, const uint8_t *pt, uint8_t *ct, size_t len)
int GEC_FN(gec_decrypt_conf)(const struct gec_sym_key *k, const uint8_t *ct, uint8_t *pt, size_t len)

// Given random bytes in the privkey, construct a private and public key pair.
void GEC_FN(gec_generate_sign_keypair)(const struct gec_privkey q, struct gec_pubkey p);

// Given a private key and a message, create a signature.
void GEC_FN(gec_sign)(const struct gec_private_sign_key *k, const uint8_t msg[GEC_MSG_LEN], uint8_t sig[GEC_SIG_LEN]);
// Given a public key, message and tag (signature) return 0 if the signature is correct, non-zero otherwize
int GEC_FN(gec_verify)(const struct gec_public_sign_key *k, const uint8_t msg[GEC_MSG_LEN], const uint8_t sig[GEC_SIG_LEN]);

// Input random GEC_PRIV_KEY_LEN bytes and compute the matching public key.
int GEC_FN(gec_generate_ephemeral_keypair)(uint8_t gec_ephemeral_priv[GEC_PRIV_EPHEMERAL_KEY_LEN], uint8_t gec_ephemeral_pub[GEC_PUB_EPHEMERAL_KEY_LEN]);

// Given a public and private ephemeral keys, compute a shared secret.  That is, `secret_bytes = Hash(dh(pub,priv))`
int GEC_FN(gec_ecdh)(uint8_t secret_bytes[GEC_SECRET_BYTES_LEN], const uint8_t gec_ephemeral_priv[GEC_PRIV_EPHEMERAL_KEY_LEN], const uint8_t gec_ephemeral_pub[GEC_PUB_EPHEMERAL_KEY_LEN]);

// Compute a hash of the input.
int GEC_FN(gec_hash)(const uint8_t *input, size_t input_len, uint8_t digest[GEC_HASH_LEN]);

#endif /*  _GEC_H */

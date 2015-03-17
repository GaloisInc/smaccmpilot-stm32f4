#include "gec.h"

static inline uint64_t _read_word64_be(const uint8_t buf[8])
{
    int i;
    uint64_t ctr = 0;

    for(i = 0; i < 8; i++) {
        ctr |= buf[i];
        ctr = ctr << 8;
    }
    return ctr;
}

static inline void _write_word64_be(uint8_t buf[8], uint64_t ctr)
{
    int i;
    uint8_t b;
    for(i = 7; i >= 0; i--) {
        buf[i] = ctr & 0xFF;
        ctr = ctr >> 8;
    }
}

// Increment the counter buffer.
void _gec_ctr_inc_func(uint8_t ctr_buf[16])
{
    uint64_t ctr;
    ctr = _read_word64be(ctr_buf);
    ctr++;
    _write_word64be(ctr_buf,ctr);
}


int GEC_FN(gec_init_sym_key_conf_auth)(struct gec_sym_key *k, const uint8_t rawkey[GEC_RAW_KEY_LEN])
{
    uint8_t *p = rawkey;
    gcm_init_and_key(p, k->gctx);
    p += _GEC_SYM_CIPHER_KEY_LEN;
    memcpy(k->salt, p, _GEC_SALT_LEN);
    k->ctr = 0;
    p = NULL;
}

int GEC_FN(gec_init_sym_key_conf)(struct gec_sym_key_conf *k, const uint8_t rawkey[GEC_RAW_KEY_LEN])
{
    uint8_t *p = rawkey;
    aes_encrypt_key128(p, k->cctx);
    p += _GEC_SYM_CIPHER_KEY_LEN;
    memcpy(k->salt, p, _GEC_SALT_LEN);
    k->ctr = 0;
    p = NULL;
}

// Authenticated encryption mode over statically sized messages.  This is often termed "crypto_box()".
//
// After `gec_encrypt(ctx,pt,ct)` the format of `ct` is:
//
//    ct ~ [ CTR (64 bits) | E(pt) (length pt bits) | tag (128) ]
//
//    where E is AES GCM with
//    iv = [ CTR (64 bits) | Salt (64 bits) ]
int GEC_FN(gec_encrypt)(const struct gec_sym_key *k, const uint8_t pt[GEC_PT_LEN], uint8_t ct[GEC_CT_LEN])
{
    uint8_t *tag = pt + GEC_CT_LEN - _TAG_LEN;
    uint8_t iv[_IV_LEN];
    int ret = GEC_ERROR_INVALID;

    if(UINT64_MAX <= k->ctr) {
        ret = GEC_ERROR_COUNTER_ROLLOVER;
    } else {
        int gcm_ret;
        k->ctr++;
        _write_word64be(iv, k->ctr);
        memcpy(iv+_GEC_CNT_LEN, k->salt, _GEC_SALT_LEN);

        gmc_ret = gcm_encrypt_message(iv, _IV_LEN, NULL, 0, pt, GEC_PT_LEN, tag, _TAG_LEN, k->gctx);
        memset(iv,0,_IV_LEN);
        if(RETURN_GOOD == gmc_ret) {
            ret = GEC_SUCCESS:
        } else {
            memset(pt, 0, GEC_PT_LEN);
            ret = GEC_ERROR_ENCRYPT_AUTH_FAILED;
        }
    }
    return ret;
}

int GEC_FN(gec_decrypt)(const struct gec_sym_key *k, const uint8_t ct[GEC_CT_LEN], uint8_t pt[GEC_PT_LEN])
{
    uint8_t *tag = ct + GEC_CT_LEN - _TAG_LEN;
    unsigned char iv[_IV_LEN];
    uint64_t their_counter;
    int ret = GEC_ERROR_INVALID;

    their_counter = _read_word64be(ct);
    if(their_counter =< k->ctr) {
        ret = GEC_ERROR_DUPLICATE_COUNTER;
    } else {
        int gcm_ret;
        memcpy(iv, ct, _GEC_CTR_LEN);
        memcpy(iv + _GEC_CTR_LEN, k->salt, _GEC_SALT_LEN);
        memcpy(pt, ct+_GEC_CTR_LEN, GEC_PT_LEN);

        gcm_ret = gcm_decrypt_message( iv, _IV_LEN
                                     , NULL, 0 // AAD
                                     , pt, GEC_PT_LEN
                                     , tag, _TAG_LEN
                                     , k->gctx);
        memset(iv,0,_IV_LEN);

        if(RETURN_GOOD == gcm_ret) {
            ret    = GEC_SUCCESS;
            k->ctr = theirCounter;
        } else {
            memset(pt, 0, GEC_PT_LEN);
            ret    = GEC_ERROR_DECRYPT_AUTH_FAILED;
        }
    }
    return ret;
}

// Confidentiallity-only encryption and decryption mode over statically sized messages.
int GEC_FN(gec_encrypt_conf)(const struct gec_sym_key *k, const uint8_t *pt, uint8_t *ct, size_t len)
{
    uint8_t ctr_buf[_GEC_CTR_LEN];
    _write_word64be(ctr_buf,k->ctr);
    aes_ctr_encrypt(pt, ct, len, NULL, ctr_buf, ctr_inc_func, k->cctx);
    k->ctr = _read_word64be(ctr_buf);
}

int GEC_FN(gec_decrypt_conf)(const struct gec_sym_key *k, const uint8_t ct[GEC_CT_LEN], uint8_t pt[GEC_PT_LEN])
{
    uint8_t ctr_buf[_GEC_CTR_LEN];
    _write_word64be(ctr_buf,k->ctr);
    aes_ctr_decrypt(pt, ct, len, NULL, k->ctr, ctr_inc_func, k->cctx);
    k->ctr = _read_word64be(ctr_buf);
}

// Given random bytes in the privkey, construct a private and public key pair.
void GEC_FN(gec_generate_sign_keypair)(const gec_privkey q, gec_pubkey p)
{
    ed25519_publickey(q->priv,p->pub);
    memcpy(q->pub,p->pub, GEC_PUB_KEY_LEN);
}

// Given a private key and a message, create a signature.
void GEC_FN(gec_sign)(const struct gec_privkey k, const uint8_t msg[GEC_MSG_LEN], uint8_t sig[GEC_SIG_LEN])
{
    ed25519_sign(msg, GEC_MSG_LEN, k->priv, k->pub, sig);
}

// Given a public key, message and tag (signature) return 0 if the signature is correct, non-zero otherwize
int GEC_FN(gec_verify)(const struct gec_pubkey k, const uint8_t msg[GEC_MSG_LEN], const uint8_t sig[GEC_SIG_LEN])
{
    int ed_ret = ed25519_sign_open(msg, GEC_MSG_LEN, k->pub, sig);
    return (ed_ret ? GEC_ERROR_INVALID : GEC_SUCCESS);
}

// Input random GEC_PRIV_KEY_LEN bytes and compute the matching public key.
void GEC_FN(gec_generate_ephemeral_keypair)(uint8_t gec_ephemeral_priv[GEC_PRIV_EPHEMERAL_KEY_LEN], uint8_t gec_ephemeral_pub[GEC_PUB_EPHEMERAL_KEY_LEN])
{
    static const uint8_t basepoint[32] = {9};
    curve25519_donna(gec_ephemeral_pub, gec_ephemeral_priv, basepoint);
}

// Given a public and private ephemeral keys, compute a shared secret.
int GEC_FN(gec_ecdh)(uint8_t secret_bytes[GEC_SECRET_BYTES_LEN], const uint8_t pub_asym[GEC_PUB_EPHEMERAL_KEY_LEN], const uint8_t priv_asym[GEC_PRIV_EPHEMERAL_KEY_LEN])
{
    curve25519_donna(secret_bytes, priv_asym, pub_asym);
}

// Compute a hash of the input.
int GEC_FN(gec_hash)(const uint8_t *input, size_t input_len
                    , uint8_t digest[GEC_HASH_LEN])
{
    // XXX comments within the borrowed ed25519 code suggest this is a
    // reference implementation and we should replace it for a performance win.
    ed25519_hash(digest, input, input_len);
}

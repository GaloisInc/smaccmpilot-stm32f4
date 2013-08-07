#include "aeslib/commsec.h"
#include <string.h>
#include <stdio.h>

int main()
{
    commsec_ctx uav, base0, base1;
    uint8_t uav_to_base_key[16] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
    uint8_t base_to_uav_key[16] = {15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0};
    uint32_t uavID = 0;
    uint32_t base0ID = 0;
    uint32_t base1ID = 1;
    uint32_t b2uSalt = 9219834;
    uint32_t u2bSalt = 284920;
    unsigned char *msg = "This is a message"; // Message to encrypt
    unsigned char msgLen = strlen(msg);
    uint8_t *package_from_uav;                // Storage for the ciphertexts
    uint8_t *package_from_base0;
    uint8_t *package_from_base1;
    uint32_t packageLen = securePkg_size_of_package(msgLen);
    uint32_t res=0;
    int i;

    // Allocate memory for the package (extra room for head and tail)
    package_from_uav   = malloc(packageLen);
    package_from_base0 = malloc(packageLen);
    package_from_base1 = malloc(packageLen);
    // FIXME error check malloc and all other function calls

    // Populate packages, leaving the room the the header at the front
    memcpy(package_from_uav   + HEADER_LEN, msg, msgLen);
    memcpy(package_from_base0 + HEADER_LEN, msg, msgLen);
    memcpy(package_from_base1 + HEADER_LEN, msg, msgLen);

    securePkg_init(&uav, uavID, b2uSalt, base_to_uav_key
                              , u2bSalt, uav_to_base_key);
    securePkg_init(&base0, base0ID, u2bSalt, uav_to_base_key
                                  , b2uSalt, base_to_uav_key);
    securePkg_init(&base1, base1ID, u2bSalt, uav_to_base_key
                                  , b2uSalt, base_to_uav_key);
    // NB securePkg_init does not zero the raw key (*_key)
    // for security reasons, this memory should be zeroed

#if 1
    fprintf(stderr, "Plaintext: ");
    for(i=0; i<10; i++) {
        fprintf(stderr, "%02x", package_from_uav[HEADER_LEN+i] & 0xFF);
    }
    fprintf(stderr, "\n");
    fprintf(stderr, "Plaintext: ");
    for(i=0; i<10; i++) {
        fprintf(stderr, "%02x", package_from_base0[HEADER_LEN+i] & 0xFF);
    }
    fprintf(stderr, "\n");
#endif

    // Perform some encryptions
    securePkg_enc_in_place(&uav,    package_from_uav , HEADER_LEN, msgLen);
    securePkg_enc_in_place(&base0, package_from_base0, HEADER_LEN, msgLen);
    securePkg_enc_in_place(&base1, package_from_base1, HEADER_LEN, msgLen);

    // Optionally print the ciphertext to show things are encrypted.
    // Just statically set this to take the first 10 bytes of the message.
#if 1
    fprintf(stderr, "Ciphertext: ");
    for(i=0; i<10; i++) {
        fprintf(stderr, "%02x", package_from_uav[HEADER_LEN+i] & 0xFF);
    }
    fprintf(stderr, "\n");
    fprintf(stderr, "Ciphertext: ");
    for(i=0; i<10; i++) {
        fprintf(stderr, "%02x", package_from_base0[HEADER_LEN+i] & 0xFF);
    }
    fprintf(stderr, "\n");
    fprintf(stderr, "Ciphertext: ");
    for(i=0; i<10; i++) {
        fprintf(stderr, "%02x", package_from_base1[HEADER_LEN+i] & 0xFF);
    }
    fprintf(stderr, "\n");
#endif

    // Show that decryption works the way we want

    // UAV will not decrypt messages from UAV
    // Bases will not decrypt messages from other bases.
    // No entity will decrypt a message more than once (replays)
    res |= securePkg_dec(&uav, package_from_uav, packageLen);
    res |= securePkg_dec(&base0, package_from_base0, packageLen);
    res |= securePkg_dec(&base0, package_from_base1, packageLen);
    fprintf(stderr, "Result of invalid decryptions (should be nonzero): %d\n", res);

    // Due to _dec mutating memory, reencrypt (just for purpose of this example)
    // Perform some encryptions
    memcpy(package_from_uav   + HEADER_LEN, msg, msgLen);
    memcpy(package_from_base0 + HEADER_LEN, msg, msgLen);
    memcpy(package_from_base1 + HEADER_LEN, msg, msgLen);
    securePkg_enc_in_place(&uav, package_from_uav , HEADER_LEN, msgLen);
    securePkg_enc_in_place(&base0, package_from_base0, HEADER_LEN, msgLen);
    securePkg_enc_in_place(&base1, package_from_base1, HEADER_LEN, msgLen);

    // The UAV can decrypt _both_ the securePkg_dec messages
    res = securePkg_dec(&uav, package_from_base0, packageLen);
    res |= securePkg_dec(&uav, package_from_base1, packageLen);
    fprintf(stderr, "Result of valid decryptions (should be zero): %d\n", res);

#if 1
    fprintf(stderr, "Plaintext: ");
    for(i=0; i<10; i++) {
        fprintf(stderr, "%02x", package_from_base0[HEADER_LEN+i] & 0xFF);
    }
    fprintf(stderr, "\n");
    fprintf(stderr, "Plaintext: ");
    for(i=0; i<10; i++) {
        fprintf(stderr, "%02x", package_from_base1[HEADER_LEN+i] & 0xFF);
    }
    fprintf(stderr, "\n");
#endif

    securePkg_zero(&uav);
    securePkg_zero(&base0);
    securePkg_zero(&base1);

    return 0;
}

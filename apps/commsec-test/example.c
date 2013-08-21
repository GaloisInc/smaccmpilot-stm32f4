#include <string.h>
#include <stdio.h>
#include "aeslib/commsec.h"

int main()
{
    // The commsec_ctx structure is a structure that is *mutated*
    // as data is sent (counter value is incremented) and received
    // (one of an array of counter values can be updated).
    commsec_ctx uav, base0, base1;

    // Separate keys allow a clean division of ownership of particular
    // fields in our commsec_ctx structure.  Additionally, separate keys
    // is just standard practice, which we bend pretty far.
    uint8_t uav_to_base_key[16] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
    uint8_t base_to_uav_key[16] = {15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0};

    // The uavID can be thought of as completely arbitrary.
    // I was tempted to define a new format that did not require the
    // UAV to have an ID, but keeping symmetry proves more useful.
    uint32_t uavID = 0;

    // The base station IDs must be unique - this is critical!
    uint32_t base0ID = 0;
    uint32_t base1ID = 1;
    uint32_t b2uSalt = 9219834;
    uint32_t u2bSalt = 284920;

    // Our API was built with the assumption that messages, "msg" here,
    // would be build in-place in a larger buffer we call the "package".
    // Thus, this example with a static "msg" variable whose memory must be
    // copied is not representative of the intended use case.
    char *msg = "This is a message"; // Message to encrypt
    unsigned char msgLen = strlen(msg);

    // package_* variables are used to store the ciphertext from one
    // entity to another.
    uint8_t *package_from_uav;
    uint8_t *package_from_base0;
    uint8_t *package_from_base1;

    // We can use the size_of_package function to determine how large our
    // buffer needs to be in order to accommodate a plaintext message of
    // a given length.
    uint32_t packageLen = securePkg_size_of_package(msgLen);
    uint32_t res=0;
    int i;

    // Allocate memory for the package (extra room for head and tail)
    package_from_uav   = malloc(packageLen);
    package_from_base0 = malloc(packageLen);
    package_from_base1 = malloc(packageLen);
    // FIXME error check malloc and all other function calls

    // Populate packages, leaving the room the the header at the front.
    // The room required for the header is a static amount 'HEADER_LEN'.
    //
    // Potentially, future changes could alter the cipher or authentication
    // used and this is why we used securePkg_size_of_package(), instead
    // of adding a static value, to obtain the overall package size.
    memcpy(package_from_uav   + HEADER_LEN, msg, msgLen);
    memcpy(package_from_base0 + HEADER_LEN, msg, msgLen);
    memcpy(package_from_base1 + HEADER_LEN, msg, msgLen);

    // Initialization includes setting up tables for the GCM routine,
    // expanding AES keys, initializing counters to zero (incoming) and
    // one (outgoing).
    securePkg_init(&uav, uavID, b2uSalt, base_to_uav_key
                              , u2bSalt, uav_to_base_key);
    securePkg_init(&base0, base0ID, u2bSalt, uav_to_base_key
                                  , b2uSalt, base_to_uav_key);
    securePkg_init(&base1, base1ID, u2bSalt, uav_to_base_key
                                  , b2uSalt, base_to_uav_key);

    // NB securePkg_init does not zero the raw key (*_key)
    // for security reasons, that memory should be zeroed

#if 1
    fprintf(stderr, "Plaintext: ");
    for(i=0; i<msgLen; i++) {
        /* fprintf(stderr, "%02x", package_from_uav[HEADER_LEN+i] & 0xFF); */
        fprintf(stderr, "%c", package_from_uav[HEADER_LEN+i]);
    }
    fprintf(stderr, "\n");
    fprintf(stderr, "Plaintext: ");
    for(i=0; i<msgLen; i++) {
        /* fprintf(stderr, "%02x", package_from_base0[HEADER_LEN+i] & 0xFF); */
        fprintf(stderr, "%c", package_from_base0[HEADER_LEN+i]);
    }
    fprintf(stderr, "\n");
#endif

    // Perform some encryptions.  Alternatively, securePkg_enc() could have
    // been used and passed the right pointers to the memory area for the
    // header (package) the message (package+HEADER_LEN) and tail
    // (package+HEADER_LEN+msgLen).
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
    fprintf(stderr, "Result of invalid decryptions (should be nonzero): %d\n", res);
    res |= securePkg_dec(&base0, package_from_base0, packageLen);
    fprintf(stderr, "Result of invalid decryptions (should be nonzero): %d\n", res);
    res |= securePkg_dec(&base0, package_from_base1, packageLen);
    fprintf(stderr, "Result of invalid decryptions (should be nonzero): %d\n", res);

    // Due to _dec mutating memory, we reencrypt
    // (just for purpose of this example). Then perform some encryptions.
    memcpy(package_from_uav   + HEADER_LEN, msg, msgLen);
    memcpy(package_from_base0 + HEADER_LEN, msg, msgLen);
    memcpy(package_from_base1 + HEADER_LEN, msg, msgLen);
    securePkg_enc_in_place(&uav, package_from_uav,     HEADER_LEN, msgLen);
    securePkg_enc_in_place(&base0, package_from_base0, HEADER_LEN, msgLen);
    securePkg_enc_in_place(&base1, package_from_base1, HEADER_LEN, msgLen);

    // The UAV can decrypt _both_ the securePkg_dec messages
    res = securePkg_dec(&uav, package_from_base0, packageLen);
    res |= securePkg_dec(&uav, package_from_base1, packageLen);
    fprintf(stderr, "Result of valid decryptions (should be zero): %d\n", res);

#if 1
    fprintf(stderr, "Plaintext: ");
    for(i=0; i<msgLen; i++) {
        /* fprintf(stderr, "%02x", package_from_base0[HEADER_LEN+i] & 0xFF); */
      fprintf(stderr, "%c", package_from_base0[HEADER_LEN+i]);
    }
    fprintf(stderr, "\n");
    fprintf(stderr, "Plaintext: ");
    for(i=0; i<msgLen; i++) {
        /* fprintf(stderr, "%02x", package_from_base1[HEADER_LEN+i] & 0xFF); */
        fprintf(stderr, "%c", package_from_base1[HEADER_LEN+i]);
    }
    fprintf(stderr, "\n");
#endif

    securePkg_zero(&uav);
    securePkg_zero(&base0);
    securePkg_zero(&base1);

    return 0;
}

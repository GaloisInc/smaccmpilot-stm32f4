#ifndef IVORY_COMMSEC_SHIM_INCL
#define IVORY_COMMSEC_SHIM_INCL

#include <stdint.h>
#include "commsec.h"

commsec_ctx uav, base;

// Proxy type
/* typedef uint8_t commsec_ctx_proxy; */

/* uint32_t securePkg_init_ivory(commsec_ctx *ctx, uint32_t myID, */
/*                         uint32_t decSalt, const uint8_t *rawDecKey, */
/*                         uint32_t encSalt, const uint8_t *rawEncKey); */

/* uint32_t securePkg_enc_in_place_ivory(commsec_ctx *ctx, uint8_t *msg, */
/*                                 uint32_t msgStartIdx, uint32_t msgLength); */

#endif // IVORY_COMMSEC_SHIM_INCL

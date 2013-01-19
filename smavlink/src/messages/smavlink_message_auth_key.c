#include <smavlink/pack.h>
#include "smavlink_message_auth_key.h"
void smavlink_send_auth_key(struct auth_key_msg* var0,
                            struct smavlink_out_channel* var1,
                            struct smavlink_system* var2)
{
    uint8_t local0[32U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[32U] = &local0;
    uint8_t(* let2)[32U] = &var0->key;
    uint8_t ix3 = 0U % 32U;
    
    for (; ix3 < 31U % 32U; ix3 = ix3 + 1U % 32U) {
        uint8_t deref4 = *&*let2[ix3];
        
        smavlink_pack_uint8_t(ref1, 0U + ix3, deref4);
    }
    smavlink_send_ivory(var1, var2, 7U, ref1, 32U, 119U);
    return;
}

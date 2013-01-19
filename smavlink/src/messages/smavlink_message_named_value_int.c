#include <smavlink/pack.h>
#include "smavlink_message_named_value_int.h"
void smavlink_send_named_value_int(struct named_value_int_msg* var0,
                                   struct smavlink_out_channel* var1,
                                   struct smavlink_system* var2)
{
    uint8_t local0[18U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0};
    uint8_t(* ref1)[18U] = &local0;
    uint32_t deref2 = *&var0->time_boot_ms;
    
    smavlink_pack_uint32_t(ref1, 0U, deref2);
    
    int32_t deref3 = *&var0->value;
    
    smavlink_pack_int32_t(ref1, 4U, deref3);
    
    uint8_t(* let4)[10U] = &var0->name;
    uint8_t ix5 = 0U % 10U;
    
    for (; ix5 < 9U % 10U; ix5 = ix5 + 1U % 10U) {
        uint8_t deref6 = *&*let4[ix5];
        
        smavlink_pack_uint8_t(ref1, 8U + ix5, deref6);
    }
    smavlink_send_ivory(var1, var2, 252U, ref1, 18U, 44U);
    return;
}

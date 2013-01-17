#include <smavlink/pack.h>
#include "smavlink_message_named_value_float.h"
void smavlink_send_named_value_float(struct named_value_float_msg* var0,
                                     struct smavlink_out_channel* var1,
                                     struct smavlink_system* var2)
{
    uint8_t local0[18U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0};
    uint8_t(* ref1)[18U] = &local0;
    uint32_t deref2 = *&var0->time_boot_ms;
    
    smavlink_pack_uint32_t(ref1, 0, deref2);
    
    float deref3 = *&var0->value;
    
    smavlink_pack_float(ref1, 4, deref3);
    
    uint8_t(* let4)[10U] = &var0->name;
    uint8_t ix5 = 0 % 10;
    
    for (; ix5 < 9 % 10; ix5 = ix5 + 1 % 10) {
        uint8_t deref6 = *&*let4[ix5];
        
        smavlink_pack_uint8_t(ref1, 8 + ix5, deref6);
    }
    smavlink_send_ivory(var1, var2, 251, ref1, 18, 170);
    return;
}

#include <smavlink/pack.h>
#include "smavlink_message_debug.h"
void smavlink_send_debug(struct debug_msg* var0,
                         struct smavlink_out_channel* var1,
                         struct smavlink_system* var2)
{
    uint8_t local0[9U] = {0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[9U] = &local0;
    uint32_t deref2 = *&var0->time_boot_ms;
    
    smavlink_pack_uint32_t(ref1, 0U, deref2);
    
    float deref3 = *&var0->value;
    
    smavlink_pack_float(ref1, 4U, deref3);
    
    uint8_t deref4 = *&var0->ind;
    
    smavlink_pack_uint8_t(ref1, 8U, deref4);
    smavlink_send_ivory(var1, var2, 254U, ref1, 9U, 46U);
    return;
}

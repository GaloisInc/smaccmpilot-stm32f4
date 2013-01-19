#include <smavlink/pack.h>
#include "smavlink_message_debug_vect.h"
void smavlink_send_debug_vect(struct debug_vect_msg* var0,
                              struct smavlink_out_channel* var1,
                              struct smavlink_system* var2)
{
    uint8_t local0[30U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[30U] = &local0;
    uint64_t deref2 = *&var0->time_usec;
    
    smavlink_pack_uint64_t(ref1, 0U, deref2);
    
    float deref3 = *&var0->x;
    
    smavlink_pack_float(ref1, 8U, deref3);
    
    float deref4 = *&var0->y;
    
    smavlink_pack_float(ref1, 12U, deref4);
    
    float deref5 = *&var0->z;
    
    smavlink_pack_float(ref1, 16U, deref5);
    
    uint8_t(* let6)[10U] = &var0->name;
    uint8_t ix7 = 0U % 10U;
    
    for (; ix7 < 9U % 10U; ix7 = ix7 + 1U % 10U) {
        uint8_t deref8 = *&*let6[ix7];
        
        smavlink_pack_uint8_t(ref1, 20U + ix7, deref8);
    }
    smavlink_send_ivory(var1, var2, 250U, ref1, 30U, 49U);
    return;
}

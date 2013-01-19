#include <smavlink/pack.h>
#include "smavlink_message_mission_set_current.h"
void smavlink_send_mission_set_current(struct mission_set_current_msg* var0,
                                       struct smavlink_out_channel* var1,
                                       struct smavlink_system* var2)
{
    uint8_t local0[4U] = {0, 0, 0, 0};
    uint8_t(* ref1)[4U] = &local0;
    uint16_t deref2 = *&var0->mission_set_current_seq;
    
    smavlink_pack_uint16_t(ref1, 0U, deref2);
    
    uint8_t deref3 = *&var0->target_system;
    
    smavlink_pack_uint8_t(ref1, 2U, deref3);
    
    uint8_t deref4 = *&var0->target_component;
    
    smavlink_pack_uint8_t(ref1, 3U, deref4);
    smavlink_send_ivory(var1, var2, 41U, ref1, 4U, 28U);
    return;
}

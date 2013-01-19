#include <smavlink/pack.h>
#include "smavlink_message_mission_clear_all.h"
void smavlink_send_mission_clear_all(struct mission_clear_all_msg* var0,
                                     struct smavlink_out_channel* var1,
                                     struct smavlink_system* var2)
{
    uint8_t local0[2U] = {0, 0};
    uint8_t(* ref1)[2U] = &local0;
    uint8_t deref2 = *&var0->target_system;
    
    smavlink_pack_uint8_t(ref1, 0U, deref2);
    
    uint8_t deref3 = *&var0->target_component;
    
    smavlink_pack_uint8_t(ref1, 1U, deref3);
    smavlink_send_ivory(var1, var2, 45U, ref1, 2U, 232U);
    return;
}

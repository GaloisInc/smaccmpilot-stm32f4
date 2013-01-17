#include <smavlink/pack.h>
#include "smavlink_message_mission_write_partial_list.h"
void smavlink_send_mission_write_partial_list(struct mission_write_partial_list_msg* var0,
                                              struct smavlink_out_channel* var1,
                                              struct smavlink_system* var2)
{
    uint8_t local0[6U] = {0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[6U] = &local0;
    int16_t deref2 = *&var0->start_index;
    
    smavlink_pack_int16_t(ref1, 0, deref2);
    
    int16_t deref3 = *&var0->end_index;
    
    smavlink_pack_int16_t(ref1, 2, deref3);
    
    uint8_t deref4 = *&var0->target_system;
    
    smavlink_pack_uint8_t(ref1, 4, deref4);
    
    uint8_t deref5 = *&var0->target_component;
    
    smavlink_pack_uint8_t(ref1, 5, deref5);
    smavlink_send_ivory(var1, var2, 38, ref1, 6, 9);
    return;
}

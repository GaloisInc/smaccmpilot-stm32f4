#include <smavlink/pack.h>
#include "smavlink_message_mission_item_reached.h"
void smavlink_send_mission_item_reached(struct mission_item_reached_msg* var0,
                                        struct smavlink_out_channel* var1,
                                        struct smavlink_system* var2)
{
    uint8_t local0[2U] = {0, 0};
    uint8_t(* ref1)[2U] = &local0;
    uint16_t deref2 = *&var0->mission_item_reached_seq;
    
    smavlink_pack_uint16_t(ref1, 0, deref2);
    smavlink_send_ivory(var1, var2, 46, ref1, 2, 11);
    return;
}

#include <smavlink/pack.h>
#include "smavlink_message_heartbeat.h"
void smavlink_send_heartbeat(struct heartbeat_msg* var0,
                             struct smavlink_out_channel* var1,
                             struct smavlink_system* var2)
{
    uint8_t local0[9U] = {0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[9U] = &local0;
    uint32_t deref2 = *&var0->custom_mode;
    
    smavlink_pack_uint32_t(ref1, 0U, deref2);
    
    uint8_t deref3 = *&var0->mavtype;
    
    smavlink_pack_uint8_t(ref1, 4U, deref3);
    
    uint8_t deref4 = *&var0->autopilot;
    
    smavlink_pack_uint8_t(ref1, 5U, deref4);
    
    uint8_t deref5 = *&var0->base_mode;
    
    smavlink_pack_uint8_t(ref1, 6U, deref5);
    
    uint8_t deref6 = *&var0->system_status;
    
    smavlink_pack_uint8_t(ref1, 7U, deref6);
    
    uint8_t deref7 = *&var0->mavlink_version;
    
    smavlink_pack_uint8_t(ref1, 8U, deref7);
    smavlink_send_ivory(var1, var2, 0U, ref1, 9U, 50U);
    return;
}

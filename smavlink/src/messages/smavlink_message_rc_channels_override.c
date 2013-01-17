#include <smavlink/pack.h>
#include "smavlink_message_rc_channels_override.h"
void smavlink_send_rc_channels_override(struct rc_channels_override_msg* var0,
                                        struct smavlink_out_channel* var1,
                                        struct smavlink_system* var2)
{
    uint8_t local0[18U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0};
    uint8_t(* ref1)[18U] = &local0;
    uint16_t deref2 = *&var0->chan1_raw;
    
    smavlink_pack_uint16_t(ref1, 0, deref2);
    
    uint16_t deref3 = *&var0->chan2_raw;
    
    smavlink_pack_uint16_t(ref1, 2, deref3);
    
    uint16_t deref4 = *&var0->chan3_raw;
    
    smavlink_pack_uint16_t(ref1, 4, deref4);
    
    uint16_t deref5 = *&var0->chan4_raw;
    
    smavlink_pack_uint16_t(ref1, 6, deref5);
    
    uint16_t deref6 = *&var0->chan5_raw;
    
    smavlink_pack_uint16_t(ref1, 8, deref6);
    
    uint16_t deref7 = *&var0->chan6_raw;
    
    smavlink_pack_uint16_t(ref1, 10, deref7);
    
    uint16_t deref8 = *&var0->chan7_raw;
    
    smavlink_pack_uint16_t(ref1, 12, deref8);
    
    uint16_t deref9 = *&var0->chan8_raw;
    
    smavlink_pack_uint16_t(ref1, 14, deref9);
    
    uint8_t deref10 = *&var0->target_system;
    
    smavlink_pack_uint8_t(ref1, 16, deref10);
    
    uint8_t deref11 = *&var0->target_component;
    
    smavlink_pack_uint8_t(ref1, 17, deref11);
    smavlink_send_ivory(var1, var2, 70, ref1, 18, 124);
    return;
}

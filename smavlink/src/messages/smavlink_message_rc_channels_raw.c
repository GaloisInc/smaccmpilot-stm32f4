#include <smavlink/pack.h>
#include "smavlink_message_rc_channels_raw.h"
void smavlink_send_rc_channels_raw(struct rc_channels_raw_msg* var0,
                                   struct smavlink_out_channel* var1,
                                   struct smavlink_system* var2)
{
    uint8_t local0[22U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0};
    uint8_t(* ref1)[22U] = &local0;
    uint32_t deref2 = *&var0->time_boot_ms;
    
    smavlink_pack_uint32_t(ref1, 0, deref2);
    
    uint16_t deref3 = *&var0->chan1_raw;
    
    smavlink_pack_uint16_t(ref1, 4, deref3);
    
    uint16_t deref4 = *&var0->chan2_raw;
    
    smavlink_pack_uint16_t(ref1, 6, deref4);
    
    uint16_t deref5 = *&var0->chan3_raw;
    
    smavlink_pack_uint16_t(ref1, 8, deref5);
    
    uint16_t deref6 = *&var0->chan4_raw;
    
    smavlink_pack_uint16_t(ref1, 10, deref6);
    
    uint16_t deref7 = *&var0->chan5_raw;
    
    smavlink_pack_uint16_t(ref1, 12, deref7);
    
    uint16_t deref8 = *&var0->chan6_raw;
    
    smavlink_pack_uint16_t(ref1, 14, deref8);
    
    uint16_t deref9 = *&var0->chan7_raw;
    
    smavlink_pack_uint16_t(ref1, 16, deref9);
    
    uint16_t deref10 = *&var0->chan8_raw;
    
    smavlink_pack_uint16_t(ref1, 18, deref10);
    
    uint8_t deref11 = *&var0->port;
    
    smavlink_pack_uint8_t(ref1, 20, deref11);
    
    uint8_t deref12 = *&var0->rssi;
    
    smavlink_pack_uint8_t(ref1, 21, deref12);
    smavlink_send_ivory(var1, var2, 35, ref1, 22, 244);
    return;
}

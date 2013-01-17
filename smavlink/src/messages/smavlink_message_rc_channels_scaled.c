#include <smavlink/pack.h>
#include "smavlink_message_rc_channels_scaled.h"
void smavlink_send_rc_channels_scaled(struct rc_channels_scaled_msg* var0,
                                      struct smavlink_out_channel* var1,
                                      struct smavlink_system* var2)
{
    uint8_t local0[22U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0};
    uint8_t(* ref1)[22U] = &local0;
    uint32_t deref2 = *&var0->time_boot_ms;
    
    smavlink_pack_uint32_t(ref1, 0, deref2);
    
    int16_t deref3 = *&var0->chan1_scaled;
    
    smavlink_pack_int16_t(ref1, 4, deref3);
    
    int16_t deref4 = *&var0->chan2_scaled;
    
    smavlink_pack_int16_t(ref1, 6, deref4);
    
    int16_t deref5 = *&var0->chan3_scaled;
    
    smavlink_pack_int16_t(ref1, 8, deref5);
    
    int16_t deref6 = *&var0->chan4_scaled;
    
    smavlink_pack_int16_t(ref1, 10, deref6);
    
    int16_t deref7 = *&var0->chan5_scaled;
    
    smavlink_pack_int16_t(ref1, 12, deref7);
    
    int16_t deref8 = *&var0->chan6_scaled;
    
    smavlink_pack_int16_t(ref1, 14, deref8);
    
    int16_t deref9 = *&var0->chan7_scaled;
    
    smavlink_pack_int16_t(ref1, 16, deref9);
    
    int16_t deref10 = *&var0->chan8_scaled;
    
    smavlink_pack_int16_t(ref1, 18, deref10);
    
    uint8_t deref11 = *&var0->port;
    
    smavlink_pack_uint8_t(ref1, 20, deref11);
    
    uint8_t deref12 = *&var0->rssi;
    
    smavlink_pack_uint8_t(ref1, 21, deref12);
    smavlink_send_ivory(var1, var2, 34, ref1, 22, 237);
    return;
}

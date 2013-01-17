#include <smavlink/pack.h>
#include "smavlink_message_hil_rc_inputs_raw.h"
void smavlink_send_hil_rc_inputs_raw(struct hil_rc_inputs_raw_msg* var0,
                                     struct smavlink_out_channel* var1,
                                     struct smavlink_system* var2)
{
    uint8_t local0[33U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[33U] = &local0;
    uint64_t deref2 = *&var0->time_usec;
    
    smavlink_pack_uint64_t(ref1, 0, deref2);
    
    uint16_t deref3 = *&var0->chan1_raw;
    
    smavlink_pack_uint16_t(ref1, 8, deref3);
    
    uint16_t deref4 = *&var0->chan2_raw;
    
    smavlink_pack_uint16_t(ref1, 10, deref4);
    
    uint16_t deref5 = *&var0->chan3_raw;
    
    smavlink_pack_uint16_t(ref1, 12, deref5);
    
    uint16_t deref6 = *&var0->chan4_raw;
    
    smavlink_pack_uint16_t(ref1, 14, deref6);
    
    uint16_t deref7 = *&var0->chan5_raw;
    
    smavlink_pack_uint16_t(ref1, 16, deref7);
    
    uint16_t deref8 = *&var0->chan6_raw;
    
    smavlink_pack_uint16_t(ref1, 18, deref8);
    
    uint16_t deref9 = *&var0->chan7_raw;
    
    smavlink_pack_uint16_t(ref1, 20, deref9);
    
    uint16_t deref10 = *&var0->chan8_raw;
    
    smavlink_pack_uint16_t(ref1, 22, deref10);
    
    uint16_t deref11 = *&var0->chan9_raw;
    
    smavlink_pack_uint16_t(ref1, 24, deref11);
    
    uint16_t deref12 = *&var0->chan10_raw;
    
    smavlink_pack_uint16_t(ref1, 26, deref12);
    
    uint16_t deref13 = *&var0->chan11_raw;
    
    smavlink_pack_uint16_t(ref1, 28, deref13);
    
    uint16_t deref14 = *&var0->chan12_raw;
    
    smavlink_pack_uint16_t(ref1, 30, deref14);
    
    uint8_t deref15 = *&var0->rssi;
    
    smavlink_pack_uint8_t(ref1, 32, deref15);
    smavlink_send_ivory(var1, var2, 92, ref1, 33, 54);
    return;
}

#include <smavlink/pack.h>
#include "smavlink_message_raw_imu.h"
void smavlink_send_raw_imu(struct raw_imu_msg* var0,
                           struct smavlink_out_channel* var1,
                           struct smavlink_system* var2)
{
    uint8_t local0[26U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[26U] = &local0;
    uint64_t deref2 = *&var0->time_usec;
    
    smavlink_pack_uint64_t(ref1, 0, deref2);
    
    int16_t deref3 = *&var0->xacc;
    
    smavlink_pack_int16_t(ref1, 8, deref3);
    
    int16_t deref4 = *&var0->yacc;
    
    smavlink_pack_int16_t(ref1, 10, deref4);
    
    int16_t deref5 = *&var0->zacc;
    
    smavlink_pack_int16_t(ref1, 12, deref5);
    
    int16_t deref6 = *&var0->xgyro;
    
    smavlink_pack_int16_t(ref1, 14, deref6);
    
    int16_t deref7 = *&var0->ygyro;
    
    smavlink_pack_int16_t(ref1, 16, deref7);
    
    int16_t deref8 = *&var0->zgyro;
    
    smavlink_pack_int16_t(ref1, 18, deref8);
    
    int16_t deref9 = *&var0->xmag;
    
    smavlink_pack_int16_t(ref1, 20, deref9);
    
    int16_t deref10 = *&var0->ymag;
    
    smavlink_pack_int16_t(ref1, 22, deref10);
    
    int16_t deref11 = *&var0->zmag;
    
    smavlink_pack_int16_t(ref1, 24, deref11);
    smavlink_send_ivory(var1, var2, 27, ref1, 26, 144);
    return;
}

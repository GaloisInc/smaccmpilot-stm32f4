#include <smavlink/pack.h>
#include "smavlink_message_hil_state.h"
void smavlink_send_hil_state(struct hil_state_msg* var0,
                             struct smavlink_out_channel* var1,
                             struct smavlink_system* var2)
{
    uint8_t local0[56U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0};
    uint8_t(* ref1)[56U] = &local0;
    uint64_t deref2 = *&var0->time_usec;
    
    smavlink_pack_uint64_t(ref1, 0, deref2);
    
    float deref3 = *&var0->roll;
    
    smavlink_pack_float(ref1, 8, deref3);
    
    float deref4 = *&var0->pitch;
    
    smavlink_pack_float(ref1, 12, deref4);
    
    float deref5 = *&var0->yaw;
    
    smavlink_pack_float(ref1, 16, deref5);
    
    float deref6 = *&var0->rollspeed;
    
    smavlink_pack_float(ref1, 20, deref6);
    
    float deref7 = *&var0->pitchspeed;
    
    smavlink_pack_float(ref1, 24, deref7);
    
    float deref8 = *&var0->yawspeed;
    
    smavlink_pack_float(ref1, 28, deref8);
    
    int32_t deref9 = *&var0->lat;
    
    smavlink_pack_int32_t(ref1, 32, deref9);
    
    int32_t deref10 = *&var0->lon;
    
    smavlink_pack_int32_t(ref1, 36, deref10);
    
    int32_t deref11 = *&var0->alt;
    
    smavlink_pack_int32_t(ref1, 40, deref11);
    
    int16_t deref12 = *&var0->vx;
    
    smavlink_pack_int16_t(ref1, 44, deref12);
    
    int16_t deref13 = *&var0->vy;
    
    smavlink_pack_int16_t(ref1, 46, deref13);
    
    int16_t deref14 = *&var0->vz;
    
    smavlink_pack_int16_t(ref1, 48, deref14);
    
    int16_t deref15 = *&var0->xacc;
    
    smavlink_pack_int16_t(ref1, 50, deref15);
    
    int16_t deref16 = *&var0->yacc;
    
    smavlink_pack_int16_t(ref1, 52, deref16);
    
    int16_t deref17 = *&var0->zacc;
    
    smavlink_pack_int16_t(ref1, 54, deref17);
    smavlink_send_ivory(var1, var2, 90, ref1, 56, 183);
    return;
}

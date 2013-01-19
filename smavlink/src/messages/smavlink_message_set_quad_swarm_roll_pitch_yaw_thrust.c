#include <smavlink/pack.h>
#include "smavlink_message_set_quad_swarm_roll_pitch_yaw_thrust.h"
void smavlink_send_set_quad_swarm_roll_pitch_yaw_thrust(struct set_quad_swarm_roll_pitch_yaw_thrust_msg* var0,
                                                        struct smavlink_out_channel* var1,
                                                        struct smavlink_system* var2)
{
    uint8_t local0[34U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[34U] = &local0;
    uint8_t deref2 = *&var0->group;
    
    smavlink_pack_uint8_t(ref1, 32U, deref2);
    
    uint8_t deref3 = *&var0->mode;
    
    smavlink_pack_uint8_t(ref1, 33U, deref3);
    
    int16_t(* let4)[4U] = &var0->roll;
    uint8_t ix5 = 0U % 4U;
    
    for (; ix5 < 3U % 4U; ix5 = ix5 + 1U % 4U) {
        int16_t deref6 = *&*let4[ix5];
        
        smavlink_pack_int16_t(ref1, 0U + ix5, deref6);
    }
    
    int16_t(* let7)[4U] = &var0->pitch;
    uint8_t ix8 = 0U % 4U;
    
    for (; ix8 < 3U % 4U; ix8 = ix8 + 1U % 4U) {
        int16_t deref9 = *&*let7[ix8];
        
        smavlink_pack_int16_t(ref1, 8U + ix8, deref9);
    }
    
    int16_t(* let10)[4U] = &var0->yaw;
    uint8_t ix11 = 0U % 4U;
    
    for (; ix11 < 3U % 4U; ix11 = ix11 + 1U % 4U) {
        int16_t deref12 = *&*let10[ix11];
        
        smavlink_pack_int16_t(ref1, 16U + ix11, deref12);
    }
    
    uint16_t(* let13)[4U] = &var0->thrust;
    uint8_t ix14 = 0U % 4U;
    
    for (; ix14 < 3U % 4U; ix14 = ix14 + 1U % 4U) {
        uint16_t deref15 = *&*let13[ix14];
        
        smavlink_pack_uint16_t(ref1, 24U + ix14, deref15);
    }
    smavlink_send_ivory(var1, var2, 61U, ref1, 34U, 240U);
    return;
}

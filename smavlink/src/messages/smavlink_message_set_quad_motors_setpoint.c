#include <smavlink/pack.h>
#include "smavlink_message_set_quad_motors_setpoint.h"
void smavlink_send_set_quad_motors_setpoint(struct set_quad_motors_setpoint_msg* var0,
                                            struct smavlink_out_channel* var1,
                                            struct smavlink_system* var2)
{
    uint8_t local0[9U] = {0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[9U] = &local0;
    uint16_t deref2 = *&var0->motor_front_nw;
    
    smavlink_pack_uint16_t(ref1, 0, deref2);
    
    uint16_t deref3 = *&var0->motor_right_ne;
    
    smavlink_pack_uint16_t(ref1, 2, deref3);
    
    uint16_t deref4 = *&var0->motor_back_se;
    
    smavlink_pack_uint16_t(ref1, 4, deref4);
    
    uint16_t deref5 = *&var0->motor_left_sw;
    
    smavlink_pack_uint16_t(ref1, 6, deref5);
    
    uint8_t deref6 = *&var0->target_system;
    
    smavlink_pack_uint8_t(ref1, 8, deref6);
    smavlink_send_ivory(var1, var2, 60, ref1, 9, 30);
    return;
}

#include <smavlink/pack.h>
#include "smavlink_message_nav_controller_output.h"
void smavlink_send_nav_controller_output(struct nav_controller_output_msg* var0,
                                         struct smavlink_out_channel* var1,
                                         struct smavlink_system* var2)
{
    uint8_t local0[26U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[26U] = &local0;
    float deref2 = *&var0->nav_roll;
    
    smavlink_pack_float(ref1, 0, deref2);
    
    float deref3 = *&var0->nav_pitch;
    
    smavlink_pack_float(ref1, 4, deref3);
    
    float deref4 = *&var0->alt_error;
    
    smavlink_pack_float(ref1, 8, deref4);
    
    float deref5 = *&var0->aspd_error;
    
    smavlink_pack_float(ref1, 12, deref5);
    
    float deref6 = *&var0->xtrack_error;
    
    smavlink_pack_float(ref1, 16, deref6);
    
    int16_t deref7 = *&var0->nav_bearing;
    
    smavlink_pack_int16_t(ref1, 20, deref7);
    
    int16_t deref8 = *&var0->target_bearing;
    
    smavlink_pack_int16_t(ref1, 22, deref8);
    
    uint16_t deref9 = *&var0->wp_dist;
    
    smavlink_pack_uint16_t(ref1, 24, deref9);
    smavlink_send_ivory(var1, var2, 62, ref1, 26, 183);
    return;
}

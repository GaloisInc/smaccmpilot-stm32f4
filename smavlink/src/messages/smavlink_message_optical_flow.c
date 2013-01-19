#include <smavlink/pack.h>
#include "smavlink_message_optical_flow.h"
void smavlink_send_optical_flow(struct optical_flow_msg* var0,
                                struct smavlink_out_channel* var1,
                                struct smavlink_system* var2)
{
    uint8_t local0[26U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[26U] = &local0;
    uint64_t deref2 = *&var0->time_usec;
    
    smavlink_pack_uint64_t(ref1, 0U, deref2);
    
    float deref3 = *&var0->flow_comp_m_x;
    
    smavlink_pack_float(ref1, 8U, deref3);
    
    float deref4 = *&var0->flow_comp_m_y;
    
    smavlink_pack_float(ref1, 12U, deref4);
    
    float deref5 = *&var0->ground_distance;
    
    smavlink_pack_float(ref1, 16U, deref5);
    
    int16_t deref6 = *&var0->flow_x;
    
    smavlink_pack_int16_t(ref1, 20U, deref6);
    
    int16_t deref7 = *&var0->flow_y;
    
    smavlink_pack_int16_t(ref1, 22U, deref7);
    
    uint8_t deref8 = *&var0->sensor_id;
    
    smavlink_pack_uint8_t(ref1, 24U, deref8);
    
    uint8_t deref9 = *&var0->quality;
    
    smavlink_pack_uint8_t(ref1, 25U, deref9);
    smavlink_send_ivory(var1, var2, 100U, ref1, 26U, 175U);
    return;
}

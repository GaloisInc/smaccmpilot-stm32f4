#include <smavlink/pack.h>
#include "smavlink_message_mission_item.h"
void smavlink_send_mission_item(struct mission_item_msg* var0,
                                struct smavlink_out_channel* var1,
                                struct smavlink_system* var2)
{
    uint8_t local0[37U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0};
    uint8_t(* ref1)[37U] = &local0;
    float deref2 = *&var0->param1;
    
    smavlink_pack_float(ref1, 0, deref2);
    
    float deref3 = *&var0->param2;
    
    smavlink_pack_float(ref1, 4, deref3);
    
    float deref4 = *&var0->param3;
    
    smavlink_pack_float(ref1, 8, deref4);
    
    float deref5 = *&var0->param4;
    
    smavlink_pack_float(ref1, 12, deref5);
    
    float deref6 = *&var0->x;
    
    smavlink_pack_float(ref1, 16, deref6);
    
    float deref7 = *&var0->y;
    
    smavlink_pack_float(ref1, 20, deref7);
    
    float deref8 = *&var0->z;
    
    smavlink_pack_float(ref1, 24, deref8);
    
    uint16_t deref9 = *&var0->mission_item_seq;
    
    smavlink_pack_uint16_t(ref1, 28, deref9);
    
    uint16_t deref10 = *&var0->command;
    
    smavlink_pack_uint16_t(ref1, 30, deref10);
    
    uint8_t deref11 = *&var0->target_system;
    
    smavlink_pack_uint8_t(ref1, 32, deref11);
    
    uint8_t deref12 = *&var0->target_component;
    
    smavlink_pack_uint8_t(ref1, 33, deref12);
    
    uint8_t deref13 = *&var0->frame;
    
    smavlink_pack_uint8_t(ref1, 34, deref13);
    
    uint8_t deref14 = *&var0->current;
    
    smavlink_pack_uint8_t(ref1, 35, deref14);
    
    uint8_t deref15 = *&var0->autocontinue;
    
    smavlink_pack_uint8_t(ref1, 36, deref15);
    smavlink_send_ivory(var1, var2, 39, ref1, 37, 254);
    return;
}

#include <smavlink/pack.h>
#include "smavlink_message_command_long.h"
void smavlink_send_command_long(struct command_long_msg* var0,
                                struct smavlink_out_channel* var1,
                                struct smavlink_system* var2)
{
    uint8_t local0[33U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[33U] = &local0;
    float deref2 = *&var0->param1;
    
    smavlink_pack_float(ref1, 0, deref2);
    
    float deref3 = *&var0->param2;
    
    smavlink_pack_float(ref1, 4, deref3);
    
    float deref4 = *&var0->param3;
    
    smavlink_pack_float(ref1, 8, deref4);
    
    float deref5 = *&var0->param4;
    
    smavlink_pack_float(ref1, 12, deref5);
    
    float deref6 = *&var0->param5;
    
    smavlink_pack_float(ref1, 16, deref6);
    
    float deref7 = *&var0->param6;
    
    smavlink_pack_float(ref1, 20, deref7);
    
    float deref8 = *&var0->param7;
    
    smavlink_pack_float(ref1, 24, deref8);
    
    uint16_t deref9 = *&var0->command;
    
    smavlink_pack_uint16_t(ref1, 28, deref9);
    
    uint8_t deref10 = *&var0->target_system;
    
    smavlink_pack_uint8_t(ref1, 30, deref10);
    
    uint8_t deref11 = *&var0->target_component;
    
    smavlink_pack_uint8_t(ref1, 31, deref11);
    
    uint8_t deref12 = *&var0->confirmation;
    
    smavlink_pack_uint8_t(ref1, 32, deref12);
    smavlink_send_ivory(var1, var2, 76, ref1, 33, 152);
    return;
}

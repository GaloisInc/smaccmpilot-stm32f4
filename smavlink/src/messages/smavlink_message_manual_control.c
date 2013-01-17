#include <smavlink/pack.h>
#include "smavlink_message_manual_control.h"
void smavlink_send_manual_control(struct manual_control_msg* var0,
                                  struct smavlink_out_channel* var1,
                                  struct smavlink_system* var2)
{
    uint8_t local0[11U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8_t(* ref1)[11U] = &local0;
    int16_t deref2 = *&var0->x;
    
    smavlink_pack_int16_t(ref1, 0, deref2);
    
    int16_t deref3 = *&var0->y;
    
    smavlink_pack_int16_t(ref1, 2, deref3);
    
    int16_t deref4 = *&var0->z;
    
    smavlink_pack_int16_t(ref1, 4, deref4);
    
    int16_t deref5 = *&var0->r;
    
    smavlink_pack_int16_t(ref1, 6, deref5);
    
    uint16_t deref6 = *&var0->buttons;
    
    smavlink_pack_uint16_t(ref1, 8, deref6);
    
    uint8_t deref7 = *&var0->target;
    
    smavlink_pack_uint8_t(ref1, 10, deref7);
    smavlink_send_ivory(var1, var2, 69, ref1, 11, 243);
    return;
}

#include <smavlink/pack.h>
#include "smavlink_message_state_correction.h"
void smavlink_send_state_correction(struct state_correction_msg* var0,
                                    struct smavlink_out_channel* var1,
                                    struct smavlink_system* var2)
{
    uint8_t local0[36U] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0};
    uint8_t(* ref1)[36U] = &local0;
    float deref2 = *&var0->xErr;
    
    smavlink_pack_float(ref1, 0, deref2);
    
    float deref3 = *&var0->yErr;
    
    smavlink_pack_float(ref1, 4, deref3);
    
    float deref4 = *&var0->zErr;
    
    smavlink_pack_float(ref1, 8, deref4);
    
    float deref5 = *&var0->rollErr;
    
    smavlink_pack_float(ref1, 12, deref5);
    
    float deref6 = *&var0->pitchErr;
    
    smavlink_pack_float(ref1, 16, deref6);
    
    float deref7 = *&var0->yawErr;
    
    smavlink_pack_float(ref1, 20, deref7);
    
    float deref8 = *&var0->vxErr;
    
    smavlink_pack_float(ref1, 24, deref8);
    
    float deref9 = *&var0->vyErr;
    
    smavlink_pack_float(ref1, 28, deref9);
    
    float deref10 = *&var0->vzErr;
    
    smavlink_pack_float(ref1, 32, deref10);
    smavlink_send_ivory(var1, var2, 64, ref1, 36, 130);
    return;
}

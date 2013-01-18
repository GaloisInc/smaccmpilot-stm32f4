#include "pid_stabilize.h"
float pid_update(struct PID* var0, float var1)
{
    float deref0 = *&var0->pid_pGain;
    float deref1 = *&var0->pid_iMin;
    float deref2 = *&var0->pid_iMax;
    float deref3 = *&var0->pid_iState;
    
    *&var0->pid_iState = (float) (deref3 + var1) <
        (float) deref1 ? deref1 : (float) (deref3 + var1) >
        (float) deref2 ? deref2 : deref3 + var1;
    
    float deref4 = *&var0->pid_iGain;
    float deref5 = *&var0->pid_iState;
    
    return deref0 * var1 + deref4 * deref5;
}
float stabilize_from_angle(struct PID* var0, struct PID* var1, float var2,
                           float var3, float var4, float var5, float var6)
{
    float r0 = pid_update(var0, var2 * var3 - var4 * 57.29578);
    float r1 = pid_update(var1, r0 - var5 * 57.29578);
    
    return ((float) r1 < (float) (0 - var6) ? 0 - var6 : (float) r1 >
            (float) var6 ? var6 : r1) / var6;
}
float stabilize_from_rate(struct PID* var0, float var1, float var2, float var3,
                          float var4)
{
    float r0 = pid_update(var0, var1 * var2 - var3 * 57.29578);
    
    return ((float) r0 < (float) (0 - var4) ? 0 - var4 : (float) r0 >
            (float) var4 ? var4 : r0) / var4;
}

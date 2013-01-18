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
    float let0 = var2 * var3;
    float let1 = var4 * 57.29578;
    float let2 = let0 - let1;
    float r3 = pid_update(var0, let2);
    float let4 = var5 * 57.29578;
    float let5 = r3 - let4;
    float r6 = pid_update(var1, let5);
    float let7 = ((float) r6 < (float) (0 - var6) ? 0 - var6 : (float) r6 >
                  (float) var6 ? var6 : r6) / var6;
    
    return let7;
}
float stabilize_from_rate(struct PID* var0, float var1, float var2, float var3,
                          float var4)
{
    float let0 = var1 * var2;
    float let1 = var3 * 57.29578;
    float let2 = let0 - let1;
    float r3 = pid_update(var0, let2);
    float let4 = ((float) r3 < (float) (0 - var4) ? 0 - var4 : (float) r3 >
                  (float) var4 ? var4 : r3) / var4;
    
    return let4;
}

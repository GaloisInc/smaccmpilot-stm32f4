#ifndef __PID_STABILIZE_H__
#define __PID_STABILIZE_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
struct PID {
    float pid_pGain;
    float pid_iGain;
    float pid_iState;
    float pid_iMin;
    float pid_iMax;
};
float pid_update(struct PID* var0, float var1);
float stabilize_axis(struct PID* var0, struct PID* var1, float var2, float var3,
                     float var4, float var5, float var6);

#ifdef __cplusplus
}
#endif
#endif /* __PID_STABILIZE_H__ */
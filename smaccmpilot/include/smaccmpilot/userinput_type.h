/* This file has been autogenerated by Ivory
 * Compiler version  Version {versionBranch = [0,1,0,0], versionTags = []}
 */
#ifndef __USERINPUT_TYPE_H__
#define __USERINPUT_TYPE_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <ivory.h>
struct userinput_result {
    bool armed;
    float throttle;
    float roll;
    float pitch;
    float yaw;
    uint8_t mode;
    uint32_t time;
};

#ifdef __cplusplus
}
#endif
#endif /* __USERINPUT_TYPE_H__ */
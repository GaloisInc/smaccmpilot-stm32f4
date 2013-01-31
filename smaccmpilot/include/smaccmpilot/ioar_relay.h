
#ifndef __SMACCMPILOT_IOAR_RELAY_H__
#define __SMACCMPILOT_IOAR_RELAY_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>

#define IOAR_RELAY_OFF          0
#define IOAR_RELAY_ON           1
#define IOAR_RELAY_BLINK_SLOW   2
#define IOAR_RELAY_BLINK_FAST   3
#define IOAR_RELAY_PULSE_SLOW   4
#define IOAR_RELAY_PULSE_FAST   5
#define IOAR_RELAY_PULSE_EXTRA_FAST   6
#define IOAR_RELAY_NUM_PATTERNS 7

void ioar_relay_init(void);
void ioar_relay_start_task(void);
void ioar_relay_set(int state);

#ifdef __cplusplus
}
#endif

#endif // __SMACCMPILOT_IOAR_RELAY_H__


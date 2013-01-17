

#ifndef __APP_STABILIZE_GCS_H__
#define __APP_STABILIZE_GCS_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <FreeRTOS.h>

struct gcs_state {
    bool armed;
};

void gcs_init(void);
void gcs_set(const struct gcs_state *state);

#ifdef __cplusplus
}
#endif

#endif // __APP_STABILIZE_GCS_H__


#include <FreeRTOS.h>
#include <task.h>

#include <string.h>

#include <smaccmpilot/optflow_compensate.h>

static uint8_t optflow_limit_mode(struct optflow_result *optflow,
                               const struct userinput_result *in);

static void optflow_no_comp   (struct optflow_result *optflow,
                               const struct userinput_result *in,
                               struct userinput_result *out);

static void optflow_alt_hold  (struct optflow_result *optflow,
                               const struct userinput_result *in,
                               struct userinput_result *out);

static void optflow_loiter    (struct optflow_result *optflow,
                               const struct userinput_result *in,
                               struct userinput_result *out);

void optflow_compensate(struct optflow_result *optflow,
                        const struct userinput_result *in,
                        struct userinput_result *out)
{
    uint8_t optflow_mode = optflow_limit_mode(optflow, in);
    if (optflow_mode == 0) { /* stabilize */
        optflow_no_comp(optflow, in, out);
    } else if (out->mode == 1) { /* alt_hold */
        optflow_alt_hold(optflow, in, out);
    } else if (out->mode == 2) { /* loiter */
        optflow_alt_hold(optflow, in, out);
        optflow_loiter(optflow, in, out);
    }
    
}

static uint8_t optflow_limit_mode(struct optflow_result *optflow,
                               const struct userinput_result *in)
{
    uint32_t now = xTaskGetTickCount();
    if ((now - optflow->time) > 150 || !optflow->valid) {
        return 0; /* stabilize */
    }

    if (optflow->quality > 40) {
        if (in->mode == 2) {
            return 2; /* loiter */
        }
    }
    if (optflow->ground_dist > 0.3f) {
        if (in->mode == 1) {
            return 1; /* alt_hold */
        }
    }

    return 0; /* stabilize */

}

static void optflow_alt_hold  (struct optflow_result *optflow,
                               const struct userinput_result *in,
                               struct userinput_result *out)
{
    memcpy(out, in, sizeof(struct userinput_result)); // PUNT
    out->mode = 1; /* alt_hold */
}

static void optflow_loiter    (struct optflow_result *optflow,
                               const struct userinput_result *in,
                               struct userinput_result *out)
{
    memcpy(out, in, sizeof(struct userinput_result)); // PUNT
    out->mode = 2; /* loiter */
}

static void optflow_no_comp   (struct optflow_result *optflow,
                               const struct userinput_result *in,
                               struct userinput_result *out)
{
    memcpy(out, in, sizeof(struct userinput_result));
    out->mode = 0; /* stabilize */
}

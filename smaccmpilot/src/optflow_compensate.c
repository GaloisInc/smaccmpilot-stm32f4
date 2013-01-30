
#include <string.h>

#include <smaccmpilot/optflow_compensate.h>

void optflow_compensate(struct optflow_result *optflow,
                        struct userinput_result *userin,
                        struct userinput_result *out)
{
    memcpy(out, userin, sizeof(struct userinput_result));
}


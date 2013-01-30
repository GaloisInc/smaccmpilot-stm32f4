
#include <FreeRTOS.h>
#include <task.h>
#include <semphr.h>

#include <hwf4/usart.h>

#include <include/mavlink/v1.0/mavlink_types.h>
#include <include/mavlink/v1.0/common/mavlink.h>

#include <smaccmpilot/optflow_input.h>

#ifdef CONFIG_OPTFLOW_UART
# define OPTFLOW_UART CONFIG_GCS_UART
#else
# define OPTFLOW_UART usart6
#endif

static void panic(const char* panicmsg);

static xSemaphoreHandle optflow_state_mutex;
static struct optflow_result shared_optflow_state;
static void optflow_set(struct optflow_result *optflow);

/* optflow receive task */
static xTaskHandle optflow_receive_taskhandle;
static void optflow_receive_task(void *arg);
static void optflow_receive_handler(const mavlink_message_t *msg);

static void optflow_receive_handle_optical_flow(const mavlink_message_t *msg);
static void optflow_receive_handle_unknown(const mavlink_message_t *msg);

static void optflow_receive_usart_init(void);

void optflow_input_init(void) {
    optflow_state_mutex = xSemaphoreCreateMutex();
}

void optflow_input_start_task(void) {
    xTaskCreate(optflow_receive_task, (signed char *)"ofr", 512, NULL, 1,
                &optflow_receive_taskhandle);
}

void optflow_input_get(struct optflow_result *optflow)
{
    if (xSemaphoreTake(optflow_state_mutex, 1)) {
        memcpy(optflow, &shared_optflow_state,
                        sizeof(struct optflow_result));
        xSemaphoreGive(optflow_state_mutex);
    } else {
        panic("PANIC: optflow_get took too long "
            "to take memory barrier\r\n");
    }
}

static void optflow_set(struct optflow_result *optflow)
{
    if (xSemaphoreTake(optflow_state_mutex, 1)) {
        memcpy(&shared_optflow_state, optflow,
                        sizeof(struct optflow_result));
        xSemaphoreGive(optflow_state_mutex);
    } else {
        panic("PANIC: optflow_set took too long "
            "to take memory barrier\r\n");
    }
}

static void optflow_receive_task(void *arg) {
    
    optflow_receive_usart_init();

    mavlink_message_t msg;
    mavlink_status_t status;
    ssize_t count;
    uint8_t byte;

    for (;;) {
        count = usart_read_timeout(OPTFLOW_UART, portMAX_DELAY, &byte, 1);
        if (count > 0) {
            bool rxcomplete = mavlink_parse_char(0, byte, &msg, &status);
            if (rxcomplete) {
                optflow_receive_handler(&msg);
            }
        }
    }
}

static void optflow_receive_handler(const mavlink_message_t *msg) {
    switch (msg->msgid) {
        case MAVLINK_MSG_ID_OPTICAL_FLOW:
            optflow_receive_handle_optical_flow(msg);
            break;
        default:
            optflow_receive_handle_unknown(msg);
            break;
    }
}

static void optflow_receive_handle_optical_flow(const mavlink_message_t *msg)
{
    struct optflow_result result;

    mavlink_optical_flow_t m;
    mavlink_msg_optical_flow_decode(msg, &m);

    uint32_t now = xTaskGetTickCount();

    result.valid       = true;
    result.ground_dist = m.ground_distance;
    result.flow_x      = m.flow_comp_m_x;
    result.flow_y      = m.flow_comp_m_y;
    result.quality     = m.quality;
    result.sensortime  = m.time_usec;
    result.time        = now;

    optflow_set(&result);
}

static void optflow_receive_handle_unknown(const mavlink_message_t *msg) {
    /* intentionally left blank */
}

static void panic(const char* panicmsg) {
        usart_write(usart2, (const uint8_t*)panicmsg, strlen(panicmsg));
        for(;;);
}

static void optflow_receive_usart_init(void) {
    usart_init(OPTFLOW_UART, 115200);
    usart_enable(OPTFLOW_UART);
}

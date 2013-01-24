/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * main.c --- SMAVLink heartbeat test.
 *
 * Copyright (C) 2012, Galois, Inc.
 * All Rights Reserved.
 *
 * This software is released under the "BSD3" license.  Read the file
 * "LICENSE" for more information.
 */

#include <sys/types.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <FreeRTOS.h>
#include <task.h>

#include <hwf4/usart.h>

#include <smavlink/send.h>
#include <smavlink/receive.h>
#include <smavlink/messages/smavlink_message_heartbeat.h>

#define SMAVLINK_UART usart1
#define DEBUG_UART    usart1

/*
 * Debug UART
 */

/** Initialize the debug UART. */
static void debug_uart_init(void)
{
  usart_init(DEBUG_UART, 115200);
  usart_enable(DEBUG_UART);
}

/** Write a string followed by CR/LF to the debug UART. */
static void debug_puts(const char *s)
{
  usart_write(DEBUG_UART, (uint8_t *)s, strlen(s));
  usart_write(DEBUG_UART, (uint8_t *)"\r\n", 2);
}

/** Write formatted output to the debug UART. */
static void debug_printf(const char *fmt, ...)
{
  va_list ap;
  char buf[128];

  va_start(ap, fmt);
  vsnprintf(buf, sizeof(buf), fmt, ap);
  usart_write(DEBUG_UART, (uint8_t *)buf, strlen(buf));
  va_end(ap);
}

/*
 * SMAVLink Channels
 */

static bool gcs_begin_atomic(void *arg, size_t len)
{
    return true;
}

static void gcs_end_atomic(void *arg)
{
}

static size_t gcs_write(void *arg, const uint8_t *buf, size_t len)
{
    struct usart *dev = (struct usart *)arg;
    return usart_write(dev, buf, len);
}

static ssize_t gcs_read(void *arg, uint8_t *buf, size_t len)
{
    struct usart *dev = (struct usart *)arg;
    return usart_read(dev, buf, len);
}

static struct smavlink_out_channel g_out_ch = {
    0,                          /* tx_seq */
    SMAVLINK_UART,              /* write_delegate */
    gcs_begin_atomic,           /* begin_atomic */
    gcs_end_atomic,             /* end_atomic */
    gcs_write                   /* write */
};

static struct smavlink_in_channel g_in_ch = {
    SMAVLINK_UART,              /* read_delegate */
    gcs_read                    /* read */
};

static struct smavlink_system g_sys = {
    1,                          /* sysid */
    0,                          /* compid */
};

/*
 * Main Task
 */

static xTaskHandle g_main_task;
static xTaskHandle g_receive_task;

static void send_heartbeat(void)
{
    struct heartbeat_msg msg;

    msg.custom_mode = 0;
    msg.mavtype = 2;            /* quad rotor */
    msg.autopilot = 0;          /* generic autopilot */
    msg.base_mode = 208;        /* stabilize/armed */
    msg.system_status = 4;      /* active */

    smavlink_send_heartbeat(&msg, &g_out_ch, &g_sys);
}

static void receive_heartbeat(const uint8_t *buf)
{
    struct heartbeat_msg msg;
    smavlink_unpack_heartbeat(&msg, &buf);
    debug_printf("heartbeat: %lu %u %u %u %u\r\n",
                 msg.custom_mode, msg.mavtype, msg.autopilot,
                 msg.base_mode, msg.system_status);
}

static void receive_task(void *arg)
{
    uint8_t buf[255];
    uint8_t msg_id;

    for (;;) {
        if (smavlink_receive(&g_in_ch, &g_sys, &msg_id, buf, sizeof(buf))) {
            if (msg_id == 0) {
                receive_heartbeat(buf);
            } else {
                debug_printf("received msg id %u\r\n", msg_id);
            }
        } else {
            debug_puts("received bad message");
        }
    }
}

static void main_task(void *arg)
{
    portTickType last_wake;

    debug_uart_init();
    debug_puts("\r\nSMAVLink Heartbeat Test\r\n");

    last_wake = xTaskGetTickCount();

    for (;;) {
        send_heartbeat();
        vTaskDelayUntil(&last_wake, 1000);
    }
}

int main(void)
{
    xTaskCreate(main_task, (signed char *)"main", 1024, NULL, 0, &g_main_task);
    xTaskCreate(receive_task, (signed char *)"receive", 1024, NULL, 0, &g_receive_task);
    vTaskStartScheduler();

    for (;;)
        ;

    return 0;
}


#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <hwf4/usart.h>

#define HX_FBO 0x7e
#define HX_CEO 0x7c
#define HX_ESC(_x) (_x^0x20)

#define HX_STATE_IDLE    0
#define HX_STATE_FSTART  1
#define HX_STATE_DATA    2
#define HX_STATE_ESC     3

// FIFO status
#define BUF_FULL(_which) (((_which##_insert + 1) & _which##_mask) == (_which##_remove))
#define BUF_NOT_FULL(_which) (((_which##_insert + 1) & _which##_mask) != (_which##_remove))
#define BUF_EMPTY(_which) (_which##_insert == _which##_remove)
#define BUF_NOT_EMPTY(_which) (_which##_insert != _which##_remove)
#define BUF_USED(_which) ((_which##_insert - _which##_remove) & _which##_mask)
#define BUF_FREE(_which) ((_which##_remove - _which##_insert - 1) & _which##_mask)

// FIFO insert/remove operations
//
// Note that these are nominally interrupt-safe as only one of each
// buffer's end pointer is adjusted by either of interrupt or regular
// mode code.  This is violated if printing from interrupt context,
// which should generally be avoided when possible.
//
#define BUF_INSERT(_which) do {  \
        _which##_insert = ((_which##_insert+1) & _which##_mask); } while(0)
#define BUF_AT_INSERT(_which) &((_which##_buf)[_which##_insert])
#define BUF_REMOVE(_which) do { \
        _which##_remove = ((_which##_remove+1) & _which##_mask); } while(0)
#define BUF_AT_REMOVE(_which) &((_which##_buf)[_which##_remove])


struct frame {
    uint8_t len;
    uint8_t data[128];
};

struct frame_builder {
    uint8_t state;
    uint8_t pending;
    uint8_t offs;
};

#define NUM_RX_FRAMES 16

static struct frame_builder rx_fbuilder;
static struct frame         rx_buf[NUM_RX_FRAMES];
static const  uint16_t      rx_mask = NUM_RX_FRAMES - 1;
static uint16_t             rx_insert, rx_remove;

#define NUM_TX_FRAMES 16

static struct frame_builder tx_fbuilder;
static struct frame         tx_buf[NUM_TX_FRAMES];
static const  uint16_t      tx_mask = NUM_TX_FRAMES - 1;
static uint16_t             tx_insert, tx_remove;

static void init (void);
static void run (bool avail, uint8_t c);

#define UART usart2
// #define UART usart1

void test_task(void) {
    uint8_t buf = 0;
    usart_init(UART, 115200);
    usart_enable(UART);

    init();
    for(;;) {
        bool got = usart_read_timeout(UART, 0, &buf, 1);
        run (got, buf);
    }
}


static void frame_rx(uint8_t c);
static void frame_process (struct frame* fro, struct frame *to);
static void frame_tx ();

static void init_frame_builder(struct frame_builder *fb);

static void init (void) {
    init_frame_builder(&rx_fbuilder);
    rx_insert = 0;
    rx_remove = 0;

    init_frame_builder(&tx_fbuilder);
    tx_insert = 0;
    tx_remove = 0;
}

static void run (bool avail, uint8_t c) {
    if (avail) {
        frame_rx(c);
    }
    if (BUF_NOT_EMPTY(rx) && BUF_NOT_FULL(tx)) {
        frame_process(BUF_AT_REMOVE(rx), BUF_AT_INSERT(tx));
        BUF_REMOVE(rx);
        BUF_INSERT(tx);
    }
    frame_tx();
}

static bool frame_rx_aux (uint8_t c, struct frame* f, struct frame_builder* fb);
static void frame_rx (uint8_t c) {
    if (BUF_NOT_FULL(rx)) {
        struct frame *rx_frame = BUF_AT_INSERT(rx);
        bool complete = frame_rx_aux(c, rx_frame, &rx_fbuilder);
        if (complete) {
            BUF_INSERT(rx);
            init_frame_builder(&rx_fbuilder);
        }
    }
}

static bool frame_rx_aux (uint8_t c, struct frame* f, struct frame_builder* fb) {
    switch (fb->state) {
        case HX_STATE_IDLE:
            if (c == HX_FBO) {
                fb->state = HX_STATE_FSTART;
            }
            break;

        case HX_STATE_FSTART:
            if (c == 0) {
                fb->state = HX_STATE_DATA;
                fb->offs = 0;
            } else if (c == HX_FBO) {
                fb->state = HX_STATE_FSTART;
            } else {
                fb->state = HX_STATE_IDLE;
            };
            break;

        case HX_STATE_DATA:
            if (c == HX_CEO) {
                // Next byte will be escaped:
                fb->state = HX_STATE_ESC;
            } else if (c == HX_FBO) {
                // Complete Frame
                f->len = fb->offs;
                fb->state = HX_STATE_IDLE;
                return true;
            } else {
                uint8_t off = fb->offs;
                if (off < 128) {
                    // Ordinary byte onto the frame:
                    f->data[off] = c;
                    fb->offs = off + 1;
                } else {
                    fb->state = HX_STATE_IDLE;
                }
            }
            break;

        case HX_STATE_ESC:
            if (c == HX_FBO) {
                fb->state = HX_STATE_IDLE;
            } else {
                uint8_t off = fb->offs;
                if (off < 128) {
                    // Escaped byte onto the frame:
                    f->data[off] = HX_ESC(c);
                    fb->offs = off + 1;
                    fb->state = HX_STATE_DATA;
                } else {
                    fb->state = HX_STATE_IDLE;
                }
            }
            break;

        default:
            fb->state = HX_STATE_IDLE;
    }
    return false;
}

static void put(uint8_t c) {
    usart_write(UART, &c, 1);
}

static bool frame_tx_aux (struct frame *f, struct frame_builder *fb);
static void frame_tx () {
    if (BUF_NOT_EMPTY(tx)) {
        struct frame *f = BUF_AT_REMOVE(tx);
        bool complete = true;
        if (f->len <= 128) {
            complete = frame_tx_aux(f, &tx_fbuilder);
        }
        if (complete) {
            BUF_REMOVE(tx);
            init_frame_builder(&tx_fbuilder);
        }
    }
}

static bool frame_tx_aux (struct frame *f, struct frame_builder *fb) {
    switch (fb->state) {
        case HX_STATE_IDLE:
            put(HX_FBO);
            fb->state = HX_STATE_FSTART;
            break;
        case HX_STATE_FSTART:
            put(0);
            fb->state = HX_STATE_DATA;
            break;
        case HX_STATE_DATA:
            if (fb->offs >= f->len) {
                put(HX_FBO);
                return true;
            }
            uint8_t o = fb->offs;
            fb->offs = o + 1;
            uint8_t d = f->data[o];
            if (d == HX_FBO || d == HX_CEO) {
                put(HX_CEO);
                fb->pending = d;
                fb->state = HX_STATE_ESC;
            } else {
                put(d);
            }
            break;
        case HX_STATE_ESC:
            put(HX_ESC(fb->pending));
            fb->state = HX_STATE_DATA;
            break;
    }
    return false;
}

static void frame_process(struct frame *fro, struct frame *to) {
    to->len = fro->len;
    // this is a memcpy
    for(int i = 0; i < fro->len; i++) {
        to->data[i] = fro->data[i];
    }
    for(int i = 0; i < 10; i++) {
        uint8_t a = to->data[(3*i)+0];
        uint8_t b = to->data[(3*i)+1];
        to->data[(3*i)+2] = a + b;
    }
}

static void init_frame_builder(struct frame_builder *fb) {
    fb->state = HX_STATE_IDLE;
    fb->pending = 0;
    fb->offs = 0;
}

/*
 * gpio.c --- STM32F4 GPIO driver.
 *
 * Copyright (C) 2012, Galois, Inc.
 * All Rights Reserved.
 *
 * This software is released under the "BSD3" license.  Read the file
 * "LICENSE" for more information.
 */


#include "stm32f4xx.h"

#include "rcc.h" 
#include "gpio.h"


/* External GPIO Interface ****************************************************/

struct gpio {
    GPIO_TypeDef *dev;
    enum RCCDevice rcc_dev;
};


void gpio_enable(struct gpio *gpio) {
    rcc_enable(gpio->rcc_dev);
}

void gpio_reset_moder(struct gpio *gpio, uint32_t val) {
    gpio->dev->MODER &= ~val;
}

void gpio_update_moder(struct gpio *gpio, uint32_t val) {
    gpio->dev->MODER |= val;
}

void gpio_reset_otyper(struct gpio *gpio, uint32_t val) {
    gpio->dev->OTYPER &= ~val;
}

void gpio_update_otyper(struct gpio *gpio, uint32_t val) {
    gpio->dev->OTYPER |= val;
}

void gpio_reset_ospeedr(struct gpio *gpio, uint32_t val) {
    gpio->dev->OSPEEDR &= ~val;
}

void gpio_update_ospeedr(struct gpio *gpio, uint32_t val) {
    gpio->dev->OSPEEDR |= val;
}

void gpio_reset_pupdr(struct gpio *gpio, uint32_t val) {
    gpio->dev->PUPDR &= ~val;
}

void gpio_update_pupdr(struct gpio *gpio, uint32_t val) {
    gpio->dev->PUPDR |= val;
}

uint32_t gpio_get_idr(struct gpio *gpio) {
    return gpio->dev->IDR;
}

void gpio_modify_odr(struct gpio *gpio, uint32_t val) {
    gpio->dev->ODR |= val;
}

void gpio_reset_afr(struct gpio *gpio, int lh, uint32_t val) {
    gpio->dev->AFR[lh] &= ~val;
}

void gpio_modify_afr(struct gpio *gpio, int lh, uint32_t val) {
    gpio->dev->AFR[lh] |= val;
}

void gpio_set_bsrrl(struct gpio *gpio, uint32_t val) {
    gpio->dev->BSRRL = val;
}

void gpio_set_bsrrh(struct gpio *gpio, uint32_t val) {
    gpio->dev->BSRRH = val;
}



/* External GPIO Pin Interface ************************************************/

struct pin {
    struct gpio *bank;
    uint8_t pin_num;
};

void pin_enable(struct pin *pin) {
    gpio_enable(pin->bank);
}

void pin_set_mode(struct pin *pin, enum pin_mode mode) {
    gpio_reset_moder(pin->bank, GPIO_MODER_MODER0 << (pin->pin_num * 2));

    uint32_t mask = (uint32_t)mode << (pin->pin_num * 2);
    gpio_update_moder(pin->bank, mask);
}

void pin_set_otype(struct pin *pin, enum pin_type type) {
    gpio_reset_otyper(pin->bank, GPIO_OTYPER_OT_0 << pin->pin_num);

    uint32_t mask = (uint32_t)type << pin->pin_num;
    gpio_update_otyper(pin->bank, mask);
}

void pin_set_ospeed(struct pin *pin, enum pin_speed speed) {
    gpio_reset_ospeedr(pin->bank, GPIO_OSPEEDER_OSPEEDR0 << (pin->pin_num * 2));

    uint32_t mask = (uint32_t)speed << (pin->pin_num * 2);
    gpio_update_ospeedr(pin->bank, mask);
}

void pin_set_pupd(struct pin *pin, enum pin_pupd pupd) {
    gpio_reset_pupdr(pin->bank, GPIO_PUPDR_PUPDR0 << pin->pin_num);

    uint32_t mask = (uint32_t)pupd << pin->pin_num;
    gpio_update_pupdr(pin->bank, mask);
}

void pin_set_af(struct pin *pin, enum pin_af af) {
    int reg = (pin->pin_num & 0x7) * 4;    // [0,7]
    int lh  = (pin->pin_num >> 0x3) & 0x1; // [0,1]

    gpio_reset_afr(pin->bank, lh, 0xf << reg);

    uint32_t mask = ((uint32_t)af & 0xf) << reg;
    gpio_modify_afr(pin->bank, lh, mask);
}


void pin_reset(struct pin *pin) {
    gpio_set_bsrrh(pin->bank, 0x1 << pin->pin_num);
}

void pin_set(struct pin *pin) {
    gpio_set_bsrrl(pin->bank, 0x1 << pin->pin_num);
}

void pin_toggle(struct pin *pin) {
    uint16_t odr = pin->bank->dev->ODR;

    if (odr & (1 << pin->pin_num))
        pin_reset(pin);
    else
        pin_set(pin);
}

bool pin_read(struct pin *pin) {
    uint16_t idr = gpio_get_idr(pin->bank);
    return (idr & (1 << pin->pin_num)) != 0;
}

/* GPIO Bank Definition *******************************************************/

/* cpp expanded */

struct gpio _gpio_a = { .dev = GPIOA, .rcc_dev = RCCDEV_GPIOA }; struct pin _pin_a0 = { .bank = & _gpio_a, .pin_num = 0 }; struct pin _pin_a1 = { .bank = & _gpio_a, .pin_num = 1 }; struct pin _pin_a2 = { .bank = & _gpio_a, .pin_num = 2 }; struct pin _pin_a3 = { .bank = & _gpio_a, .pin_num = 3 }; struct pin _pin_a4 = { .bank = & _gpio_a, .pin_num = 4 }; struct pin _pin_a5 = { .bank = & _gpio_a, .pin_num = 5 }; struct pin _pin_a6 = { .bank = & _gpio_a, .pin_num = 6 }; struct pin _pin_a7 = { .bank = & _gpio_a, .pin_num = 7 }; struct pin _pin_a8 = { .bank = & _gpio_a, .pin_num = 8 }; struct pin _pin_a9 = { .bank = & _gpio_a, .pin_num = 9 }; struct pin _pin_a10 = { .bank = & _gpio_a, .pin_num = 10 }; struct pin _pin_a11 = { .bank = & _gpio_a, .pin_num = 11 }; struct pin _pin_a12 = { .bank = & _gpio_a, .pin_num = 12 }; struct pin _pin_a13 = { .bank = & _gpio_a, .pin_num = 13 }; struct pin _pin_a14 = { .bank = & _gpio_a, .pin_num = 14 }; struct pin _pin_a15 = { .bank = & _gpio_a, .pin_num = 15 };
struct gpio _gpio_b = { .dev = GPIOB, .rcc_dev = RCCDEV_GPIOB }; struct pin _pin_b0 = { .bank = & _gpio_b, .pin_num = 0 }; struct pin _pin_b1 = { .bank = & _gpio_b, .pin_num = 1 }; struct pin _pin_b2 = { .bank = & _gpio_b, .pin_num = 2 }; struct pin _pin_b3 = { .bank = & _gpio_b, .pin_num = 3 }; struct pin _pin_b4 = { .bank = & _gpio_b, .pin_num = 4 }; struct pin _pin_b5 = { .bank = & _gpio_b, .pin_num = 5 }; struct pin _pin_b6 = { .bank = & _gpio_b, .pin_num = 6 }; struct pin _pin_b7 = { .bank = & _gpio_b, .pin_num = 7 }; struct pin _pin_b8 = { .bank = & _gpio_b, .pin_num = 8 }; struct pin _pin_b9 = { .bank = & _gpio_b, .pin_num = 9 }; struct pin _pin_b10 = { .bank = & _gpio_b, .pin_num = 10 }; struct pin _pin_b11 = { .bank = & _gpio_b, .pin_num = 11 }; struct pin _pin_b12 = { .bank = & _gpio_b, .pin_num = 12 }; struct pin _pin_b13 = { .bank = & _gpio_b, .pin_num = 13 }; struct pin _pin_b14 = { .bank = & _gpio_b, .pin_num = 14 }; struct pin _pin_b15 = { .bank = & _gpio_b, .pin_num = 15 };
struct gpio _gpio_c = { .dev = GPIOC, .rcc_dev = RCCDEV_GPIOC }; struct pin _pin_c0 = { .bank = & _gpio_c, .pin_num = 0 }; struct pin _pin_c1 = { .bank = & _gpio_c, .pin_num = 1 }; struct pin _pin_c2 = { .bank = & _gpio_c, .pin_num = 2 }; struct pin _pin_c3 = { .bank = & _gpio_c, .pin_num = 3 }; struct pin _pin_c4 = { .bank = & _gpio_c, .pin_num = 4 }; struct pin _pin_c5 = { .bank = & _gpio_c, .pin_num = 5 }; struct pin _pin_c6 = { .bank = & _gpio_c, .pin_num = 6 }; struct pin _pin_c7 = { .bank = & _gpio_c, .pin_num = 7 }; struct pin _pin_c8 = { .bank = & _gpio_c, .pin_num = 8 }; struct pin _pin_c9 = { .bank = & _gpio_c, .pin_num = 9 }; struct pin _pin_c10 = { .bank = & _gpio_c, .pin_num = 10 }; struct pin _pin_c11 = { .bank = & _gpio_c, .pin_num = 11 }; struct pin _pin_c12 = { .bank = & _gpio_c, .pin_num = 12 }; struct pin _pin_c13 = { .bank = & _gpio_c, .pin_num = 13 }; struct pin _pin_c14 = { .bank = & _gpio_c, .pin_num = 14 }; struct pin _pin_c15 = { .bank = & _gpio_c, .pin_num = 15 };
struct gpio _gpio_d = { .dev = GPIOD, .rcc_dev = RCCDEV_GPIOD }; struct pin _pin_d0 = { .bank = & _gpio_d, .pin_num = 0 }; struct pin _pin_d1 = { .bank = & _gpio_d, .pin_num = 1 }; struct pin _pin_d2 = { .bank = & _gpio_d, .pin_num = 2 }; struct pin _pin_d3 = { .bank = & _gpio_d, .pin_num = 3 }; struct pin _pin_d4 = { .bank = & _gpio_d, .pin_num = 4 }; struct pin _pin_d5 = { .bank = & _gpio_d, .pin_num = 5 }; struct pin _pin_d6 = { .bank = & _gpio_d, .pin_num = 6 }; struct pin _pin_d7 = { .bank = & _gpio_d, .pin_num = 7 }; struct pin _pin_d8 = { .bank = & _gpio_d, .pin_num = 8 }; struct pin _pin_d9 = { .bank = & _gpio_d, .pin_num = 9 }; struct pin _pin_d10 = { .bank = & _gpio_d, .pin_num = 10 }; struct pin _pin_d11 = { .bank = & _gpio_d, .pin_num = 11 }; struct pin _pin_d12 = { .bank = & _gpio_d, .pin_num = 12 }; struct pin _pin_d13 = { .bank = & _gpio_d, .pin_num = 13 }; struct pin _pin_d14 = { .bank = & _gpio_d, .pin_num = 14 }; struct pin _pin_d15 = { .bank = & _gpio_d, .pin_num = 15 };
struct gpio _gpio_e = { .dev = GPIOE, .rcc_dev = RCCDEV_GPIOE }; struct pin _pin_e0 = { .bank = & _gpio_e, .pin_num = 0 }; struct pin _pin_e1 = { .bank = & _gpio_e, .pin_num = 1 }; struct pin _pin_e2 = { .bank = & _gpio_e, .pin_num = 2 }; struct pin _pin_e3 = { .bank = & _gpio_e, .pin_num = 3 }; struct pin _pin_e4 = { .bank = & _gpio_e, .pin_num = 4 }; struct pin _pin_e5 = { .bank = & _gpio_e, .pin_num = 5 }; struct pin _pin_e6 = { .bank = & _gpio_e, .pin_num = 6 }; struct pin _pin_e7 = { .bank = & _gpio_e, .pin_num = 7 }; struct pin _pin_e8 = { .bank = & _gpio_e, .pin_num = 8 }; struct pin _pin_e9 = { .bank = & _gpio_e, .pin_num = 9 }; struct pin _pin_e10 = { .bank = & _gpio_e, .pin_num = 10 }; struct pin _pin_e11 = { .bank = & _gpio_e, .pin_num = 11 }; struct pin _pin_e12 = { .bank = & _gpio_e, .pin_num = 12 }; struct pin _pin_e13 = { .bank = & _gpio_e, .pin_num = 13 }; struct pin _pin_e14 = { .bank = & _gpio_e, .pin_num = 14 }; struct pin _pin_e15 = { .bank = & _gpio_e, .pin_num = 15 };
struct gpio _gpio_f = { .dev = GPIOF, .rcc_dev = RCCDEV_GPIOF }; struct pin _pin_f0 = { .bank = & _gpio_f, .pin_num = 0 }; struct pin _pin_f1 = { .bank = & _gpio_f, .pin_num = 1 }; struct pin _pin_f2 = { .bank = & _gpio_f, .pin_num = 2 }; struct pin _pin_f3 = { .bank = & _gpio_f, .pin_num = 3 }; struct pin _pin_f4 = { .bank = & _gpio_f, .pin_num = 4 }; struct pin _pin_f5 = { .bank = & _gpio_f, .pin_num = 5 }; struct pin _pin_f6 = { .bank = & _gpio_f, .pin_num = 6 }; struct pin _pin_f7 = { .bank = & _gpio_f, .pin_num = 7 }; struct pin _pin_f8 = { .bank = & _gpio_f, .pin_num = 8 }; struct pin _pin_f9 = { .bank = & _gpio_f, .pin_num = 9 }; struct pin _pin_f10 = { .bank = & _gpio_f, .pin_num = 10 }; struct pin _pin_f11 = { .bank = & _gpio_f, .pin_num = 11 }; struct pin _pin_f12 = { .bank = & _gpio_f, .pin_num = 12 }; struct pin _pin_f13 = { .bank = & _gpio_f, .pin_num = 13 }; struct pin _pin_f14 = { .bank = & _gpio_f, .pin_num = 14 }; struct pin _pin_f15 = { .bank = & _gpio_f, .pin_num = 15 };
struct gpio _gpio_g = { .dev = GPIOG, .rcc_dev = RCCDEV_GPIOG }; struct pin _pin_g0 = { .bank = & _gpio_g, .pin_num = 0 }; struct pin _pin_g1 = { .bank = & _gpio_g, .pin_num = 1 }; struct pin _pin_g2 = { .bank = & _gpio_g, .pin_num = 2 }; struct pin _pin_g3 = { .bank = & _gpio_g, .pin_num = 3 }; struct pin _pin_g4 = { .bank = & _gpio_g, .pin_num = 4 }; struct pin _pin_g5 = { .bank = & _gpio_g, .pin_num = 5 }; struct pin _pin_g6 = { .bank = & _gpio_g, .pin_num = 6 }; struct pin _pin_g7 = { .bank = & _gpio_g, .pin_num = 7 }; struct pin _pin_g8 = { .bank = & _gpio_g, .pin_num = 8 }; struct pin _pin_g9 = { .bank = & _gpio_g, .pin_num = 9 }; struct pin _pin_g10 = { .bank = & _gpio_g, .pin_num = 10 }; struct pin _pin_g11 = { .bank = & _gpio_g, .pin_num = 11 }; struct pin _pin_g12 = { .bank = & _gpio_g, .pin_num = 12 }; struct pin _pin_g13 = { .bank = & _gpio_g, .pin_num = 13 }; struct pin _pin_g14 = { .bank = & _gpio_g, .pin_num = 14 }; struct pin _pin_g15 = { .bank = & _gpio_g, .pin_num = 15 };
struct gpio _gpio_h = { .dev = GPIOH, .rcc_dev = RCCDEV_GPIOH }; struct pin _pin_h0 = { .bank = & _gpio_h, .pin_num = 0 }; struct pin _pin_h1 = { .bank = & _gpio_h, .pin_num = 1 }; struct pin _pin_h2 = { .bank = & _gpio_h, .pin_num = 2 }; struct pin _pin_h3 = { .bank = & _gpio_h, .pin_num = 3 }; struct pin _pin_h4 = { .bank = & _gpio_h, .pin_num = 4 }; struct pin _pin_h5 = { .bank = & _gpio_h, .pin_num = 5 }; struct pin _pin_h6 = { .bank = & _gpio_h, .pin_num = 6 }; struct pin _pin_h7 = { .bank = & _gpio_h, .pin_num = 7 }; struct pin _pin_h8 = { .bank = & _gpio_h, .pin_num = 8 }; struct pin _pin_h9 = { .bank = & _gpio_h, .pin_num = 9 }; struct pin _pin_h10 = { .bank = & _gpio_h, .pin_num = 10 }; struct pin _pin_h11 = { .bank = & _gpio_h, .pin_num = 11 }; struct pin _pin_h12 = { .bank = & _gpio_h, .pin_num = 12 }; struct pin _pin_h13 = { .bank = & _gpio_h, .pin_num = 13 }; struct pin _pin_h14 = { .bank = & _gpio_h, .pin_num = 14 }; struct pin _pin_h15 = { .bank = & _gpio_h, .pin_num = 15 };
struct gpio _gpio_i = { .dev = GPIOI, .rcc_dev = RCCDEV_GPIOI }; struct pin _pin_i0 = { .bank = & _gpio_i, .pin_num = 0 }; struct pin _pin_i1 = { .bank = & _gpio_i, .pin_num = 1 }; struct pin _pin_i2 = { .bank = & _gpio_i, .pin_num = 2 }; struct pin _pin_i3 = { .bank = & _gpio_i, .pin_num = 3 }; struct pin _pin_i4 = { .bank = & _gpio_i, .pin_num = 4 }; struct pin _pin_i5 = { .bank = & _gpio_i, .pin_num = 5 }; struct pin _pin_i6 = { .bank = & _gpio_i, .pin_num = 6 }; struct pin _pin_i7 = { .bank = & _gpio_i, .pin_num = 7 }; struct pin _pin_i8 = { .bank = & _gpio_i, .pin_num = 8 }; struct pin _pin_i9 = { .bank = & _gpio_i, .pin_num = 9 }; struct pin _pin_i10 = { .bank = & _gpio_i, .pin_num = 10 }; struct pin _pin_i11 = { .bank = & _gpio_i, .pin_num = 11 }; struct pin _pin_i12 = { .bank = & _gpio_i, .pin_num = 12 }; struct pin _pin_i13 = { .bank = & _gpio_i, .pin_num = 13 }; struct pin _pin_i14 = { .bank = & _gpio_i, .pin_num = 14 }; struct pin _pin_i15 = { .bank = & _gpio_i, .pin_num = 15 };

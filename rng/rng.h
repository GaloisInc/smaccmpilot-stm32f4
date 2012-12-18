#ifndef __RNG_H
#define __RNG_H

#include <stdint.h>
#include <stdbool.h>

void rng_init(void);

bool rng_word(portBASE_TYPE timeout, uint32_t *word);

void rng_irq_handler(void);

#endif /* __RNG_H */

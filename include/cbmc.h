#ifndef CBMC_H
#define CBMC_H

#define REQUIRES(arg) __CPROVER_assume(arg)
#define PROVIDES(arg) __CPROVER_assert(arg)

#endif // CBMC_H

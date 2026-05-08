/* SPDX-License-Identifier: MIT */
/* Copyright (c) 2025 Jorge Luis Galvez Vallejo */
/* Returns a 64-bit seed for the Fortran RNG, mixing wall-clock nanoseconds
   with the process id so successive runs (and concurrent runs) differ. */

#include <stdint.h>
#include <time.h>
#include <unistd.h>

int64_t pic_knowledge_seed(void)
{
   struct timespec ts;
   clock_gettime(CLOCK_REALTIME, &ts);
   uint64_t s = (uint64_t)ts.tv_sec * 1000000000ull + (uint64_t)ts.tv_nsec;
   s ^= ((uint64_t)getpid()) << 16;
   return (int64_t)s;
}

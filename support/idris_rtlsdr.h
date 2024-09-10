#ifndef __IDRIS_RTLSDR
#define __IDRIS_RTLSDR

#include <stddef.h>
#include <stdint.h>

const void * idris_rtlsdr_open(uint32_t index, uint32_t *ret);
int idris_rtlsdr_read_ptr_ref(const int *p, size_t off);

#endif /* __IDRIS_RTLSDR */

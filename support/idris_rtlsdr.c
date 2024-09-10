#include <stddef.h>
#include <stdint.h>

#include <rtl-sdr.h>

#include "idris_rtlsdr.h"

const void * idris_rtlsdr_open(uint32_t index, uint32_t *ret)
{
	rtlsdr_dev_t *dev;
	*ret = rtlsdr_open(&dev, index);
	return dev;
}

int idris_rtlsdr_read_ptr_ref(const int *p, size_t off)
{
	return p[off];
}

uint8_t idris_rtlsdr_ref(uint8_t *ptr, int ofs)
{
	return ptr[ofs];
}

module Bindings.RtlSdr.Raw.Buffer

import Bindings.RtlSdr.Device

import Data.Buffer

%default total

-- RTLSDR_API int rtlsdr_read_sync(rtlsdr_dev_t *dev, void *buf, int len, int *n_read);
export
%foreign (librtlsdr "read_sync")
read_sync: Ptr RtlSdrHandle -> Buffer -> Int -> Ptr Int -> PrimIO Int

-- typedef void(*rtlsdr_read_async_cb_t)(unsigned char *buf, uint32_t len, void *ctx);
ReadAsyncFnPrim = Ptr Bits8 -> Int -> AnyPtr -> PrimIO ()

-- RTLSDR_API int rtlsdr_read_async(rtlsdr_dev_t *dev, rtlsdr_read_async_cb_t cb, void *ctx, uint32_t buf_num, uint32_t buf_len);
export
%foreign (librtlsdr "read_async")
read_async: Ptr RtlSdrHandle -> ReadAsyncFnPrim -> AnyPtr -> Int -> Int -> PrimIO Int

-- RTLSDR_API int rtlsdr_cancel_async(rtlsdr_dev_t *dev);
export
%foreign (librtlsdr "cancel_async")
cancel_async: Ptr RtlSdrHandle -> PrimIO Int

-- RTLSDR_API int rtlsdr_reset_buffer(rtlsdr_dev_t *dev);
export
%foreign (librtlsdr "reset_buffer")
reset_buffer: Ptr RtlSdrHandle -> PrimIO Int

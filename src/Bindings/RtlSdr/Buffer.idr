module Bindings.RtlSdr.Buffer

import Bindings.RtlSdr.Device

-- RTLSDR_API int rtlsdr_reset_buffer(rtlsdr_dev_t *dev);
export
%foreign (librtlsdr "reset_buffer")
reset_buffer: RtlSdrHandle -> Int

-- RTLSDR_API int rtlsdr_read_sync(rtlsdr_dev_t *dev, void *buf, int len, int *n_read);
export
%foreign (librtlsdr "read_sync")
read_sync: RtlSdrHandle -> RtlSdrHandle -> Int -> Ptr Int -> Int

-- typedef void(*rtlsdr_read_async_cb_t)(unsigned char *buf, uint32_t len, void *ctx);
ReadAsyncFn = String -> Int -> RtlSdrHandle

-- RTLSDR_API int rtlsdr_wait_async(rtlsdr_dev_t *dev, rtlsdr_read_async_cb_t cb, void *ctx);
export
%foreign (librtlsdr "wait_async")
wait_async: RtlSdrHandle -> ReadAsyncFn -> RtlSdrHandle -> Int

-- RTLSDR_API int rtlsdr_read_async(rtlsdr_dev_t *dev, rtlsdr_read_async_cb_t cb, void *ctx, uint32_t buf_num, uint32_t buf_len);
export
%foreign (librtlsdr "read_async")
read_async: RtlSdrHandle -> ReadAsyncFn -> RtlSdrHandle -> Int -> Int -> Int

-- RTLSDR_API int rtlsdr_cancel_async(rtlsdr_dev_t *dev);
export
%foreign (librtlsdr "cancel_async")
cancel_async: RtlSdrHandle -> Int

module Bindings.RtlSdr.Raw.Device

import System.FFI

%default total

-- Device handle data type.
export
-- data RtlSdrHandle = MkDevice AnyPtr
data RtlSdrHandle : Type where [external]

-- librtlsdr binding helper.
public export
librtlsdr : String -> String
librtlsdr fn = "C:" ++ "rtlsdr_" ++ fn ++ ",librtlsdr"

--RTLSDR_API int rtlsdr_get_device_usb_strings(uint32_t index,
--					     char *manufact,
--					     char *product,
--					     char *serial);
export
%foreign (librtlsdr "get_device_usb_strings")
get_device_usb_strings: Int -> Ptr String -> Ptr String -> Ptr String -> Int

-- RTLSDR_API uint32_t rtlsdr_get_device_count(void);
export
%foreign (librtlsdr "get_device_count")
get_device_count: Int

-- RTLSDR_API const char* rtlsdr_get_device_name(uint32_t index);
export
%foreign (librtlsdr "get_device_name")
get_device_name: Int -> String

-- RTLSDR_API int rtlsdr_open(rtlsdr_dev_t **dev, uint32_t index);
export
%foreign (librtlsdr "open")
open_prim: Ptr RtlSdrHandle -> Int -> PrimIO Int

-- RTLSDR_API int rtlsdr_close(rtlsdr_dev_t *dev);
export
%foreign (librtlsdr "close")
close: Ptr RtlSdrHandle -> PrimIO Int

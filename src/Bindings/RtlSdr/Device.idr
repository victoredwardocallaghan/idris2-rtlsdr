module Bindings.RtlSdr.Device

import Bindings.RtlSdr.Raw.Device
import Bindings.RtlSdr.Error

import public Bindings.RtlSdr.Raw.Device
--import public Bindings.RtlSdr.Raw.Device.get_device_count
--import public Bindings.RtlSdr.Raw.Device.get_device_name

import System.FFI

%default total


-- wrapper C func helper.
public export
idris_rtlsdr : String -> String
idris_rtlsdr fn = "C:" ++ "idris_rtlsdr_" ++ fn ++ ",rtlsdr-idris"

-- XXX support/ runtime wraps
%foreign (idris_rtlsdr "open")
idris_rtlsdr_open : Int -> Ptr Int -> PrimIO AnyPtr

-- XXX support/.. int read_refint(int *p);
export
%foreign (idris_rtlsdr "read_refint")
idris_rtlsdr_read_refint : Ptr Int -> Int

export
rtlsdr_open : Int -> IO (Maybe (Ptr RtlSdrHandle))
rtlsdr_open idx = do
  v <- prim__castPtr <$> malloc 4 -- ret
  -- const void * idris_rtlsdr_open(uint32_t index, uint32_t *ret);
  p <- fromPrim $ idris_rtlsdr_open idx v
  let ret = idris_rtlsdr_read_refint v
  free $ prim__forgetPtr v
  io_pure $ if ret == 0 then Just (prim__castPtr p) else Nothing


export
rtlsdr_close : Ptr RtlSdrHandle -> IO (Either RTLSDR_ERROR ())
rtlsdr_close h = do
  r <- fromPrim $ close h
  io_pure $ if r == 0 then Right () else Left RtlSdrError

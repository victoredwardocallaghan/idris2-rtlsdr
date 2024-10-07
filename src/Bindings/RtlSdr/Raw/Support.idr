module Bindings.RtlSdr.Raw.Support

import Data.IOArray
import System.FFI

%default total

-- wrapper C func helper.
idris_rtlsdr : String -> String
idris_rtlsdr fn = "C:" ++ "idris_rtlsdr_" ++ fn ++ ",rtlsdr-idris"

-- XXX support/ runtime wraps
export
%foreign (idris_rtlsdr "open")
idris_rtlsdr_open : Int -> Ptr Int -> PrimIO AnyPtr

-- XXX support/.. int read_ptr_ref(int *p, int off);
%foreign (idris_rtlsdr "read_ptr_ref")
idris_rtlsdr_read_ptr_ref : Ptr Int -> Int -> Int

export
readBufPtr : Ptr Int -> Int -> IO (List Int)
readBufPtr p n = for [0..n-1] $ \i => io_pure $ idris_rtlsdr_read_ptr_ref p i

%foreign (idris_rtlsdr "read_ptr_ref_")
idris_rtlsdr_read_ptr_ref' : Ptr Bits8 -> Int -> Bits8

export
readBufPtr' : Ptr Bits8 -> Int -> IO (IOArray Bits8)
readBufPtr' p n =
  let
    readBufPtr'' : Ptr Bits8 -> Int -> IOArray Bits8 -> IO (IOArray Bits8)
    readBufPtr'' _ 0 ioa = io_pure ioa
    readBufPtr'' p n ioa = do
      ignore $ writeArray ioa (n-1) $ idris_rtlsdr_read_ptr_ref' p (n-1)
      io_pure ioa
  in
    readBufPtr'' p n =<< newArray n

export
peekInt : Ptr Int -> Int
peekInt p = idris_rtlsdr_read_ptr_ref p 0

export
%foreign (idris_rtlsdr "getstring")
idris_rtlsdr_getstring : Ptr String -> String

module Bindings.RtlSdr.Buffer

import public Bindings.RtlSdr.Raw.Buffer
import Bindings.RtlSdr.Device
import Bindings.RtlSdr.Error
import Bindings.RtlSdr.Raw.Support

import System.FFI

%default total

export
resetBuffer : Ptr RtlSdrHandle -> IO (Either RTLSDR_ERROR ())
resetBuffer h = do
  r <- fromPrim $ reset_buffer h
  io_pure $ if r == 0 then Right () else Left RtlSdrError

export
readSync : Ptr RtlSdrHandle -> AnyPtr -> Int -> IO (Either RTLSDR_ERROR Int)
readSync h b l = do
  v <- prim__castPtr <$> malloc 4 -- n_read
  r <- fromPrim $ read_sync h b l v
  let nr = peekInt v
  free $ prim__forgetPtr v
  io_pure $ if r == 0 then Right nr else Left RtlSdrError

public export
ReadAsyncFn : Type
ReadAsyncFn = AnyPtr -> List Bits8 -> IO ()

private
readBuf : AnyPtr -> Int -> IO (List Bits8)
readBuf buf len =
  for [0..len-1] $ \ofs =>
    fromPrim $ peekBits8 buf ofs

export
readAsync : Ptr RtlSdrHandle -> ReadAsyncFn -> AnyPtr -> Int -> Int -> IO (Either RTLSDR_ERROR ())
readAsync h cbIO ctx bn bl = do
  let cbPrim = \bufPtr, bufLen, ctxPtr => toPrim $ do
        cbIO ctxPtr =<< readBuf bufPtr bufLen
  r <- fromPrim $ read_async h cbPrim ctx bn bl
  io_pure $ if r == 0 then Right () else Left RtlSdrError

export
cancelAsync : Ptr RtlSdrHandle -> IO (Either RTLSDR_ERROR ())
cancelAsync h = do
  r <- fromPrim $ cancel_async h
  io_pure $ if r == 0 then Right () else Left RtlSdrError

module Bindings.RtlSdr.Gain

import Bindings.RtlSdr.Device
import Bindings.RtlSdr.Error
import Bindings.RtlSdr.Raw.Gain
import Bindings.RtlSdr.Raw.Support

import System.FFI

%default total

export
getTunerGains : Ptr RtlSdrHandle -> IO (Either RTLSDR_ERROR (List Int))
getTunerGains h = do
  n <- fromPrim $ get_tuner_gains h (prim__castPtr prim__getNullAnyPtr)
  if n < 0 then do
             io_pure $ Left RtlSdrError
           else do
             v <- prim__castPtr <$> malloc n
             _ <- fromPrim $ get_tuner_gains h v
             g <- readBufPtr v n
             free $ prim__forgetPtr v
             io_pure $ Right g

export
setTunerGain : Ptr RtlSdrHandle -> Int -> IO (Either RTLSDR_ERROR ())
setTunerGain h g = do
  r <- fromPrim $ set_tuner_gain h g
  io_pure $ if r == 0 then Right () else Left RtlSdrError

export
setTunerBandwidth : Ptr RtlSdrHandle -> Int -> IO (Either RTLSDR_ERROR ())
setTunerBandwidth h bw = do
  r <- fromPrim $ set_tuner_bandwidth h bw
  io_pure $ if r == 0 then Right () else Left RtlSdrError

export
getTunerGain : Ptr RtlSdrHandle -> IO (Either RTLSDR_ERROR Int)
getTunerGain h = do
  r <- fromPrim $ get_tuner_gain h
  io_pure $ if r == 0 then Left RtlSdrError else Right r

export
setTunerIFGain : Ptr RtlSdrHandle -> Int -> Int -> IO (Either RTLSDR_ERROR ())
setTunerIFGain h s g = do
  r <- fromPrim $ set_tuner_if_gain h s g
  io_pure $ if r == 0 then Right () else Left RtlSdrError

export
setTunerGainMode : Ptr RtlSdrHandle -> Bool -> IO (Either RTLSDR_ERROR ())
setTunerGainMode h t = do
  r <- fromPrim $ set_tuner_gain_mode h (if t then 1 else 0)
  io_pure $ if r == 0 then Right () else Left RtlSdrError

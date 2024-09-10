module Bindings.RtlSdr.Frequency

import Bindings.RtlSdr.Device
import Bindings.RtlSdr.Error
import Bindings.RtlSdr.Raw.Frequency
import Bindings.RtlSdr.Raw.Support

import System.FFI

%default total

export
setXTALFreq : Ptr RtlSdrHandle -> Int -> Int -> IO (Either RTLSDR_ERROR ())
setXTALFreq h f tf = do
  r <- fromPrim $ set_xtal_freq h f tf
  io_pure $ if r == 0 then Right () else Left RtlSdrError

export
getXTALFreq : Ptr RtlSdrHandle -> IO (Either RTLSDR_ERROR (Int, Int))
getXTALFreq h = do
  rtl_freq   <- prim__castPtr <$> malloc 4 -- rtl_freq frequency value used to clock the RTL2832 in Hz
  tuner_freq <- prim__castPtr <$> malloc 4 -- tuner_freq frequency value used to clock the tuner IC in Hz
  r <- fromPrim $ get_xtal_freq h rtl_freq tuner_freq
  let v = (peekInt rtl_freq, peekInt tuner_freq)
  free $ prim__forgetPtr rtl_freq
  free $ prim__forgetPtr tuner_freq
  io_pure $ if r == 0 then Right v else Left RtlSdrError


export
setCenterFreq : Ptr RtlSdrHandle -> Int -> IO Int
setCenterFreq h f = fromPrim $ set_center_freq h f

export
getCenterFreq : Ptr RtlSdrHandle -> IO (Either RTLSDR_ERROR Int)
getCenterFreq h = do
  r <- fromPrim $ get_center_freq h
  io_pure $ if r == 0 then Left RtlSdrError else Right r

export
setFreqCorrection : Ptr RtlSdrHandle -> Int -> IO (Either RTLSDR_ERROR ())
setFreqCorrection h ppm = do
  r <- fromPrim $ set_freq_correction h ppm
  io_pure $ if r == 0 then Right () else Left RtlSdrError

export
getFreqCorrection : Ptr RtlSdrHandle -> IO Int
getFreqCorrection h = fromPrim $ get_freq_correction h

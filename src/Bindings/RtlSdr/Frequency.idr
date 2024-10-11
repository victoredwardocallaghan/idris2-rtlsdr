module Bindings.RtlSdr.Frequency

import Bindings.RtlSdr.Device
import Bindings.RtlSdr.Error
import Bindings.RtlSdr.Raw.Frequency
import Bindings.RtlSdr.Raw.Support

import System.FFI

%default total

||| Set crystal oscillator frequencies used for the RTL2832 and the tuner IC.
|||
||| Usually both ICs use the same clock. Changing the clock may make sense if
||| you are applying an external clock to the tuner or to compensate the
||| frequency (and samplerate) error caused by the original (cheap) crystal.
|||
||| NOTE: Call this function only if you fully understand the implications.
|||
||| @h is the device handle
||| @f  is the frequency used to clock the RTL2832 in Hz
||| @tf is the frequency value used to clock the tuner IC in Hz
export
setXTALFreq : Ptr RtlSdrHandle -> Int -> Int -> IO (Either RTLSDR_ERROR ())
setXTALFreq h f tf = do
  r <- fromPrim $ set_xtal_freq h f tf
  io_pure $ if r == 0 then Right () else Left RtlSdrError

||| Get crystal oscillator frequencies used for the RTL2832 and the tuner IC.
|||
||| Usually both ICs use the same clock.
|||
||| This will return a tuple of (rtl_freq, tuner_freq) where,
|||   rtl_freq frequency value used to clock the RTL2832 in Hz
|||   tuner_freq frequency value used to clock the tuner IC in Hz
|||
||| @h is the device handle
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


||| Set actual frequency the device is tuned to.
|||
||| @h is the device handle
||| @f is the frequency in Hz
export
setCenterFreq : Ptr RtlSdrHandle -> Nat -> IO (Either RTLSDR_ERROR ())
setCenterFreq h f = do
  r <- fromPrim $ set_center_freq h (cast {to=Int} f)
  io_pure $ if r == 0 then Right () else Left RtlSdrInvalidFreq

||| Get actual frequency the device is tuned to.
|||
||| @h is the device handle
export
getCenterFreq : Ptr RtlSdrHandle -> IO (Either RTLSDR_ERROR Nat)
getCenterFreq h = do
  r <- fromPrim $ get_center_freq h
  io_pure $ if r == 0 then Left RtlSdrError else Right (cast {to=Nat} r)

||| Set the frequency correction value for the device.
|||
||| @h is the device handle
||| @ppm correction value in parts per million (ppm)
export
setFreqCorrection : Ptr RtlSdrHandle -> Int -> IO (Either RTLSDR_ERROR ())
setFreqCorrection h ppm = do
  r <- fromPrim $ set_freq_correction h ppm
  io_pure $ if r == 0 then Right () else Left RtlSdrError

||| Get actual frequency correction value of the device.
|||
||| @h is the device handle
export
getFreqCorrection : Ptr RtlSdrHandle -> IO Int
getFreqCorrection h = fromPrim $ get_freq_correction h

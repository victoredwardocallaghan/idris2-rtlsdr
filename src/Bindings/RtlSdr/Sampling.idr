module Bindings.RtlSdr.Sampling

import Bindings.RtlSdr.Device
import Bindings.RtlSdr.Error
import Bindings.RtlSdr.Raw.Sampling

%default total

||| Set the sample rate for the device, also selects the baseband filters
||| according to the requested sample rate for tuners where this is possible.
|||
||| @h is the device handle
||| @r is the sample rate to be set, possible values are:
||| 		    225001 - 300000 Hz
||| 		    900001 - 3200000 Hz
||| 		    sample loss is to be expected for rates > 2400000
export
setSampleRate : Ptr RtlSdrHandle -> Nat -> IO (Either RTLSDR_ERROR ())
setSampleRate h r = do
  r <- fromPrim $ set_sample_rate h (cast {to = Int} r)
  io_pure $ if r == 0 then Right () else Left RtlSdrInvalidRate

||| Get actual sample rate the device is configured to.
|||
||| @h is the device handle
export
getSampleRate : Ptr RtlSdrHandle -> IO Int
getSampleRate h = fromPrim $ get_sample_rate h

||| Enable or disable the internal digital AGC of the RTL2832.
|||
||| @h is the device handle
||| @t is the toggle of digital AGC mode, True means enabled, False means disabled
export
setAGCMode : Ptr RtlSdrHandle -> Bool -> IO (Either RTLSDR_ERROR ())
setAGCMode h t = do
  r <- fromPrim $ set_agc_mode h (if t then 1 else 0)
  io_pure $ if r == 0 then Right () else Left RtlSdrError

public export
data SamplingType = SAMPLING_DISABLED | SAMPLING_I_ADC_ENABLED | SAMPLING_Q_ADC_ENABLED | SAMPLING_IQ_ADC_ENABLED

export
Show SamplingType where
  show SAMPLING_DISABLED      = "Disabled"
  show SAMPLING_I_ADC_ENABLED = "I-ADC Input Enabled"
  show SAMPLING_Q_ADC_ENABLED = "Q-ADC Input Enabled"
  show SAMPLING_IQ_ADC_ENABLED = "IQ-ADC Input Enabled"

toSamplingType : Int -> Maybe SamplingType
toSamplingType 0 = Just SAMPLING_DISABLED
toSamplingType 1 = Just SAMPLING_I_ADC_ENABLED
toSamplingType 2 = Just SAMPLING_Q_ADC_ENABLED
toSamplingType 3 = Just SAMPLING_IQ_ADC_ENABLED
toSamplingType _ = Nothing

fromSamplingType : SamplingType -> Int
fromSamplingType SAMPLING_DISABLED      = 0
fromSamplingType SAMPLING_I_ADC_ENABLED = 1
fromSamplingType SAMPLING_Q_ADC_ENABLED = 2
fromSamplingType SAMPLING_IQ_ADC_ENABLED = 3

||| Enable or disable the direct sampling mode. When enabled, the IF mode
||| of the RTL2832 is activated, and `setCenterFreq` will control
||| the IF-frequency of the DDC, which can be used to tune from 0 to 28.8 MHz
||| (xtal frequency of the RTL2832).
|||
||| @h is the device handle
||| @t is the mode of `SampleType`
export
setDirectSampling : Ptr RtlSdrHandle -> SamplingType -> IO (Either RTLSDR_ERROR ())
setDirectSampling h t = do
  r <- fromPrim $ set_direct_sampling h (fromSamplingType t)
  io_pure $ if r < 0 then Left RtlSdrError else Right ()

||| Get state of the direct sampling mode
|||
||| @h is the device handle
export
getDirectSampling : Ptr RtlSdrHandle -> IO (Either RTLSDR_ERROR (Maybe SamplingType))
getDirectSampling h = do
  r <- fromPrim $ get_direct_sampling h
  io_pure $ if r < 0 then Left RtlSdrError else Right (toSamplingType r)

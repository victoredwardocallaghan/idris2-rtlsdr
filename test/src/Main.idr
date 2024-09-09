module Main

import Bindings.RtlSdr
import Data.Bits
import Data.Buffer
import Data.List
import System.FFI
import System.File
import System.File.Buffer

testOpenClose : IO ()
testOpenClose = do
  putStrLn "opening RTL SDR idx 0"
  h <- rtlsdr_open 0
  case h of
    Nothing => putStrLn "Failed to open device handle"
    Just h => do
      putStrLn $ show $ getTunerType h
      o <- getOffsetTuning h
      putStrLn $ "Tuner offset: " ++ (show o)
      g <- getTunerGain h
      putStrLn $ "Gain: " ++ (show g)
      f <- getCenterFreq h
      putStrLn $ "Freq: " ++ (show f)

      _ <- rtlsdr_close h
      putStrLn "Done, closing.."

abs : (i8, q8 : Bits8) -> Bits8
abs i8 q8 =
  let
    i : Double
    i = (cast i8 - 128) / 128

    q : Double
    q = (cast q8 - 128) / 128

    amp : Double
    amp = sqrt (i*i + q*q)
  in
    -- format: U8
    cast (amp * 255 * 16)

unIQ : List Bits8 -> List Bits8
unIQ [] = []
unIQ [_] = []
unIQ (i :: q :: rest) = abs i q :: unIQ rest

demodAM : List Bits8 -> List Bits8
demodAM = unIQ

writeBufToFile : List Bits8 -> IO ()
writeBufToFile bytes = do
  let len : Int = cast (length bytes)
  Just buf <- newBuffer len
    | Nothing => putStrLn "could not allocate buffer"

  for_ (zip [0 .. len-1] bytes) $ \(i, byte) =>
    setBits8 buf i byte

  result <- withFile "data.u8" Append printLn $ \f => do
    Right () <- writeBufferData f buf 0 len
      | Left (err, len) => do
          printLn ("could not writeBufferData", err, len)
          pure $ Left ()

    pure $ Right ()

  case result of
    Left err => printLn err
    Right () => pure ()

readAsyncCallback : AnyPtr -> List Bits8 -> IO ()
readAsyncCallback ctx buf = writeBufToFile (demodAM buf)

testAM : IO ()
testAM = do
  putStrLn "opening RTL SDR idx 0"
  h <- rtlsdr_open 0
  case h of
    Nothing => putStrLn "Failed to open device handle"
    Just h => do
      -- let fq = 476_425_000 -- UHF chan1 -- 133_250_000 -- YBTH AWIS
      let fq = 106_800_000

      _ <- setTunerGainMode h False
      _ <- setAGCMode h True -- ON
      _ <- setCenterFreq h fq
      _ <- setTunerBandwidth h 0 -- auto
      _ <- setTunerGain h 182
      -- _ <- setDirectSampling h (SAMPLING_I_ADC_ENABLED | SAMPLING_Q_ADC_ENABLED)
      _ <- setSampleRate h 250_000

      f <- getCenterFreq h
      putStrLn $ "Freq set to: " ++ (show f)

      fc <- getFreqCorrection h
      putStrLn $ "Freq correction set to: " ++ (show fc)

      g <- getTunerGain h
      putStrLn $ "Gain: " ++ (show g)
      f <- getCenterFreq h
      putStrLn $ "Freq: " ++ (show f)
      s <- getDirectSampling h
      putStrLn $ "Sampling mode: " ++ (show s)
      r <- getSampleRate h
      putStrLn $ "Sample rate: " ++ (show r)

      -- flush buffer
      _ <- resetBuffer h

      _ <- readAsync h readAsyncCallback prim__getNullAnyPtr 0 0

      _ <- rtlsdr_close h
      putStrLn "Done, closing.."


testDeviceFound : IO ()
testDeviceFound = do
  let n = get_device_count
  putStrLn $ "Device Count: " ++ show n
  for_ [0..n-1] $ \k => putStrLn $ "Device Name: " ++ get_device_name k

main : IO ()
main = do
  testDeviceFound
  testOpenClose
  testAM

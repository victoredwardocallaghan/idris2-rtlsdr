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

abs : (i, q : Bits8) -> (Bits8, Bits8)
abs i q =
  let
    ii : Double -- Bits32
    ii = cast i * cast i

    qq : Double -- Bits32
    qq = cast q * cast q

    iiqq' : Double -- Bits32
    iiqq' = sqrt ( ii + qq )

    -- clamp to 16bit word size.
    iiqq : Bits16
    iiqq = if iiqq' > 32768 then 32768 else (cast iiqq')

    -- encode U8 wav
    iiqqLo : Bits8
    iiqqLo = cast iiqq

    iiqqHi : Bits8
    iiqqHi = cast (iiqq `shiftR` 8)
  in
    (iiqqHi, iiqqLo)

unIQ : List Bits8 -> List Bits8
unIQ [] = []
unIQ [_] = []
unIQ (i :: q :: rest) =
  let (hi, lo) = abs i q
    in lo :: hi :: unIQ rest

demodAM : List Bits8 -> List Bits8
demodAM = unIQ

writeBufToFile : List Bits8 -> IO ()
writeBufToFile bytes = do
  let len : Int = cast (length bytes)
  Just buf <- newBuffer len
    | Nothing => putStrLn "could not allocate buffer"

  for_ (zip [0 .. len-1] bytes) $ \(i, byte) =>
    setBits8 buf i byte

  result <- withFile "data.wav" Append printLn $ \f => do
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
      let fq = 476_425_000 -- UHF chan1 -- 133_250_000 -- YBTH AWIS

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

module Main

import Bindings.RtlSdr
import Data.Bits
import Data.Buffer
import Data.List
import Data.String
import System
import System.FFI
import System.File
import System.File.Buffer

import AM
import EEPromRead

writeBufToFile : String -> List Int16 -> IO ()
writeBufToFile fpath bytes = do
  let len : Int = cast (length bytes)
  Just buf <- newBuffer (2*len)
    | Nothing => putStrLn "could not allocate buffer"

  for_ (zip [0 .. len-1] bytes) $ \(i, w) =>
    setBits16 buf (2*i) (cast w)

  result <- withFile fpath Append printLn $ \f => do
    Right () <- writeBufferData f buf 0 (2*len)
      | Left (err, len) => do
          printLn ("could not writeBufferData", err, len)
          pure $ Left ()

    pure $ Right ()

  case result of
    Left err => printLn err
    Right () => pure ()

readAsyncCallback : String -> Int -> Int -> Int -> ReadAsyncFn
readAsyncCallback fpath thres drate scale ctx buf = writeBufToFile fpath (demodAMStream buf drate scale thres)

record Args where
  constructor MkArgs
  fpath : Maybe String
  freq  : Maybe Int
  thres : Maybe Int
  ppm   : Maybe Int

testAM : Args -> IO ()
testAM args = do
  putStrLn "opening RTL SDR idx 0"
  h <- rtlsdr_open 0
  case h of
    Nothing => putStrLn "Failed to open device handle"
    Just h => do
      --let fq_default = 133_250_000 -- YBTH AWIS
      let fq_default = 127_350_000 -- YBTH CTAF
      let fq = fromMaybe fq_default args.freq

      let rate_in = 24_000
      putStrLn $ "Using a in rate of: " ++ (show $ rate_in `div` 1_000) ++ " kHz."
      let rate_iq = 1_008_000
      putStrLn $ "Sampling IQ stream at: " ++ (show $ rate_iq `div` 1_000) ++ "kHz."
      let rate_downsample = (1_000_000 `div` rate_in) + 1
      putStrLn $ "Calculated downsampling of: " ++ (show rate_downsample) ++ "x."
      let output_scale = (1 `shiftL` 15) `div` (128 * rate_downsample)
      putStrLn $ " debug> output scaled by: " ++ show output_scale

      _ <- setTunerGainMode h False -- manual gain
      _ <- setTunerGain h 192 -- 19.2dB

      _ <- setAGCMode h True -- ON
      _ <- setCenterFreq h fq
      _ <- setTunerBandwidth h 0 -- auto
      -- _ <- setDirectSampling h (SAMPLING_I_ADC_ENABLED | SAMPLING_Q_ADC_ENABLED)
      _ <- setSampleRate h rate_iq
      let ppm = fromMaybe (-15) args.ppm -- default ppm of -15
      _ <- setFreqCorrection h ppm

      f <- getCenterFreq h
      putStrLn $ "Freq set to: " ++ (show f)

      fc <- getFreqCorrection h
      putStrLn $ "Freq correction set to: " ++ (show fc)

      gs <- getTunerGains h
      putStrLn $ "Gains: " ++ (show gs)
      g <- getTunerGain h
      putStrLn $ "Gain: " ++ (show g)
      f <- getCenterFreq h
      putStrLn $ "Freq: " ++ (show f)
      o <- getOffsetTuning h
      putStrLn $ "Tuner offset: " ++ (show o)
      s <- getDirectSampling h
      putStrLn $ "Sampling mode: " ++ (show s)
      r <- getSampleRate h
      putStrLn $ "Sample rate: " ++ (show r)

      -- flush buffer
      _ <- resetBuffer h

      let fpath = fromMaybe "/dev/stderr" args.fpath
      putStrLn $ "File to write out to '" ++ (show fpath) ++ "'."

      let thres = fromMaybe 15 args.thres -- default threshold of >15
      _ <- readAsync h (readAsyncCallback fpath thres rate_downsample output_scale) prim__getNullAnyPtr 0 0

      _ <- rtlsdr_close h
      putStrLn "Done, closing.."


testDeviceFound : IO ()
testDeviceFound = do
  let n = get_device_count
  putStrLn $ "Device Count: " ++ show n
  for_ [0..n-1] $ \k => putStrLn $ "Device Name: " ++ get_device_name k

parseArgs : List String -> Args -> Either String Args
parseArgs [] = Right
parseArgs ("--file" :: f :: rest) = parseArgs rest . {fpath := Just f}
parseArgs ("--freq" :: f :: rest) =
  case parsePositive f of
    Nothing => \args => Left $ "--freq: could not parse: " ++ f
    Just f' => parseArgs rest . {freq  := Just f'}
parseArgs ("--threshold" :: t :: rest) =
  case parsePositive t of
    Nothing => \args => Left $ "--threshold: could not parse: " ++ t
    Just t' => parseArgs rest . {thres  := Just t'}
parseArgs ("--ppm" :: p :: rest) =
  case parsePositive p of
    Nothing => \args => Left $ "--ppm: could not parse: " ++ p
    Just p' => parseArgs rest . {ppm := Just p'}
parseArgs (arg :: rest) =
  \args => Left $ "unknown argument: " ++ arg

defaultArgs : Args
defaultArgs = MkArgs Nothing Nothing Nothing Nothing

main : IO ()
main = do
  exeName :: args' <- getArgs
    | [] => putStrLn "impossible: empty args"
  case parseArgs args' defaultArgs of
    Left err => putStrLn err
    Right args => do
      testDeviceFound
      testDumpEEProm
      testAM args

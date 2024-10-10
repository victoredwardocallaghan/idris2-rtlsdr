module Main

import Bindings.RtlSdr
import Data.Bits
import Data.Buffer
import Data.Either
import Data.List
import Data.String
import System
import System.Concurrency
import System.FFI
import System.File
import System.File.Buffer
import System.Signal

import AM
import EEPromRead

putErr : String -> IO ()
putErr s = ignore $ fPutStrLn stderr s

-- Package up a List of S16 words of PCM into a Buffer.
getWAV16Buffer : List Int16 -> IO (Maybe (Buffer, Int))
getWAV16Buffer words = do
  let len : Int = cast (length words)
  Just buf <- newBuffer (2*len) -- 2Bytes in 16Bit word.
    | Nothing => io_pure Nothing

  for_ (zip [0 .. len-1] words) $ \(i, w) =>
    setBits16 buf (2*i) (cast w)

  io_pure $ Just (buf, 2*len)

writeBufToFile : String -> Buffer -> Int -> IO (Either () ())
writeBufToFile fpath buf len = do
  withFile fpath Append printLn $ \f => do
    x <- writeBufferData f buf 0 len
    case x of
         Right () => pure $ Right ()
         Left (err, len) => do
           printLn ("could not writeBufferData", err, len)
           pure $ Left () -- FIXME: should return (err, len) instead.


data RWStream : Type where
  Stream : (stream : List IQ) -> RWStream
  Done : RWStream

writer : (wch : Channel RWStream) -> String -> Int -> Int -> IO ()
writer wch fpath dsr thres =
  do
    (Stream wstream) <- channelGet wch
      | Done => pure ()
    Just (buf, len) <- getWAV16Buffer (demodAMStream wstream dsr thres)
      | Nothing => putErr "getWAV16Buffer could not allocate buffer"
    r <- writeBufToFile fpath buf len
    case r of
         Left _ => pure ()
         Right () => writer wch fpath dsr thres

run : (rch : Channel RWStream) -> (wch : Channel RWStream) -> Ptr RtlSdrHandle -> Bool -> IO ()
run rch wch h False =
  do
    (Stream rstream) <- channelGet rch
      | Done => pure ()
    channelPut wch (Stream rstream)
    sig <- handleNextCollectedSignal
    case sig of
         Just SigINT => do
           putErr "Caught ^C"
           _ <- cancelAsync h
           putErr "Cancelled readAsync"
           run rch wch h True
         _ => run rch wch h False
run rch wch h True =
  do
    (Stream rstream) <- channelGet rch
      | Done => pure ()
    run rch wch h True

readAsyncCallback : Channel RWStream -> ReadAsyncFn
readAsyncCallback rch ctx iqlist = channelPut rch (Stream iqlist)

record Args where
  constructor MkArgs
  dpath : Maybe String
  fpath : Maybe String
  freq  : Maybe Int
  thres : Maybe Int
  ppm   : Maybe Int
  rate  : Maybe Int

freqStr : Int -> String
freqStr v = case v of
                 127350000 => "YBTH CTAF, " ++ show (v `div` 1_000) ++ "kHz."
                 133250000 => "YBTH AWIS, " ++ show (v `div` 1_000) ++ "kHz."
                 135250000 => "MEL CEN, " ++ show (v `div` 1_000) ++ "kHz."
                 _ => show (v `div` 1_000) ++ "kHz."

decodeRTLSDRError : (Show a) => Either RTLSDR_ERROR a -> String
decodeRTLSDRError v = fromMaybe "<unknown>" $ map show $ getRight v

cfgRTL : Ptr RtlSdrHandle -> Int -> Int -> Nat -> IO ()
cfgRTL h fq ppm r = do
      _ <- setTunerGainMode h False -- manual gain
      _ <- setTunerGain h (-100) -- auto
      _ <- setAGCMode h True -- ON
      _ <- setCenterFreq h fq
      _ <- setTunerBandwidth h 0 -- auto
      -- _ <- setDirectSampling h (SAMPLING_I_ADC_ENABLED | SAMPLING_Q_ADC_ENABLED)
      _ <- setSampleRate h r
      _ <- setFreqCorrection h ppm

      fc <- getFreqCorrection h
      putErr $ "Freq correction set to: " ++ (show fc)

      gs <- getTunerGains h
      putErr $ "Gains: " ++ (show gs)
      g <- getTunerGain h
      putErr $ "Gain: " ++ decodeRTLSDRError g
      Right f <- getCenterFreq h | Left _ => putErr "Freq: <unknown>"
      putErr $ "Freq: " ++ (freqStr f)
      o <- getOffsetTuning h
      putErr $ "Tuner offset: " ++ decodeRTLSDRError o
      s <- getDirectSampling h
      putErr $ "Sampling mode: " ++ decodeRTLSDRError s
      r <- getSampleRate h
      putErr $ "Sample rate: " ++ (show r)

      io_pure ()

testAM : Args -> IO ()
testAM args = do
  putErr "opening RTL SDR idx 0"
  h <- rtlsdr_open 0
  case h of
    Nothing => putErr "Failed to open device handle"
    Just h => do
      --let fq_default = 133_250_000 -- YBTH AWIS
      let fq_default = 127_350_000 -- YBTH CTAF
      let fq = fromMaybe fq_default args.freq

      let rate_in_default = 24_000
      let rate_in = fromMaybe rate_in_default args.rate

      putErr $ "Using a in rate of: " ++ (show $ rate_in `div` 1_000) ++ " kHz."
      let rate_downsample = (1_000_000 `div` rate_in) + 1
      putErr $ "Calculated downsampling of: " ++ (show rate_downsample) ++ "x."
      let rate_iq = cast $ rate_downsample * rate_in
      putErr $ "Sampling IQ stream at: " ++ (show $ rate_iq `div` 1_000) ++ "kHz."

      let ppm = fromMaybe 0 args.ppm -- default ppm of zero.
      let thres = fromMaybe 30 args.thres -- default threshold of -30dB.

      let fpath = fromMaybe "/dev/stdout" args.fpath
      putErr $ "File to write out to '" ++ (show fpath) ++ "'."

      cfgRTL h fq ppm rate_iq

      -- flush buffer
      _ <- resetBuffer h

      _ <- collectSignal SigINT

      readCh <- makeChannel
      writeCh <- makeChannel

      runTID <- fork $ run readCh writeCh h False
      writeTID <- fork $ writer writeCh fpath rate_downsample thres

      -- Use 6x 4096 length buffers for each read process cycle instead of larger default.
      -- This allows realtime streaming of samples into sound card at 24kHz or 48kHz rate_in.
      readTID <- fork $ do
        _ <- readAsync h (readAsyncCallback readCh) prim__getNullAnyPtr 6 4096
        pure ()

      threadWait readTID
      putErr "waited for reader to stop"

      channelPut readCh Done
      threadWait runTID

      putErr "waited for run to stop"

      channelPut writeCh Done
      threadWait writeTID

      putErr "waited for writer to stop"

      _ <- rtlsdr_close h
      putErr "Done, closing.."


testDeviceFound : IO ()
testDeviceFound = do
  let di = getDeviceIndexBySerial "00127350"
  putErr $ "Search for device with serial '00127350': " ++ (show di)
  let di' = getDeviceIndexBySerial "00000001"
  putErr $ "Search for device with serial '00000001': " ++ (show di')
  let n = get_device_count
  putErr $ "Device Count: " ++ show n
  for_ [0..n-1] $ \k => putErr $ "Device Name: " ++ get_device_name k
  for_ [0..n-1] $ \k => do
    Right ds <- getDeviceUSBStrings 0 | Left _ => pure ()
    putErr $ "Found: " ++ (show ds)

parseArgs : List String -> Args -> Either String Args
parseArgs [] = Right
parseArgs ("--dump" :: f :: rest) = parseArgs rest . {dpath := Just f}
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
parseArgs ("--rate" :: r :: rest) =
  case parsePositive r of
    Nothing => \args => Left $ "--rate: could not parse: " ++ r
    Just r' => parseArgs rest . {rate := Just r'}
parseArgs (arg :: rest) =
  \args => Left $ "unknown argument: " ++ arg

defaultArgs : Args
defaultArgs = MkArgs Nothing Nothing Nothing Nothing Nothing Nothing

main : IO ()
main = do
  exeName :: args' <- getArgs
    | [] => putErr "impossible: empty args"
  case parseArgs args' defaultArgs of
    Left err => putErr err
    Right args => do
      testDeviceFound
      testDumpEEProm args.dpath
      testAM args

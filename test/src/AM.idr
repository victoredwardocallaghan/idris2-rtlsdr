module AM

import Data.Buffer
import Data.List

-- Calculate the magnitude of the IQ vector and scale it,
-- scale is defined as S16_MAX_SZ/128.
mag : Int -> (i, q : Int16) -> Int16
mag s i q =
  let
    ii : Double
    ii = cast i * cast i

    qq : Double
    qq = cast q * cast q
  in
    cast $ sqrt ( ii + qq ) * (cast s)

demodAM : Int -> List Int16 -> List Int16
demodAM _ [] = []
demodAM _ [_] = []
demodAM s (i :: q :: rest) =
  let w = mag s i q
    in w :: demodAM s rest

average : List Int16 -> Int16
average xs = cast {to = Int16} $
  foldr ((+) . cast {to = Int}) 0 xs `div` cast (length xs)

downSample : Int -> List Int16 -> List Int16
downSample chunkLen [] = []
downSample chunkLen xs with (splitAt (cast chunkLen) xs)
  _ | (chunk, rest) = average chunk :: downSample chunkLen rest

thresholdFilter : Int -> List Int16 -> List Int16
thresholdFilter t xs = map (\v => if abs(v) > (cast t) then v else 0) xs

-- Turn [U8] into [S16] re-centred around zero.
scaleStream : List Bits8 -> List Int16
scaleStream l = map (\i => cast {to = Int16} i - 127) l

export
demodAMStream : List Bits8 -> Int -> Int -> Int -> List Int16
demodAMStream l dsr s t =
  let
    demod   : List Int16
    demod   = demodAM s $ scaleStream l
    dsample : List Int16
    dsample = downSample dsr demod
  in
    thresholdFilter t dsample


module Ivory.HXStream.Test where

import Ivory.HXStream


{-
decodeEncoded :: [Frame] -> Bool
decodeEncoded fs = fs' == fs
  where
  encoded = foldl (\acc f -> B.append acc (encode f)) B.empty fs
  (fs',_) = decode encoded emptyStreamState
-}

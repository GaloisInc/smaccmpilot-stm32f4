
module Data.HXStream.Test where

import qualified Data.ByteString as B

import Test.QuickCheck

import Data.HXStream

decodeEncoded :: [Frame] -> Bool
decodeEncoded fs = fs' == map B.pack fs
  where
  encoded = foldl (\acc f -> B.append acc (encode f)) B.empty fs
  (fs',_) = decode encoded emptyStreamState

main :: IO ()
main = quickCheck decodeEncoded

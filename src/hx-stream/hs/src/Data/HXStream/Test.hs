
module Data.HXStream.Test where

import qualified Data.ByteString.Char8 as B

import Test.QuickCheck

import Data.HXStream

decodeEncoded :: [Frame] -> Bool
decodeEncoded fs = fs' == fs
  where
  encoded = foldl (\acc f -> B.append acc (encode f)) B.empty fs
  (fs',_) = decode encoded emptyStreamState

main :: IO ()
main = quickCheck decodeEncoded

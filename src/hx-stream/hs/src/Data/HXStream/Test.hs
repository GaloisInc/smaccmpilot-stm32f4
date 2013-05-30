
module Data.HXStream.Test where

import Control.Applicative
import qualified Data.ByteString.Char8 as B

import Test.QuickCheck

import Data.HXStream

instance Arbitrary Frame where
  arbitrary = Frame <$> arbitrary

decodeEncoded :: [Frame] -> Bool
decodeEncoded fs = fs' == fs
  where
  encoded = foldl (\acc f -> B.append acc (encode f)) B.empty fs
  (fs',_) = decode encoded emptyStreamState

module Data.HXStream.Test where

import qualified Data.DList      as D
import qualified Data.ByteString as B
import qualified Test.QuickCheck as Q

import Data.HXStream

newtype BS = BS B.ByteString

instance Show BS where
  show (BS bs) = show (B.unpack bs)

prettyShow :: B.ByteString -> String
prettyShow bs = show (B.unpack bs)

instance Q.Arbitrary BS where
  arbitrary = do
   let w = Q.frequency [(2, return fbo), (1, return ceo), (5, Q.arbitrary)]
   bs <- Q.vectorOf 100 w
   return (BS $ B.pack bs)

decodeEncoded :: BS -> Bool
decodeEncoded (BS bs) =
  bs == decoded `B.append` (B.pack $ D.toList $ frame st)
  where
  encoded    = encode 0 bs
  (decs, st) = decode encoded emptyStreamState
  decoded    = B.concat $ snd $ unzip decs

main :: IO ()
main = Q.quickCheck decodeEncoded

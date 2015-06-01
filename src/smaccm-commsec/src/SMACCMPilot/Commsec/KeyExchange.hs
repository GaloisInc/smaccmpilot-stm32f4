{-# LANGUAGE RecordWildCards #-}

module SMACCMPilot.Commsec.KeyExchange
  ( PubKey(..)
  , PrivKey(..)
  , pubKeyParser
  , privKeyParser
  , keyPairParser
  ) where

import Data.Word
import Ivory.Tower.Config

data PubKey =
  PubKey
    { ke_pubkey :: [Word8] -- Vector of 32 word8s
    } deriving (Eq, Show)

data PrivKey =
  PrivKey
    { ke_privkey :: [Word8] -- Vector of 32 word8s
    } deriving (Eq, Show)

pubKeyParser :: ConfigParser PubKey
pubKeyParser = subsection "pub" $ fmap PubKey $ keyParser

privKeyParser :: ConfigParser PrivKey
privKeyParser = subsection "priv" $ fmap PrivKey $ keyParser

keyPairParser :: ConfigParser (PubKey, PrivKey)
keyPairParser = do
  pub <- pubKeyParser
  priv <- privKeyParser
  return (pub, priv)

keyParser :: ConfigParser [Word8]
keyParser = arrayOfLen 32 $ boundedInteger 0 255
  where
  arrayOfLen l e = do
    a <- array e
    if length a == l then return a
      else fail ("not of length " ++ show l)
  boundedInteger l h = do
    i <- integer
    if i >= l && i <= h then return (fromIntegral i)
      else fail ("not within bounds [" ++  show l ++ ".." ++ show h ++ "]")


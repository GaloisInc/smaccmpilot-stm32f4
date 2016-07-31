module SMACCMPilot.Commsec.SymmetricKey where

import Data.Word
import Ivory.Tower.Config

data SymmetricKey =
  SymmetricKey
    { sk_c2s :: [Word8] -- Vector of 24 word8s
    , sk_s2c :: [Word8] -- Vector of 24 word8s
    } deriving (Eq, Show)

symmetricKeyParser :: ConfigParser SymmetricKey
symmetricKeyParser = subsection "symmetric_key" $ do
  s2c <- subsection "server_to_client" ks
  c2s <- subsection "client_to_server" ks
  return SymmetricKey { sk_c2s = c2s, sk_s2c = s2c }
  where
  ks = subsection "keysalt" (arrayOfLen 24 (boundedInteger 0 255))
  arrayOfLen l e = do
    a <- array e
    if length a == l then return a
      else fail ("not of length " ++ show l)
  boundedInteger l h = do
    i <- integer
    if i >= l && i <= h then return (fromIntegral i)
      else fail ("not within bounds [" ++  show l ++ ".." ++ show h ++ "]")

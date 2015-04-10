
module SMACCMPilot.Commsec.Native.Encode
  ( commsecEncode
  , CommsecEncode
  , commsec_encode_run
  ) where

import Data.Word

import qualified Data.ByteString as B
import Data.ByteString (ByteString)

import Crypto.Cipher.AES                -- Vincent's GCM routine
import Crypto.Cipher.Types
import Data.Serialize

import SMACCMPilot.Commsec.Keys
import SMACCMPilot.Commsec.Native.Error

data CommsecEncode =
  CommsecEncode
    { commsec_encode_aes  :: AES
    , commsec_encode_salt :: Word64
    , commsec_encode_ctr  :: Word32
    }

commsec_encode_run :: CommsecEncode -> ByteString
                   -> (CommsecEncode, Either CommsecError ByteString)
commsec_encode_run encoder plaintext
  | ctr == maxBound = (encoder,  Left CounterTooBig)
  | otherwise       = (encoder', Right packedct)
  where
  aes      = commsec_encode_aes  encoder
  salt     = commsec_encode_salt encoder
  ctr      = commsec_encode_ctr  encoder
  encoder' = encoder { commsec_encode_ctr = ctr + 1 }
  header   = runPut $ do
    putWord32be ctr
  iv       = runPut $ do
    putWord64be salt
    putWord32be ctr
  aad      = B.empty
  tagLen   = 12
  (cyphertext, AuthTag tag) = encryptGCM aes iv aad plaintext
  packedct = B.concat [ header, cyphertext, B.take tagLen tag]

commsecEncode :: KeySalt -> CommsecEncode
commsecEncode ks = CommsecEncode
  { commsec_encode_aes  = initAES (B.pack (ks_key ks))
  , commsec_encode_salt = ks_salt ks
  , commsec_encode_ctr  = 1
  }



module SMACCMPilot.Commsec.Native.Encode
  ( gecEncode
  , GecEncode
  , gec_encode_run
  ) where

import Data.Word

import qualified Data.ByteString as B
import Data.ByteString (ByteString)

import Crypto.Cipher.AES                -- Vincent's GCM routine
import Crypto.Cipher.Types
import Data.Serialize

import SMACCMPilot.Commsec.Keys
import SMACCMPilot.Commsec.Native.Error

data GecEncode =
  GecEncode
    { gec_encode_aes  :: AES
    , gec_encode_salt :: Word64
    , gec_encode_ctr  :: Word32
    }

gec_encode_run :: GecEncode -> ByteString
                   -> (GecEncode, Either GecError ByteString)
gec_encode_run encoder plaintext
  | ctr == maxBound = (encoder,  Left CounterTooBig)
  | otherwise       = (encoder', Right packedct)
  where
  aes      = gec_encode_aes  encoder
  salt     = gec_encode_salt encoder
  ctr      = gec_encode_ctr  encoder
  encoder' = encoder { gec_encode_ctr = ctr + 1 }
  header   = runPut $ do
    putWord32be ctr
  iv       = runPut $ do
    putWord64be salt
    putWord32be ctr
  aad      = B.empty
  tagLen   = 12
  (cyphertext, AuthTag tag) = encryptGCM aes iv aad plaintext
  packedct = B.concat [ header, cyphertext, B.take tagLen tag]

gecEncode :: KeySalt -> GecEncode
gecEncode ks = GecEncode
  { gec_encode_aes  = initAES (B.pack (ks_key ks))
  , gec_encode_salt = ks_salt ks
  , gec_encode_ctr  = 1
  }


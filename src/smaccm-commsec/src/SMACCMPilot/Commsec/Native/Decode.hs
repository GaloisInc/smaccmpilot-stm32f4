
module SMACCMPilot.Commsec.Native.Decode
  ( gecDecode
  , GecDecode
  , gec_decode_run
  ) where

import Data.Word

import qualified Data.ByteString as B
import Data.ByteString (ByteString)

import Crypto.Cipher.AES                -- Vincent's GCM routine
import Crypto.Cipher.Types
import Data.Serialize

import SMACCMPilot.Commsec.Keys
import SMACCMPilot.Commsec.Native.Error

data GecDecode =
  GecDecode
    { gec_decode_aes  :: AES
    , gec_decode_salt :: Word64
    , gec_decode_ctr  :: Word32
    }

gec_decode_run :: GecDecode -> ByteString
                   -> (GecDecode, Either GecError ByteString)
gec_decode_run decoder cyphertext = case unpack cyphertext of
  Left _ -> (decoder, Left (TooShort (B.length cyphertext)))
  Right (ctr, ct, tag) -> dec ctr ct tag
  where
  aes      = gec_decode_aes  decoder
  salt     = gec_decode_salt decoder
  dec_ctr  = gec_decode_ctr  decoder

  unpack :: ByteString -> Either String (Word32, ByteString, ByteString)
  unpack pkg = (flip runGet) pkg $ do
    ctr <- getWord32be
    ct  <- getByteString 80
    tag <- getByteString 12
    return (ctr, ct, tag)

  dec ct_ctr ct tag = case () of
    -- Magic number - multiple client ids are deprecated
    () | dec_ctr == maxBound -> (decoder, Left CounterTooBig)
       | dec_ctr >= ct_ctr   -> (decoder, Left CounterTooOld)
       | ct_tag /= tag       -> (decoder, Left (BadTag ct_tag tag))
       | otherwise           -> (decoder', Right pt)
    where
    iv = runPut $ do
      putWord64be salt
      putWord32be ct_ctr
    aad = B.empty
    (pt, AuthTag auth_tag) = decryptGCM aes iv aad ct
    tagLen = 12
    ct_tag = B.take tagLen auth_tag
    decoder' = decoder { gec_decode_ctr = ct_ctr }

gecDecode :: KeySalt -> GecDecode
gecDecode ks = GecDecode
  { gec_decode_aes  = initAES (B.pack (ks_key ks))
  , gec_decode_salt = ks_salt ks
  , gec_decode_ctr  = 0
  }


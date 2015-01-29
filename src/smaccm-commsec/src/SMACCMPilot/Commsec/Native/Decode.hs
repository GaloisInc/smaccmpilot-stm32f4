
module SMACCMPilot.Commsec.Native.Decode
  ( commsecDecode
  , CommsecDecode
  , commsec_decode_run
  ) where

import Data.Word

import qualified Data.ByteString as B
import Data.ByteString (ByteString)

import Crypto.Cipher.AES                -- Vincent's GCM routine
import Crypto.Cipher.Types
import Data.Serialize

import SMACCMPilot.Commsec.Keys
import SMACCMPilot.Commsec.Native.Error

data CommsecDecode =
  CommsecDecode
    { commsec_decode_aes  :: AES
    , commsec_decode_salt :: Word32
    , commsec_decode_ctr  :: Word32
    }

commsec_decode_run :: CommsecDecode -> ByteString
                   -> (CommsecDecode, Either CommsecError ByteString)
commsec_decode_run decoder cyphertext = case unpack cyphertext of
  Left _ -> (decoder, Left (TooShort (B.length cyphertext)))
  Right (bid, ctr, ct, tag) -> dec bid ctr ct tag
  where
  aes      = commsec_decode_aes  decoder
  salt     = commsec_decode_salt decoder
  dec_ctr  = commsec_decode_ctr  decoder

  unpack :: ByteString -> Either String (Word32, Word32, ByteString, ByteString)
  unpack pkg = (flip runGet) pkg $ do
    bid <- getWord32be
    ctr <- getWord32be
    ct  <- getByteString 80
    tag <- getByteString 8
    return (bid, ctr, ct, tag)

  dec cid ct_ctr ct tag = case cid of
    -- Magic number - multiple client ids are deprecated
    1 | dec_ctr == maxBound -> (decoder, Left CounterTooBig)
      | dec_ctr >= ct_ctr   -> (decoder, Left CounterTooOld)
      | ct_tag /= tag       -> (decoder, Left (BadTag ct_tag tag))
      | otherwise           -> (decoder', Right pt)
    _ -> (decoder, Left (UnknownID (fromIntegral cid)))
    where
    iv = runPut $ do
      putWord32be salt
      putWord32be 1 -- Magic Number - 1 is the only valid cid now
      putWord32be ct_ctr
    aad = B.empty
    (pt, AuthTag auth_tag) = decryptGCM aes iv aad ct
    tagLen = 8
    ct_tag = B.take tagLen auth_tag
    decoder' = decoder { commsec_decode_ctr = ct_ctr }

commsecDecode :: KeySalt -> CommsecDecode
commsecDecode ks = CommsecDecode
  { commsec_decode_aes  = initAES (B.pack (ks_key ks))
  , commsec_decode_salt = ks_salt ks
  , commsec_decode_ctr  = 0
  }


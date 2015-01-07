
module SMACCMPilot.Commsec.Native.Decode
  ( commsecDecode
  , CommsecDecode
  , commsec_decode_run
  ) where

import Data.Word

import qualified Data.ByteString as B
import Data.ByteString (ByteString)

import qualified Data.Map as M
import Data.Map (Map)

import Crypto.Cipher.AES                -- Vincent's GCM routine
import Crypto.Cipher.Types
import Data.Serialize

import SMACCMPilot.Commsec.Config
import SMACCMPilot.Commsec.Native.Error

data CommsecDecode =
  CommsecDecode
    { commsec_decode_aes  :: AES
    , commsec_decode_salt :: Word32
    , commsec_decode_ctrs :: Map Word32 Word32
    }

commsec_decode_run :: CommsecDecode -> ByteString
                   -> (CommsecDecode, Either CommsecError ByteString)
commsec_decode_run decoder cyphertext = case unpack cyphertext of
  Left _ -> (decoder, Left (TooShort (B.length cyphertext)))
  Right (bid, ctr, ct, tag) -> dec bid ctr ct tag
  where
  aes      = commsec_decode_aes  decoder
  salt     = commsec_decode_salt decoder
  ctrs     = commsec_decode_ctrs decoder

  unpack :: ByteString -> Either String (Word32, Word32, ByteString, ByteString)
  unpack pkg = (flip runGet) pkg $ do
    bid <- getWord32be
    ctr <- getWord32be
    ct  <- getByteString 80
    tag <- getByteString 8
    return (bid, ctr, ct, tag)

  dec bid ct_ctr ct tag = case M.lookup bid ctrs of
    Nothing -> (decoder, Left (UnknownID (fromIntegral bid)))
    Just ctr
      | ctr == maxBound -> (decoder, Left CounterTooBig)
      | ctr >= ct_ctr   -> (decoder, Left CounterTooOld)
      | ct_tag /= tag   -> (decoder, Left (BadTag ct_tag tag))
      | otherwise       -> (decoder', Right pt)
    where
    iv = runPut $ do
      putWord32be salt
      putWord32be bid
      putWord32be ct_ctr
    aad = B.empty
    (pt, AuthTag auth_tag) = decryptGCM aes iv aad ct
    tagLen = 8
    ct_tag = B.take tagLen auth_tag
    decoder' = decoder { commsec_decode_ctrs = M.insert bid ct_ctr ctrs }

commsecDecode :: KeySalt -> Integer -> CommsecDecode
commsecDecode ks eid = CommsecDecode
  { commsec_decode_aes  = initAES (B.pack (ks_key ks))
  , commsec_decode_salt = ks_salt ks
  , commsec_decode_ctrs = M.fromList [(fromInteger eid, 0)]
  }


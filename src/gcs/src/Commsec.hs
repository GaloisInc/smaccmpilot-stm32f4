{-# LANGUAGE  GeneralizedNewtypeDeriving #-}

module Commsec
  ( BaseId(..)
  , SecureContext_HS(..)
  , Context
  , secPkgInit_HS
  , secPkgEncInPlace_HS
  , secPkgDec_HS
  ) where

import Data.Word


import qualified Data.ByteString as B
import Data.ByteString (ByteString)

import Crypto.Cipher.AES                -- Vincent's GCM routine
import Crypto.Cipher.Types
import Data.IORef
import Data.Serialize

-- Import for testing
-- import Data.Maybe
-- import qualified Data.ByteString.Char8 as BC
-- import Hexdump

--------------------------------------------------------------------------------
--Types

newtype BaseId = BaseId Word32
                 deriving (Eq, Ord, Show, Num)

instance Serialize BaseId where
    get = BaseId `fmap` getWord32be
    put (BaseId x) = putWord32be x

-- Key, salt, station ID, counter

-- You send using just one key (a shared key for bases) or the UAV key.
type OutContext = (AES, Word32, BaseId, Word32)

-- You can have many senders, so we have a list.
type InContext  = (AES, Word32, [(BaseId,Word32)])

data SecureContext_HS = SC { inContext    :: InContext
                           , outContext   :: OutContext
                           }

type Context = IORef SecureContext_HS

----------------------------------------------------------------------

commsecPkg :: OutContext -> ByteString
       -> (OutContext, Maybe (ByteString,ByteString,ByteString))
commsecPkg ctx@(key,salt,bid,ctr) pt
    | ctr == maxBound = (ctx, Nothing)
    | otherwise       =
  let iv  = runPut (putWord32be salt >> put bid >> putWord32be ctr)
      new = (key,salt,bid,ctr+1)
      aad = B.empty
      (ct, AuthTag tag) = encryptGCM key iv aad pt
      tagLen   = 8
      header   = runPut (put bid >> putWord32be ctr)
  in (new, Just (header, ct, B.take tagLen tag))

secPkgEnc_HS :: Context -> ByteString
             -> IO (Maybe (ByteString,ByteString,ByteString))
secPkgEnc_HS rc pt = do
  atomicModifyIORef' rc $ \(SC _in outbound') ->
    let (newOut,res) = commsecPkg outbound' pt in
    (SC _in newOut,res)

dec :: InContext -> ByteString -> (InContext, Maybe ByteString)
dec old@(key,salt,bidList) pkg =
    let aad = B.empty
        -- Get the iv out of the send package.
        Right (bid,newCtr,ct,tag) =
           runGet (do bid'    <- get
                      newCtr' <- getWord32be
                      pt'     <- getByteString . (subtract 8) =<< remaining
                      tag'    <- getByteString =<< remaining
                      return (bid',newCtr',pt',tag')
                  ) pkg
        -- Serialize the salt I have with the sent baseID and updated counter.
        iv  = runPut (putWord32be salt >> put bid >> putWord32be newCtr)
        -- Replace the BaseID/Counter pair in the list with the updated pair.
        newBids = (bid,newCtr) : filter ((/= bid) . fst) bidList
        new     = (key, salt, newBids)
        -- Decrypt the message.  I get the sent message and auth. tag, if all
        -- goes well.
        (pt, AuthTag decTag) = decryptGCM key iv aad ct
    in case lookup bid bidList of
        Nothing -> (old, Nothing)
        Just cnt
          -- Counter is too high or old.
          | cnt == maxBound || cnt >= newCtr    -> (old,Nothing)
          -- Return my updated inContext with the decrypted message.
          | B.take (B.length tag) decTag == tag -> (new,Just pt)
          | otherwise                           -> (old,Nothing)

initOutContext :: ByteString -> Word32 -> BaseId -> OutContext
initOutContext key salt bid = (initAES key, salt, bid, 1)

initInContext :: ByteString -> Word32 -> InContext
initInContext key salt
  = (initAES key, salt, zip (map BaseId [0..]) (replicate 16 0))

--------------------------------------------------------------------------------
-- Main interface functions

-- | Initialize a package returning a context.  Takes your ID, salt and key for
-- others to send to you, and salt and key for sending to others.  Returns an
-- updated context.
secPkgInit_HS :: BaseId -> Word32 -> ByteString -> Word32 -> ByteString
              -> IO Context
secPkgInit_HS b dsalt dkey esalt ekey
  = newIORef (SC (initInContext dkey dsalt)
                 (initOutContext ekey esalt b))

-- | Decrypt a package given a context.  Returns the decrypted message, if
-- successful.
secPkgDec_HS :: Context -> ByteString -> IO (Maybe ByteString)
secPkgDec_HS rc pkg =
  atomicModifyIORef' rc $ \(SC inbound' _out) ->
    let (newIn, res) = dec inbound' pkg in
    (SC newIn _out, res)

-- | Encrypt a message given a context.  Returns the concatenated header
p-- (including the sender's ID and message counter), the encrypted message, and
-- the authentication tag.
secPkgEncInPlace_HS :: Context -> ByteString -> IO (Maybe ByteString)
secPkgEncInPlace_HS rc pt = do
    r <- secPkgEnc_HS rc pt
    return $ fmap (\(a,b,c) -> B.concat [a,b,c]) r

--------------------------------------------------------------------------------
-- Testing

{-
maxMsgLen: :: Int
maxMsgLen = 84

uavID, base0ID, base1ID :: BaseId
uavID   = 0
base0ID = 0
base1ID = 1

b2uSalt, u2bSalt :: Word32
b2uSalt = 9219834
u2bSalt = 284920

someMsg :: String
someMsg = "This is a message from "

mkMsg :: String -> String
mkMsg msg = m ++ replicate n ' '
  where m = someMsg ++ msg
        n = maxMsgLen - length m

uavToBaseKey, baseToUavKey :: ByteString
uavToBaseKey = B.pack [0..15::Word8]
baseToUavKey = B.pack [15,14..0::Word8]

main :: IO ()
main = do
  uavCtx   <- secPkgInit_HS uavID   b2uSalt baseToUavKey u2bSalt uavToBaseKey
  base0Ctx <- secPkgInit_HS base0ID u2bSalt uavToBaseKey b2uSalt baseToUavKey
  base1Ctx <- secPkgInit_HS base1ID u2bSalt uavToBaseKey b2uSalt baseToUavKey

  Just uavPkg <- secPkgEncInPlace_HS uavCtx (BC.pack $ mkMsg "uav!")

  putStrLn (simpleHex uavPkg)

  secPkgDec_HS base0Ctx uavPkg >>= BC.putStrLn . fromJust
  -- Can't decrypt twice.
  -- secPkgDec_HS base0Ctx uavPkg >>= BC.putStrLn . fromJust
-}

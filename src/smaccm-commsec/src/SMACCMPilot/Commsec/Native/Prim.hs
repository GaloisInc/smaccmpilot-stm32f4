{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SMACCMPilot.Commsec.Native.Prim
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

--------------------------------------------------------------------------------
--Types

-- BaseId includes the UAV.

newtype BaseId = BaseId Word32
                 deriving (Eq, Ord, Show, Num)

instance Serialize BaseId where
    get = BaseId `fmap` getWord32be
    put (BaseId x) = putWord32be x

-- Key, salt, station ID, counter
type OutContext = (AES, Word32, BaseId, Word32)

-- You can have many senders, so we have a list of id/counter pairs.
type InContext  = (AES, Word32, [(BaseId,Word32)])

data SecureContext_HS = SC { inContext    :: InContext
                           , outContext   :: OutContext
                           }

type Context = IORef SecureContext_HS

-- Decryption failures
data Failure = CntrTooBig
             | CntTooOld
             | UnknownId
             | BadTag B.ByteString B.ByteString
             | CommsecTooShort Int
  deriving (Read, Eq)

instance Show Failure where
  show failure = commsecErr (msg failure)
    where
    msg CntrTooBig  = "Counter has overflowed!"
    msg CntTooOld   = "Counter is stale!"
    msg UnknownId   = "Unknown sender!"
    msg (BadTag tag tag') = "Incorrect authentication tag! rx tag: "
      ++ show (B.unpack tag) ++ " computed tag "
      ++ show (B.unpack tag')
    msg (CommsecTooShort len) = "Commsec too short: received length: "
      ++ show len

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

commsecErr :: String -> String
commsecErr err = "Commsec error: " ++ err

dec :: InContext -> ByteString -> Either Failure (InContext, ByteString)
dec (key,salt,bidList) pkg =
  either (const $ Left $ CommsecTooShort $ B.length pkg)
         go
         parseMsg

  where
  aad = B.empty
  parseMsg =
    runGet (do bid'    <- get
               newCtr' <- getWord32be
               pt'     <- getByteString . (subtract 8) =<< remaining
               tag'    <- getByteString =<< remaining
               return (bid',newCtr',pt',tag')
           ) pkg

  go (bid,newCtr,ct,tag) =
    -- Serialize the salt I have with the sent baseID and updated counter.
    let iv  = runPut (putWord32be salt >> put bid >> putWord32be newCtr) in
    -- Replace the BaseID/Counter pair in the list with the updated pair.
    let newBids = (bid,newCtr) : filter ((/= bid) . fst) bidList in
    let new     = (key, salt, newBids) in
    -- Decrypt the message.  I get the sent message and auth. tag, if all
    -- goes well.
    let (pt, AuthTag decTag) = decryptGCM key iv aad ct in
    let decTag' = B.take (B.length tag) decTag in

    case lookup bid bidList of
      Nothing             -> Left UnknownId
      Just cnt
        -- Counter is too high.
        | cnt == maxBound -> Left CntrTooBig
        -- Counter is too old.
        | cnt >= newCtr   -> Left CntTooOld
        -- Return my updated inContext with the decrypted message.
        | decTag' == tag  -> Right (new, pt)
        -- Bad tag.
        | otherwise       -> Left (BadTag tag decTag')

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
secPkgDec_HS :: Context
             -> ByteString
             -> IO (Either Failure ByteString)
secPkgDec_HS rc pkg =
  atomicModifyIORef' rc $ \sc@(SC inbound' _out) ->
    let resOrFail = dec inbound' pkg in
    case resOrFail of
      -- Threw away the message; keep the context the same.
      Left err           -> (sc, Left err)
      Right (newIn, res) -> (SC newIn _out, Right res)

-- | Encrypt a message given a context.  Returns the concatenated header
-- (including the sender's ID and message counter), the encrypted message, and
-- the authentication tag.
secPkgEncInPlace_HS :: Context -> ByteString -> IO (Maybe ByteString)
secPkgEncInPlace_HS rc pt = do
    r <- secPkgEnc_HS rc pt
    return $ fmap (\(a,b,c) -> B.concat [a,b,c]) r



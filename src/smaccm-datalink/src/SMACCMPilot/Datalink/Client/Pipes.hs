
module SMACCMPilot.Datalink.Client.Pipes where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Internal (c2w)
import           Data.Word
import           Text.Printf

import           Control.Monad
import           Pipes

import SMACCMPilot.Datalink.Client.Monad
import SMACCMPilot.Datalink.Client.ByteString

import qualified SMACCMPilot.Datalink.HXStream.Native as HX
import SMACCMPilot.Commsec.Sizes
import GEC.Datagram.Pure

word8Log :: String -> Pipe Word8 Word8 GW ()
word8Log tag = do
  w8 <- await
  lift $ writeLog (printf "%s Word8 %d (0x%0.2x)" tag w8 w8)
  word8Log tag

frameLog :: Pipe (HX.Tag, ByteString) (HX.Tag, ByteString) GW ()
frameLog = do
  (t, bs) <- await
  lift $ writeLog $ bytestringDebug (printf "frame (tag %d)" t) bs
  yield (t, bs)
  frameLog

bytestringLog :: String -> Pipe ByteString ByteString GW ()
bytestringLog tag = do
  bs <- await
  lift $ writeDbg $ bytestringDebug tag bs
  yield bs
  bytestringLog tag


showLog :: Show a => Pipe a a GW ()
showLog = do
  a <- await
  lift $ writeLog $ show a
  yield a
  showLog

tagger :: Monad m => HX.Tag -> Pipe a (HX.Tag, a) m ()
tagger t = do
  a <- await
  yield (t,a)
  tagger t

untagger :: Monad m => HX.Tag -> Pipe (HX.Tag, a) a m ()
untagger t = do
  (t', a) <- await
  when (t == t') $ yield a
  untagger t

bytestringUnpack :: Monad m => Pipe ByteString Word8 m ()
bytestringUnpack = do
  bs <- await
  mapM_ (yield . c2w) (B.unpack bs)
  bytestringUnpack

hxDecoder :: Monad m => Pipe ByteString (HX.Tag, ByteString) m ()
hxDecoder = bytestringUnpack >-> aux HX.emptyStreamState
  where
  aux ss = do
     b <- await
     let (mf, ss') = HX.decodeByte b ss
     case mf of
       Just tbs -> yield tbs
       Nothing -> return ()
     aux ss'

hxEncoder :: Pipe (HX.Tag, ByteString) ByteString GW ()
hxEncoder = do
  (t, bs) <- await
  case bytestringPad cyphertextSize bs of
    Left err -> lift (writeErr ("when padding to cyphertextSize: " ++ err))
    Right padded -> yield (HX.encode t padded)
  hxEncoder

type KeySalt = ByteString

commsecEncoder :: KeySalt -> Pipe ByteString ByteString GW ()
commsecEncoder ks = do
  case mkContextOut Small ks of
    Just ctx -> do
      lift (writeLog ("created commsecEncoder with ks " ++ show ks))
      aux ctx
    Nothing -> lift (writeErr "FATAL: Failed to create GEC encode context")
  where
  aux ctx = do
    pt <- await
    case encode ctx pt of
      Just (ctx', ct) -> yield ct >> aux ctx'
      Nothing -> lift (writeErr "GEC encode failed") >> aux ctx

commsecDecoder :: KeySalt -> Pipe ByteString ByteString GW ()
commsecDecoder ks = do
  case mkContextIn Small ks of
    Just ctx -> do
      lift (writeLog ("created commsecDecoder with ks " ++ show ks))
      aux ctx
    Nothing -> lift (writeErr "FATAL: failed to create GEC decode context")
  where
  aux ctx = do
    ct <- await
    case decode ctx ct of
      Just (ctx', pt) -> yield pt >> aux ctx'
      Nothing -> lift (writeErr "GEC decode failed") >> aux ctx

padder :: Integer -> Pipe ByteString ByteString GW ()
padder l = do
  b <- await
  case bytestringPad l b of
    Left err -> lift (writeErr err)
    Right padded -> yield padded
  padder l

unpadder :: Integer -> Pipe ByteString ByteString GW ()
unpadder l = do
  a <- await
  let (s,e) = B.splitAt (fromInteger l) a
  case B.all (== '\0') e of
    True -> yield s
    False -> lift (writeErr "unpadder error: tail contained nonzeroes")
  unpadder l

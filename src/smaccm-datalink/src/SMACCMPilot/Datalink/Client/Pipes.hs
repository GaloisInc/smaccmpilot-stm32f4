
module SMACCMPilot.Datalink.Client.Pipes where

import           Data.ByteString.Char8 (ByteString)
import           Data.Word
import           Text.Printf

import           Control.Monad
import           Pipes

import SMACCMPilot.Datalink.Client.Monad
import SMACCMPilot.Datalink.Client.ByteString

import qualified SMACCMPilot.Datalink.HXStream.Native as HX
import SMACCMPilot.Commsec.Sizes
import SMACCMPilot.Commsec.Native

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
  lift $ writeLog $ bytestringDebug tag bs
  yield bs
  bytestringLog tag


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

hxDecoder :: Monad m => Pipe Word8 (HX.Tag, ByteString) m ()
hxDecoder = aux HX.emptyStreamState
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

commsecEncoder :: KeySalt -> Pipe ByteString ByteString GW ()
commsecEncoder ks = do
  lift (writeLog ("created commsecEncoder with ks " ++ show ks))
  aux (commsecEncode ks)
  where
  aux e = do
    pt <- await
    let (e', er) = commsec_encode_run e pt
    case er of
      Left err -> lift (writeErr (show err))
      Right ct -> yield ct
    aux e'

commsecDecoder :: KeySalt -> Pipe ByteString ByteString GW ()
commsecDecoder ks = do
  lift (writeLog ("created commsecDecoder with ks " ++ show ks))
  aux (commsecDecode ks)
  where
  aux d = do
    ct <- await
    let (d', dr) = commsec_decode_run d ct
    case dr of
      Left err -> lift (writeErr (show err))
      Right pt -> yield pt
    aux d'


{-# LANGUAGE ExistentialQuantification, ForeignFunctionInterface, EmptyDataDecls #-}
{- interopTest.hs
 - This interoperability test is not ment to replace KATs and other
 - validation techniques that should be applied prior to fielding of the
 - associated AES-GCM code and 'commsec' encapsulation.
 -
 - However, this test routine does serve to demonstrate interoperability
 - with a completely separate implementation of AES that, in at least one
 - version, was shown to be correct against a large set of KATs.
 -
 - Function:                    Test:                   Notes:
 - securePkg_init               correct keyExpand       implicit
 - securePkg_zero               nada
 - securePkg_enc_in_place       correctness             interop
 - securePkg_dec                correctness             interop
 - securePkg_enc                correctness             interop
 -}
module Main where

import Control.Monad
import Data.Word
import Foreign.Ptr
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal (allocaBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Marshal.Alloc (mallocBytes)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Monadic
import Test.QuickCheck

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BC

import Crypto.Cipher.AES                -- Vincent's GCM routine
import Control.Concurrent.MVar
import Data.Serialize

import Debug.Trace

#include "commsec.h"

----------------------------------------------------------------------
-- Foreign import the securePkg_* calls which should be used
-- by HACMS/SMACCMS

-- If this were production code you would want to track the SecureContext
-- being zeroed and throw a run-time error when using a zeroed Ctx.
newtype SecureContext = SCtx (ForeignPtr Ctx)
data Ctx
newtype BaseId = BaseId Word32
                 deriving (Eq, Ord, Show)

instance Storable BaseId where
    sizeOf ~(BaseId x) = sizeOf x
    peek = fmap BaseId . peek . castPtr
    poke ptr (BaseId x) = poke (castPtr ptr) x
    alignment ~(BaseId x) = alignment x

instance Arbitrary BaseId where
    arbitrary = (BaseId . (`rem` 16) . abs) `fmap` arbitrary

instance Arbitrary ByteString where
    arbitrary = do
        len0 <- arbitrary
        let len = abs len0 `rem` 128
        B.pack `fmap` replicateM len arbitrary

instance Serialize BaseId where
    get = BaseId `fmap` getWord32be
    put (BaseId x) = putWord32be x

foreign import ccall "securePkg_size_of_message"
    c_securePkg_size_of_message :: CInt -> CInt

secPkg_size_of_message :: Int -> Int
secPkg_size_of_message = fromIntegral . c_securePkg_size_of_message . fromIntegral

foreign import ccall "securePkg_size_of_package"
    c_securePkg_size_of_package :: CInt -> CInt
secPkg_size_of_package :: Int -> Int
secPkg_size_of_package = fromIntegral . c_securePkg_size_of_package . fromIntegral

foreign import ccall "securePkg_init"
    c_securePkg_init :: Ptr Ctx -> BaseId ->
                        Word32 -> Ptr Word8 -> -- Decrypt Key
                        Word32 -> Ptr Word8 -> -- Encrypt Key
                        IO Word32

secPkgInit :: BaseId -> Word32 -> ByteString -> Word32 -> ByteString -> IO SecureContext
secPkgInit bid dsalt dkey esalt ekey = do
    -- FIXME sizeof is giving bad values!
    fptr <- mallocForeignPtrBytes ({#sizeof commsec_ctx#} + 10000)
    b <- withForeignPtr fptr $ \ctx ->
          BC.unsafeUseAsCString dkey $ \ptrD ->
           BC.unsafeUseAsCString ekey $ \ptrE ->
            c_securePkg_init ctx bid dsalt (castPtr ptrD) esalt (castPtr ptrE)
    if b == 0 then return (SCtx fptr) else error "secPkgInit failed"

foreign import ccall "securePkg_zero"
    c_securePkg_zero :: Ptr Ctx -> IO ()

secPkgZero :: SecureContext -> IO ()
secPkgZero (SCtx ptr) = withForeignPtr ptr c_securePkg_zero

foreign import ccall "securePkg_enc_in_place"
    c_securePkg_enc_in_place :: Ptr Ctx ->   -- Cipher ctx
                                Ptr Word8 -> -- Buffer
                                Word32 ->    -- Msg start index
                                Word32 ->    -- Msg Length
                                IO Word32

-- Encrypting data in place requires room for the header (8 bytes)
-- and room for the trailing tag (8 bytes)
secPkgEncInPlace :: SecureContext -> ByteString -> IO (Maybe ByteString)
secPkgEncInPlace (SCtx c) pt = do
    let pkgSz = secPkg_size_of_package . B.length $ pt
    pkg <- BI.mallocByteString pkgSz
    b <- withForeignPtr pkg $ \ptrCT ->
          BC.unsafeUseAsCStringLen pt $ \(ptrPT,lenPT) -> do
            copyBytes (castPtr ptrCT `plusPtr` 8) ptrPT lenPT
            withForeignPtr c $ \ctxPtr ->
             c_securePkg_enc_in_place ctxPtr (castPtr ptrCT) 8 (fromIntegral lenPT)
    let b = 0
    return (if b == 0 then Just (BI.fromForeignPtr pkg 0 pkgSz)
                      else Nothing)

foreign import ccall "securePkg_enc"
    c_securePkg_enc :: Ptr Ctx ->   -- Cipher ctx
                       Ptr Word8 -> -- PT Buffer (in) CT Buffer (out)
                       Word32    -> -- PT Len
                       Ptr Word8 -> -- Header Buffer (8 bytes, min)
                       Ptr Word8 -> -- Tag buffer (8 bytes, min)
                       IO Word32

-- Encrypting data in place requires room for the header (8 bytes)
-- and room for the trailing tag (8 bytes)
secPkgEnc :: SecureContext -> ByteString -> IO (Maybe (ByteString,ByteString,ByteString))
secPkgEnc (SCtx c) pt = do
    let tagLen = 8 -- C and IVORY could should use the C #define
        hdrLen = 8
        lenCT  = B.length pt
    ct  <- BI.mallocByteString lenCT
    hdr <- BI.mallocByteString hdrLen
    tag <- BI.mallocByteString tagLen
    b <- withForeignPtr ct $ \ptrCT ->
          BC.unsafeUseAsCStringLen pt $ \(ptrPT,lenPT) ->
           withForeignPtr tag $ \ptrTag ->
            withForeignPtr hdr $ \ptrHdr ->
             withForeignPtr c $ \ctxPtr -> do
               copyBytes (castPtr ptrCT) (castPtr ptrPT) lenPT
               c_securePkg_enc ctxPtr ptrCT (fromIntegral lenPT) ptrHdr ptrTag
    let tagBS = BI.fromForeignPtr tag 0 tagLen
        hdrBS = BI.fromForeignPtr hdr 0 hdrLen
        ctBS  = BI.fromForeignPtr ct 0 lenCT
    return (if b == 0 then Just (hdrBS,ctBS,tagBS) else Nothing)

foreign import ccall "securePkg_dec"
    c_securePkg_dec :: Ptr Ctx   ->
                       Ptr Word8 ->
                       Word32    ->
                       IO Word32

secPkgDec :: SecureContext -> ByteString -> IO (Maybe ByteString)
secPkgDec (SCtx c) ct =
  BC.unsafeUseAsCStringLen ct $ \(ptrPKG,lenPKG) -> do
    let lenW32 = fromIntegral lenPKG
        lenPT  = secPkg_size_of_message lenPKG
    fpPT <- BI.mallocByteString lenPT
    b <- allocaBytes lenPKG $ \tmpPtr -> do
         copyBytes tmpPtr ptrPKG lenPKG
         withForeignPtr c $ \ctxPtr -> do
         res <- c_securePkg_dec ctxPtr (castPtr tmpPtr) lenW32
         withForeignPtr fpPT $ \ptrPT -> do
           copyBytes ptrPT (tmpPtr `plusPtr` 8) lenPT
         return res
    return (if b == 0 then Just (BI.fromForeignPtr fpPT 0 lenPT)
                      else Nothing)

----------------------------------------------------------------------
-- Haskell re-implementation of the SecurePkg format.  This code uses
-- a completely separate C AES routine.  This is NOT a reference
-- in that it is easy to read, it is simply a re-implementation which
-- serves as a light sanity check on the functionallity of the prior
-- implementation.

-- Key, salt, station ID, counter
type OutContext = (Key,Word32,BaseId,Word32)
type InContext = (Key,Word32,[(BaseId,Word32)])
data SecureContext_HS = SC { inbound    :: InContext
                           , outbound   :: OutContext }
type ReferenceContext = MVar SecureContext_HS

refPkg :: OutContext -> ByteString -> (OutContext, Maybe (ByteString,ByteString,ByteString))
refPkg ctx@(key,salt,bid,ctr) pt
    | ctr == maxBound = (ctx, Nothing)
    | otherwise       =
  let iv  = runPut (putWord32be salt >> put bid >> putWord32be ctr)
      new = (key,salt,bid,ctr+1)
      aad = B.empty
      (ct,tag) = encryptGCM key (IV iv) aad pt
      tagLen   = 8
      header   = runPut (put bid >> putWord32be ctr)
  in (new, Just (header, ct, B.take tagLen tag))

refDec :: InContext -> ByteString -> (InContext, Maybe ByteString)
refDec old@(key,salt,bidList) pkg =
    let iv  = runPut (putWord32be salt >> put bid >> putWord32be newCtr)
        aad = B.empty
        Right (bid,newCtr,ct,tag) =
           runGet (do bid    <- get
                      newCtr <- getWord32be
                      pt <- getByteString . (subtract 8) =<< remaining
                      tag <- getByteString =<< remaining
                      return (bid,newCtr,pt,tag)
                      ) pkg
        newBids = (bid,newCtr) : filter ((/= bid) . fst) bidList
        new     = (key,salt,newBids)
        (pt,decTag) = decryptGCM key (IV iv) aad ct
    in case lookup bid bidList of
        Nothing -> (old, Nothing)
        Just cnt | cnt == maxBound || cnt >= newCtr    -> (old,Nothing)
                 | B.take (B.length tag) decTag == tag -> (new,Just pt)
                 | otherwise                           -> (old,Nothing)


refInitOutContext :: ByteString -> Word32 -> BaseId -> OutContext
refInitOutContext key salt bid = (initKey key, salt, bid, 1)

refInitInContext :: ByteString -> Word32 -> InContext
refInitInContext key salt
  = (initKey key, salt, zip (map BaseId [0..]) (replicate 16 0))

secPkgInit_HS :: BaseId -> Word32 -> ByteString -> Word32 -> ByteString -> IO ReferenceContext
secPkgInit_HS b dsalt dkey esalt ekey
  = newMVar (SC (refInitInContext dkey dsalt)
                (refInitOutContext ekey esalt b))

secPkgEncInPlace_HS :: ReferenceContext -> ByteString -> IO (Maybe ByteString)
secPkgEncInPlace_HS rc pt = do
    r <- secPkgEnc_HS rc pt
    case r of
        Just (a,b,c) -> return (Just (B.concat [a,b,c]))
        Nothing -> return Nothing

----------------------------------------------------------------------
-- Lifting the reference implementation to match the API
secPkgEnc_HS :: ReferenceContext -> ByteString -> IO (Maybe (ByteString,ByteString,ByteString))
secPkgEnc_HS rc pt =
  modifyMVar rc $ \(SC _in outbound) ->
    let (newOut,res) = refPkg outbound pt in do
    return (SC _in newOut,res)

secPkgDec_HS :: ReferenceContext -> ByteString -> IO (Maybe ByteString)
secPkgDec_HS rc pkg =
  modifyMVar rc $ \(SC inbound _out) ->
    let (newIn, res) = refDec inbound pkg in return (SC newIn _out, res)

-----------------------------------------------------------------------
-- Quickcheck tests showing compatability of the two implementations.

-- FIXME more tests, use monadic guard to not test trivial pt==null case.

prop_encEq :: ByteString -> ByteString -> ByteString -> Word32 -> Word32 -> Word32 -> Property
prop_encEq dKeySmall eKeySmall ptSmall dsalt esalt ctr
  = monadicIO $ do
    let dkey = B.take 16 (B.append dKeySmall (B.replicate 16 0))
    let ekey = B.take 16 (B.append eKeySmall (B.replicate 16 0))
    let pt   = B.take (max (B.length ptSmall) 1) (B.append ptSmall (B.replicate 1 1))
    sctx <- run $ secPkgInit    (BaseId 0) dsalt dkey esalt ekey
    rctx <- run $ secPkgInit_HS (BaseId 0) dsalt dkey esalt ekey
    rPKG <- run $ secPkgEnc_HS rctx pt
    sPKG <- run $ secPkgEnc sctx pt
    assert $ sPKG == rPKG

prop_decCompat :: ByteString -> ByteString -> ByteString -> Word32 -> Word32 -> Word32 -> Property
prop_decCompat dKeySmall eKeySmall ptSmall dsalt esalt ctr
  = monadicIO $ do
    let dkey = B.take 16 (B.append dKeySmall (B.replicate 16 0))
    let ekey = B.take 16 (B.append eKeySmall (B.replicate 16 0))
    let pt   = B.take (max (B.length ptSmall) 1) (B.append ptSmall (B.replicate 1 1))
    sctx <- run $ secPkgInit (BaseId 0) dsalt dkey esalt ekey
    rctx <- run $ secPkgInit_HS (BaseId 0) dsalt dkey esalt ekey
    Just rPKG <- run $ secPkgEncInPlace_HS rctx pt
    Just sPKG <- run $ secPkgEncInPlace sctx pt
    dec1 <- run $ secPkgDec_HS rctx sPKG
    dec2 <- run $ secPkgDec sctx rPKG
    assert $ sPKG == rPKG && dec1 == dec2

prop_decFail :: ByteString -> ByteString -> ByteString -> Word32 -> Word32 -> Word32 -> Property
prop_decFail dKeySmall eKeySmall ptSmall dsalt esalt ctr
  = monadicIO $ do
    let dkey = B.take 16 (B.append dKeySmall (B.replicate 16 0))
    let ekey = B.take 16 (B.append eKeySmall (B.replicate 16 0))
    let pt   = B.take (max (B.length ptSmall) 1) (B.append ptSmall (B.replicate 1 1))
    sctx <- run $ secPkgInit (BaseId 0) dsalt dkey esalt ekey
    rctx <- run $ secPkgInit_HS (BaseId 0) dsalt dkey esalt ekey
    Just sPKG0 <- run $ secPkgEncInPlace sctx pt
    i <- pick $ fmap ((`rem` B.length sPKG0) . abs) arbitrary
    let sPKG = B.pack . incIth i . B.unpack $ sPKG0
    dec1 <- run $ secPkgDec_HS rctx sPKG
    dec2 <- run $ secPkgDec sctx sPKG
    assert $ dec1 == dec2 && dec1 == Nothing
 where
 incIth i ls = take i ls ++ [(ls !! i) + 1] ++ drop (i+1) ls

data Test = forall a. Testable a => T a String

tests = [T prop_encEq "prop_encEq"
        ,T prop_decFail "prop_decFail"
        ,T prop_decCompat "prop_decCompat"
        ]

runTest :: Test -> IO ()
runTest (T a s) = putStrLn ("Testing: " ++ s) >> quickCheck a

runTests :: [Test] -> IO ()
runTests = mapM_ runTest

main :: IO ()
main = runTests tests
{-
main = do
    putStrLn "Notice the commsec_ctx structure is ~9k, depending on configuration.  However, c2hs believes it is smaller (see the below number) so we manually ad 10K to make this test work.   This is a definite FIXME."
    print {#sizeof commsec_ctx#}
    let key = B.replicate 16 0
    ctx <- secPkgInit (BaseId 0) 1 key 1 key
    ctx2 <- secPkgInit_HS (BaseId 0) 1 key 1 key
    Just pkg1 <- secPkgEncInPlace_HS ctx2 (B.pack [1..100])
    Just pkg2 <- secPkgEncInPlace ctx (B.pack [1..100])
    Just pkg3 <- secPkgEncInPlace ctx (B.pack [1..100])
    print pkg1
    print pkg2
    print pkg3
    secPkgDec_HS ctx2 pkg1 >>= print
    secPkgDec ctx pkg2 >>= print
    secPkgDec ctx pkg3 >>= print
    return ()
-}

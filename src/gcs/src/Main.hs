module Main where

import           System.Environment
import           System.Console.GetOpt
import qualified Data.ByteString as B

import           Data.List
import           Data.Word
import           CommsecServer

--------------------------------------------------------------------------------

main :: IO ()
main =
  --True baseToUavKey b2uSalt uavToBaseKey u2bSalt
  commsecServer . getOpts =<< getArgs

--------------------------------------------------------------------------------

options :: [OptDescr (Options -> Options)]
options =
  [ Option [] ["baseid"]
      (ReqArg (\arg opts -> opts { baseID = mkID arg}) "uint32")
      "base identifier"
  , Option [] ["basekey"]
      (ReqArg (\arg opts -> opts { baseKey = mkKey arg}) "[uint8]")
      "base key: 16 uint8s in form '[3, 4, 5, ..., n]'"
  , Option [] ["basesalt"]
      (ReqArg (\arg opts -> opts { baseSalt = mkSalt arg}) "uint32")
      "base salt"
  , Option [] ["uavkey"]
      (ReqArg (\arg opts -> opts { uavKey = mkKey arg}) "[uint8]")
      "uav key: 16 uint8s in form '[3, 4, 5, ..., n]'"
  , Option [] ["uavsalt"]
      (ReqArg (\arg opts -> opts { uavSalt = mkSalt arg}) "uint32")
      "uav salt"
  , Option [] ["no-errors"]
      (NoArg (\opts -> opts { showErrs = False }))
      "Display commsec errors"
 ]

mkID :: String -> Word32
mkID opt =
  maybe (error "Could not parse Base ID") chkSz (readMaybe opt)
  where
  chkSz ident | ident < 16 = ident
              | otherwise  = error "BaseID must be less than 16"

mkSalt :: String -> Word32
mkSalt opt =
  maybe (error "Could not parse salt") id (readMaybe opt)

mkKey :: String -> [Word8]
mkKey opt =
  maybe (error "Could not parse key") chkLen (readMaybe opt)
  where
  chkLen ls | length ls == 16 = ls
            | otherwise       = error "Key wrong length: expected length: 16."

getOpts :: [String] -> Options
getOpts argv = do
  case getOpt Permute options argv of
    (opts, nonOpts, errs)
      | not (null errs)    -> error $ "Option errors: "
                                   ++ unwords errs ++ usageInfo "" options
      | not (null nonOpts) -> error $ "Unrecognized: options: "
                                   ++ unwords nonOpts
      | otherwise          -> foldl (flip id) defaultOpts opts

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
                [(x, "")] -> Just x
                _         -> Nothing

--------------------------------------------------------------------------------
-- Testing

uavToBaseKey, baseToUavKey :: B.ByteString
uavToBaseKey = B.pack [0..15    :: Word8]
baseToUavKey = B.pack [15,14..0 :: Word8]

b2uSalt, u2bSalt :: Word32
b2uSalt = 9219834
u2bSalt = 284920

--------------------------------------------------------------------------------


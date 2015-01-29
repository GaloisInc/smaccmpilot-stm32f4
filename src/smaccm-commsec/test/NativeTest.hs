module Main where

import System.Exit (exitFailure, exitSuccess)
import qualified Data.ByteString.Char8 as BC

import SMACCMPilot.Commsec.Config
import SMACCMPilot.Commsec.Native.Encode
import SMACCMPilot.Commsec.Native.Decode

trivial_key :: KeySalt
trivial_key = KeySalt { ks_key = take 16 [1..], ks_salt = 0xdeadbeef }

mkPt :: String -> BC.ByteString
mkPt m = BC.pack (m ++ replicate n ' ')
  where
  n = maxMsgLen - length m
  maxMsgLen = 80


shouldPass :: Either String String -> IO ()
shouldPass t = case t of
  Left e -> putStrLn e >> exitFailure
  Right m -> putStrLn m
shouldFail :: Either String String -> IO ()
shouldFail t = case t of
  Left e -> putStrLn e
  Right m -> putStrLn m >> exitFailure

main :: IO ()
main = do
  shouldPass test_encode_decode
  shouldPass test_progression
  shouldFail test_replay
  shouldFail test_keys_dont_match
  shouldFail test_salts_dont_match
  exitSuccess


test_encode_decode :: Either String String
test_encode_decode =
  let (_e', er) = commsec_encode_run e m in
  case er of
    Left err -> failure (show err)
    Right ct ->
      let (_d', dr) = commsec_decode_run d ct in
      case dr of
        Left err -> failure (show err)
        Right m' -> case m == m' of
          False -> failure "result message did not match input"
          True -> Right "test_encode_decode success"
  where
  failure t = Left ("intended failure in test_encode_decode: " ++ t)
  m = mkPt "the quick brown fox jumped"
  e = commsecEncode trivial_key
  d = commsecDecode trivial_key

test_keys_dont_match :: Either String String
test_keys_dont_match =
  let (_e', er) = commsec_encode_run e m in
  case er of
    Left err -> failure (show err)
    Right ct ->
      let (_d', dr) = commsec_decode_run d ct in
      case dr of
        Left err -> failure (show err)
        Right m' -> case m == m' of
          False -> failure "result message did not match input"
          True -> Right "test_keys_dont_match failed to fail"
  where
  failure t = Left ("intended failure in test_keys_dont_match: " ++ t)
  m = mkPt "the quick brown fox jumped"
  e = commsecEncode trivial_key
  d = commsecDecode trivial_key { ks_key = take 16 [2..] }

test_salts_dont_match :: Either String String
test_salts_dont_match =
  let (_e', er) = commsec_encode_run e m in
  case er of
    Left err -> failure (show err)
    Right ct ->
      let (_d', dr) = commsec_decode_run d ct in
      case dr of
        Left err -> failure (show err)
        Right m' -> case m == m' of
          False -> failure "result message did not match input"
          True -> Right "test_salts_dont_match failed to fail"
  where
  failure t = Left ("intended failure in test_salts_dont_match: " ++ t)
  m = mkPt "the quick brown fox jumped"
  e = commsecEncode trivial_key
  d = commsecDecode trivial_key { ks_salt = 0xcafecafe }

test_progression :: Either String String
test_progression =
  let e = commsecEncode trivial_key
      d = commsecDecode trivial_key
      in case aux e d of
        Left err -> Left (err ++ " in first round")
        Right (e', d') -> case aux e' d' of
          Left err -> Left (err ++ " in second round")
          Right (e'', d'') -> case aux e'' d'' of
            Left err -> Left (err ++ " in third round")
            Right _ -> Right "test_progression success"
  where
  aux e d =
    let (e', er) = commsec_encode_run e m in
    case er of
      Left err -> failure (show err)
      Right ct ->
        let (d', dr) = commsec_decode_run d ct in
        case dr of
          Left err -> failure (show err)
          Right m' -> case m == m' of
            False -> failure "result message did not match input"
            True -> Right (e', d')
  failure t = Left ("failure in test_progression: " ++ t)
  m = mkPt "the quick brown fox jumped"

test_replay :: Either String String
test_replay =
  let e = commsecEncode trivial_key
      d = commsecDecode trivial_key
      in case aux e d of
        Left err -> Left (err ++ " in first round")
        Right (_e', d') -> case aux e d' of -- Use original encode context, not incremented one.
          Left err -> Left (err ++ " in second round")
          Right _ -> Right "test_replay failed to fail"
  where
  aux e d =
    let (e', er) = commsec_encode_run e m in
    case er of
      Left err -> failure (show err)
      Right ct ->
        let (d', dr) = commsec_decode_run d ct in
        case dr of
          Left err -> failure (show err)
          Right m' -> case m == m' of
            False -> failure "result message did not match input"
            True -> Right (e', d')
  failure t = Left ("intended failure in test_replay: " ++ t)
  m = mkPt "the quick brown fox jumped"


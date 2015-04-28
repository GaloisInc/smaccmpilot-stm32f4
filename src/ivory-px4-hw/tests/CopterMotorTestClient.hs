{-# LANGUAGE RecordWildCards #-}

module Main where

import System.Environment (getArgs)
import System.Console.GetOpt
import System.Exit
import System.IO
import Data.Functor.Identity
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Token
import Text.Parsec.Combinator
import Text.Parsec.Language
import Text.Parsec.Error

import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Data.Char (chr)

import           Control.Monad
import           Control.Concurrent (threadDelay)
import           Pipes

import System.Console.Haskeline

import SMACCMPilot.Datalink.Client.Async
import SMACCMPilot.Datalink.Client.Opts
import SMACCMPilot.Datalink.Client.Console
import SMACCMPilot.Datalink.Client.Queue
import SMACCMPilot.Datalink.Client.Serial
import SMACCMPilot.Datalink.Client.Monad
import SMACCMPilot.Datalink.Client.ByteString
import SMACCMPilot.Datalink.Client.Pipes

import qualified SMACCMPilot.Datalink.HXStream.Native as HX
import SMACCMPilot.Commsec.Sizes

main :: IO ()
main = do
  argv <- getArgs
  case getOpt' Permute options argv of
    (opts, [], [], []) -> replClient (foldl (flip id) defaultOpts opts)
    (_, nonOpts, unOpts, errs) -> usage ("invalid arguments: "
                                        ++ unwords nonOpts
                                        ++ unwords unOpts
                                        ++ unwords errs)

usage :: String -> IO a
usage errs = do
  hPutStr stderr (usageInfo errs options)
  exitFailure

-------------

replClient :: Options -> IO ()
replClient opts = do
  console <- newConsole opts

  (ser_in_pop, ser_out_push) <- serialServer opts console

  _ <- asyncRunEffect console "serial in"
          $ popProducer ser_in_pop
        >-> hxDecoder
        >-> frameLog
        >-> forever (await >> return ())

  (repl_push, repl_pop) <- newQueue

  _ <- asyncRunEffect console "serial out"
           $ popProducer repl_pop
         >-> bytestringLog "raw"
         >-> pushConsumer ser_out_push

  runInputT defaultSettings $ runEffect $
          repl console motorsOff >-> pushConsumer repl_push
  where
  repl :: Console -> MotorState -> Producer ByteString (InputT IO) ()
  repl console s = do
    -- give concurrent threads time to print to console
    lift $ lift $ threadDelay 100
    c <- lift $ lift $ getConsoleOutput console
    lift $ outputStr c
    minput <- lift (getInputLine "% ")
    case minput of
      Just ":q"    -> return ()
      Just ":quit" -> return ()
      Just ""      -> repl console s
      Just cmd  -> do
        (mresponse, msend, s') <- lift (liftIO (processCmd cmd s))
        case mresponse of
          Just r -> lift (outputStrLn r)
          Nothing -> return ()
        case msend of
          Just b -> yield b
          Nothing -> return ()
        repl console s'
      Nothing   -> repl console s
    where
    loop k = k >> repl console

---------- MANAGING MOTOR STATE -------------

data MotorState =
  MotorState
    { m1 :: Integer
    , m2 :: Integer
    , m3 :: Integer
    , m4 :: Integer
    } deriving (Eq, Show)

motorsOff :: MotorState
motorsOff = MotorState
  { m1 = 0
  , m2 = 0
  , m3 = 0
  , m4 = 0
  }

mkPayload :: MotorState -> ByteString
mkPayload MotorState{..} = framed
  where
  contents       = B.pack ("motors" ++ (map (chr . fromIntegral) [m1, m2, m3, m4]))
  (Right packed) = bytestringPad cyphertextSize contents
  framed         = HX.encode 0 packed

processCmd :: String -> MotorState
           -> IO (Maybe String, Maybe ByteString, MotorState)
processCmd cmd s = case parseCmd cmd of
  Right f -> newstate (f s)
  Left e -> return (Just ("Invalid command: " ++ show e), Nothing, s)
  where
  newstate s' = return (Just ("New output: " ++ show s'), Just (mkPayload s'), s')

-------------- PARSING ------------------

type Parser u a = ParsecT String u Identity a

lexer :: GenTokenParser String u Identity
lexer = makeTokenParser haskellDef

tWhiteSpace :: Parser u ()
tWhiteSpace = (whiteSpace lexer) <?> "whitespace"

tInteger :: Parser u Integer
tInteger = (integer lexer) <?> "integer"

tIntegerInRange :: Integer -> Integer -> Parser u Integer
tIntegerInRange lo hi = do
  n <- tInteger
  if (n < lo || n > hi)
     then fail ("expected integer between " ++ show lo ++ " and " ++ show hi)
     else return n

tWord :: Parser u String
tWord = (many1 (noneOf "()\" \t\n\r")) <?> "word"

tCmd :: Parser u (MotorState -> MotorState)
tCmd = do
  w <- tWord
  case w of
    "off" -> return (const motorsOff)
    "resend" -> return id
    "motor" -> do
      tWhiteSpace
      mnum <- tIntegerInRange 1 4
      tWhiteSpace
      mval <- tIntegerInRange 0 100
      return $ \s -> case mnum of
        1 -> s { m1 = mval }
        2 -> s { m2 = mval }
        3 -> s { m3 = mval }
        4 -> s { m4 = mval }
    _ -> fail "unknown command"

parseCmd :: String -> Either ParseError (MotorState -> MotorState)
parseCmd = runP tCmd () ""


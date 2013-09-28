{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Char (ord)

import Ivory.Language
import Ivory.Stdlib

import Ivory.Tower
import Ivory.Tower.StateMachine
import Ivory.Tower.Frontend

import Ivory.BSP.STM32F4.RCC (BoardHSE)
import qualified Ivory.HW.SearchDir          as HW
import qualified Ivory.BSP.STM32F4.SearchDir as BSP

import Ivory.BSP.STM32F4.UART

import SMACCMPilot.Hardware.GPS.UBlox
import Platform

main :: IO ()
main = compilePlatforms conf (gpsPlatforms app)
  where
  conf = searchPathConf [ HW.searchDir, BSP.searchDir ]

app :: forall p . (GPSUart p, BoardHSE p) => Tower p ()
app = do
  (shelli,shello) <- uartTower (consoleUart (Proxy :: Proxy p)) 115200
  (gpsi,gpso)     <- uartTower (gpsUart     (Proxy :: Proxy p)) 38400
  shell "gps test shell. hard to use? blame pat." shello shelli
  task "ubloxGPS" $ ubloxGPSTask gpso gpsi

shell :: String
      -> ChannelSource 1024 (Stored Uint8)
      -> ChannelSink   128  (Stored Uint8)
      -> Tower p ()
shell greet ostream istream = task "shell" $ do
  out <- withChannelEmitter  ostream "ostream"
  inp <- withChannelEvent    istream "istream"
  withStackSize 1024
  let puts str = mapM_ (\c -> putc (fromIntegral (ord c))) str
      putc c = emitV_ out c

  sm <- stateMachine "motor_shell" $ mdo
    init <- stateNamed "init" $ entry $ do
      liftIvory_ $ puts (greet ++ "\n")
      goto prompt

    prompt <- stateNamed "prompt" $ do
      entry $ liftIvory_ $ puts "> "
      on inp $ \inref -> liftIvory $ do
        i <- deref inref
        putc i -- Echo
        return $ do
          branch (i `isChar` '?')   help
          branch (i `isChar` '\n')  prompt
          goto unknown

    unknown <- stateNamed "unknown" $ do
      entry $ do
        liftIvory_ $ puts "\n\nunknown command"
        goto help

    help <- stateNamed "help" $ do
      entry $ do
        liftIvory_ $ puts helpmsg
        goto prompt

    return init

  taskInit $ begin sm

helpmsg :: String
helpmsg = unlines $
  [ "\n\nHelp:"
  , ""
  , ""
  , "? for help"
  ]

isChar :: Uint8 -> Char -> IBool
isChar b c = b ==? (fromIntegral (ord c))

-- Hacky and unsafe...
itoa :: Uint8 -> Uint8
itoa i = i + (fromIntegral (ord '0'))
atoi :: Uint8 -> Uint8
atoi a = a - (fromIntegral (ord '0'))

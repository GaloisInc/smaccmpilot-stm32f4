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
  (shelli :: ChannelSink 128 (Stored Uint8)
   ,shello :: ChannelSource 128 (Stored Uint8)) <- uartTower
                                                    (consoleUart (Proxy :: Proxy p))
                                                    115200
  (gpsi :: ChannelSink 128 (Stored Uint8)
   ,gpso :: ChannelSource 128 (Stored Uint8)) <- uartTower
                                                  (gpsUart (Proxy :: Proxy p))
                                                  38400
  packetid <- channel
  shell "gps test shell, console." shello shelli (snk packetid)
  task "ubloxGPS" $ ubloxGPSTask gpso gpsi (src packetid)
--  forward gpso shelli
--  forward shello gpsi


forward :: (SingI n, SingI m, IvoryArea a, IvoryZero a)
        => ChannelSource n a
        -> ChannelSink   m a
        -> Tower p ()
forward osrc isnk = task "forward" $ do
  o <- withChannelEmitter  osrc "ostream"
  i <- withChannelEvent    isnk "istream"
  onEvent i (emit_ o)


shell :: (SingI n, SingI m, SingI o)
      => String
      -> ChannelSource n (Stored Uint8)
      -> ChannelSink   m (Stored Uint8)
      -> ChannelSink   o (Stored Uint8)
      -> Tower p ()
shell greet ostream istream igps = task "shell" $ do
  out <- withChannelEmitter  ostream "ostream"
  inp <- withChannelEvent    istream "istream"
  gpsin <- withChannelEvent  igps    "gpsin"
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
          branch (i `isChar` 'f')   forward
          branch (i `isChar` 'r')   rxed
          branch (i `isChar` '\n')  prompt
          goto unknown

    forward <- stateNamed "forward" $ do
      entry $ liftIvory_ $ puts "\nforwarding gps traffic, any key to exit\n"
      on inp $ \_ -> goto prompt
      on gpsin $ \v -> liftIvory_ $ emit_ out v

    rxed <- stateNamed "rxed" $ do
      entry $ liftIvory_ $ puts "\nshowing recieved gps packet ids, any key to exit\n"
      on inp $ \_ -> goto prompt
      on gpsin $ \v -> liftIvory_ $ do
        packetid <- deref v
        puts "# "
        putc (itoa packetid)
        puts "\n"

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
  , "r: show recieved messageids"
  , "f: forward gps traffic to console"
  , "   any key to end forwarding"
  , "? for help"
  ]

isChar :: Uint8 -> Char -> IBool
isChar b c = b ==? (fromIntegral (ord c))

-- Hacky and unsafe...
itoa :: Uint8 -> Uint8
itoa i = i + (fromIntegral (ord '0'))
atoi :: Uint8 -> Uint8
atoi a = a - (fromIntegral (ord '0'))

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Char (ord)

import Ivory.Language
import Ivory.Stdlib

import Ivory.Tower
import Ivory.Tower.StateMachine
import Ivory.Tower.Frontend

import qualified Ivory.HW.SearchDir          as HW
import qualified Ivory.BSP.STM32.SearchDir as BSP

import Ivory.BSP.STM32.Driver.UART
import Ivory.BSP.STM32F405.UART

import Ivory.BSP.STM32.Signalable
import Ivory.BSP.STM32.PlatformClock

import qualified Ivory.BSP.STM32F405.Interrupt as F405

import Platform

main :: IO ()
main = compilePlatforms conf (motorPlatforms app)
  where
  conf = searchPathConf [ HW.searchDir, BSP.searchDir ]

app :: ( RawMotorControl p, PlatformClock p, STM32Signal p
       , InterruptType p ~ F405.Interrupt)
    => Tower p ()
app = do
  c <- channel
  rawMotorControl (snk c)
  (i,o) <- uartTower uart1 115200 (Proxy :: Proxy 128)
  shell "motor control shell. hard to use? blame pat" o i (src c)

shell :: String
      -> ChannelSource (Stored Uint8)
      -> ChannelSink   (Stored Uint8)
      -> ChannelSource (Array 4 (Stored IFloat))
      -> Tower p ()
shell greet ostream istream motorstream = task "shell" $ do
  out <- withChannelEmitter  ostream "ostream"
  inp <- withChannelEvent    istream "istream"
  motorctl <- withChannelEmitter motorstream "motorctl"
  -- withStackSize 1024 -- XXX
  motorNum <- taskLocal "motorNum"
  (throttle :: Ref Global (Array 4 (Stored Uint8))) <- taskLocal "throttle"
  let puts :: (GetAlloc eff ~ Scope s) => [Char] -> Ivory eff ()
      puts str = mapM_ (\c -> putc (fromIntegral (ord c))) str
      putc :: (GetAlloc eff ~ Scope s) => Uint8 -> Ivory eff ()
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
          branch (i `isChar` 'm')   motorset
          branch (i `isChar` 'o')   off
          branch (i `isChar` '?')   help
          branch (i `isChar` '\n')  prompt
          goto unknown
    motorset <- stateNamed "motorset" $ do
      on inp $ \inref -> liftIvory $ do
        i <- deref inref
        putc i -- Echo
        store motorNum ((atoi i) - 1)
        return $ do
          branch (i `isChar` '1')   motorval
          branch (i `isChar` '2')   motorval
          branch (i `isChar` '3')   motorval
          branch (i `isChar` '4')   motorval
          goto unknown
    motorval <- stateNamed "motorval" $ do
      on inp $ \inref -> liftIvory $ do
        i <- deref inref
        putc i -- Echo
        v <- assign (atoi i)
        whitespace <- assign (i `isChar` ' ')
        mNum <- deref motorNum
        valid <- assign ((v >=? 0) .&& (v <=? 9))
        when valid $ do
          store (throttle ! (toIx mNum)) v
        return $ do
          branch valid commit
          branch (iNot whitespace) unknown

    commit <- stateNamed "commit" $ do
      entry $ liftIvory_ $ do
        puts "\noutput throttle is: "
        arrayMap $ \i -> do
          t <- deref (throttle ! i)
          putc (itoa t)
          puts " "
        puts "\ncommit with 'Y'"
      on inp $ \inref -> liftIvory $ do
        i <- deref inref
        return $ do
          branch (i `isChar` 'Y') send
          goto prompt


    off <- stateNamed "off" $ do
      entry $ liftIvory $ do
        arrayMap $ \i -> store (throttle ! i) 0
        puts "\noutput throttle is: "
        arrayMap $ \i -> do
          t <- deref (throttle ! i)
          putc (itoa t)
          puts " "
        puts "\n"
        return $ goto send

    send <- stateNamed "send" $ do
      entry $ liftIvory $ do
        fthr <- local (iarray [])
        arrayMap $ \i -> do
          t <- deref (throttle ! i)
          store (fthr ! i) (mapThr t)
        emit_ motorctl (constRef fthr)
        puts "motors set!\n"
        return $ goto prompt

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

mapThr :: Uint8 -> IFloat
mapThr t = foldr aux 0.0 tbl
 where
 aux (u,v) acc = (u ==? t) ? (v, acc)
 tbl = [(0,0.0)
       ,(1,0.1)
       ,(2,0.2)
       ,(3,0.3)
       ,(4,0.4)
       ,(5,0.5)
       ,(6,0.6)
       ,(7,0.7)
       ,(8,0.8)
       ,(9,1.0)]

helpmsg :: String
helpmsg = unlines $
  [ "\n\nHelp:"
  , "mXY sets motor X in [1..4]  to throttle Y in [0..9]"
  , "o turns motors off"
  , "? for help"
  ]

isChar :: Uint8 -> Char -> IBool
isChar b c = b ==? (fromIntegral (ord c))


-- Hacky and unsafe...
itoa :: Uint8 -> Uint8
itoa i = i + (fromIntegral (ord '0'))
atoi :: Uint8 -> Uint8
atoi a = a - (fromIntegral (ord '0'))

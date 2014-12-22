{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module PX4.Tests.CopterMotors (app) where

import Data.Char (ord)

import Ivory.Language
import Ivory.Stdlib

import Ivory.Tower
import Ivory.Tower.StateMachine

import Ivory.BSP.STM32.Driver.UART

import qualified Ivory.BSP.STM32F405.Interrupt as F405
import qualified BSP.Tests.Platforms as BSP
import PX4.Tests.Platforms

app :: (e -> PX4Platform F405.Interrupt)
    -> Tower e ()
app topx4 = do
  px4platform <- fmap topx4 getEnv
  c <- channel
  px4platform_motorcontrol px4platform tocc (snd c)
  (i,o) <- uartTower tocc (console px4platform) 115200 (Proxy :: Proxy 128)
  shell "motor control shell. hard to use? blame pat" o i (fst c)
  where
  tocc = BSP.testplatform_clockconfig . px4platform_testplatform . topx4
  console = BSP.testUART . BSP.testplatform_uart . px4platform_testplatform

shell :: String
      -> ChanInput (Stored Uint8)
      -> ChanOutput (Stored Uint8)
      -> ChanInput (Array 4 (Stored IFloat))
      -> Tower e ()
shell greet ostream istream motorstream = monitor "shell" $ do

  motorNum <- state "motorNum"
  (throttle :: Ref Global (Array 4 (Stored Uint8))) <- state "throttle"

  let puts :: (GetAlloc eff ~ Scope s) => Emitter (Stored Uint8) -> [Char] -> Ivory eff ()
      puts e str = mapM_ (\c -> putc e (fromIntegral (ord c))) str
      putc :: (GetAlloc eff ~ Scope s) => Emitter (Stored Uint8) -> Uint8 -> Ivory eff ()
      putc e c = emitV e c
  sm <- stateMachine "motor_shell" $ mdo
    init <- machineStateNamed "init" $ timeout (Milliseconds 1) $ do
      e <- machineEmitter ostream (fromIntegral ((length greet) + 1))
      machineControl $ \_ -> do
        puts e (greet ++ "\n")
        return (goto prompt)
    prompt <- machineStateNamed "prompt" $ do
      entry $ do
        e <- machineEmitter ostream 2
        machineCallback $ const $ puts e "> "
      on istream $ do
        e <- machineEmitter ostream 1
        machineControl $ \inref -> do
          i <- deref inref
          putc e i -- Echo
          return $ do
            branch (i `isChar` 'm')   motorset
            branch (i `isChar` 'o')   off
            branch (i `isChar` '?')   help
            branch (i `isChar` '\n')  prompt
            goto unknown
    motorset <- machineStateNamed "motorset" $ do
      on istream $ do
        e <- machineEmitter ostream 1
        machineControl $ \inref -> do
          i <- deref inref
          putc e i -- Echo
          store motorNum ((atoi i) - 1)
          return $ do
            branch (i `isChar` '1')   motorval
            branch (i `isChar` '2')   motorval
            branch (i `isChar` '3')   motorval
            branch (i `isChar` '4')   motorval
            goto unknown
    motorval <- machineStateNamed "motorval" $ do
      on istream $ do
        e <- machineEmitter ostream 1
        machineControl $ \inref -> do
          i <- deref inref
          putc e i -- Echo
          v <- assign (atoi i)
          whitespace <- assign (i `isChar` ' ')
          mNum <- deref motorNum
          valid <- assign ((v >=? 0) .&& (v <=? 9))
          when valid $ do
            store (throttle ! (toIx mNum)) v
          return $ do
            branch valid commit
            branch (iNot whitespace) unknown

    commit <- machineStateNamed "commit" $ do
      entry $ do
        e <- machineEmitter ostream 48
        machineCallback $ \_ -> do
          puts e "\noutput throttle is: "
          arrayMap $ \i -> do
            t <- deref (throttle ! i)
            putc e (itoa t)
            puts e " "
          puts e "\ncommit with 'Y'"
      on istream $ machineControl $ \inref -> do
        i <- deref inref
        return $ do
          branch (i `isChar` 'Y') send
          goto prompt


    off <- machineStateNamed "off" $ do
      entry $ do
        e <- machineEmitter ostream 48
        machineControl $ \_ -> do
          arrayMap $ \i -> store (throttle ! i) 0
          puts e "\noutput throttle is: "
          arrayMap $ \i -> do
            t <- deref (throttle ! i)
            putc e (itoa t)
            puts e " "
          puts e "\n"
          return $ goto send

    send <- machineStateNamed "send" $ do
      entry $ do
        let response = "motors set!\n"
        e <- machineEmitter ostream (fromIntegral (length response))
        motorctl <- machineEmitter motorstream 1
        machineControl $ \_ -> do
          fthr <- local (iarray [])
          arrayMap $ \i -> do
            t <- deref (throttle ! i)
            store (fthr ! i) (mapThr t)
          emit motorctl (constRef fthr)
          puts e "motors set!\n"
          return $ goto prompt

    unknown <- machineStateNamed "unknown" $ do
      entry $ do
        let response = "\n\nunknown command"
        e <- machineEmitter ostream (fromIntegral (length response))
        machineControl $ \_ -> do
          puts e response
          return $ goto help

    help <- machineStateNamed "help" $ do
      entry $ do
        e <- machineEmitter ostream (fromIntegral (length helpmsg))
        machineControl $ \_ -> do
          puts e helpmsg
          return $ goto prompt

    return init

  stateMachine_onChan sm istream

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

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}

import Data.Char (ord)

import Ivory.Language
import Ivory.Stdlib
import Ivory.HW
import Ivory.BitData
import Ivory.Tower
import Ivory.Tower.StateMachine

import Ivory.Tower.Frontend
import qualified Ivory.HW.SearchDir as HW
import qualified Ivory.BSP.STM32F4.SearchDir as BSP

import Ivory.BSP.STM32F4.GPIO

import LEDTower

app :: Tower p ()
app = do
  blinkApp 250  [led1]
  blinkApp 500  [led2]
  blinkApp 1000 [led3]
  blinkApp 2000 [led4]
  where
  led1 = LED pinD12 ActiveHigh
  led2 = LED pinD13 ActiveHigh
  led3 = LED pinD14 ActiveHigh
  led4 = LED pinD15 ActiveHigh

main = compilePlatforms conf [("open407vc", Twr app)]
  where
  conf = searchPathConf [HW.searchDir, BSP.searchDir]


button :: GPIOPin -> Tower p (ChannelSink 1 (Stored IBool))
button pin = do
  c <- channelWithSize
  task "btn" $ do
    e <- withChannelEmitter (src c) "btnstate"
    ctr <- taskLocalInit "changectr" (ival (0 :: Uint32))
    debouncer <- stateMachine "debouncer" $ mdo
                         -- XXX periodically:
      down    <- state $ onTimeout 1 $ liftIvory $ do
                   v <- pinRead pin
                   return $ branch v rising
      rising  <- state $ do
                   -- XXX really, on entering state should have its own
                   -- handler...
                   onTimeout 0 $ liftIvory_ (store ctr 0)
                   -- XXX periodically:
                   onTimeout 1 $ liftIvory $ do
                     v <- pinRead pin
                     c <- deref ctr
                     return $ do
                       branch (iNot v)          down
                       branch (c >=? threshold) risen
      risen   <- state $ onTimeout 0 $ do
                   liftIvory_ (emitV_ e true)
                   goto up
      up      <- state $ onTimeout 1 $ liftIvory $ do
                   v <- pinRead pin
                   return $ branch (iNot v) falling
      falling <- state $ do
                   onTimeout 0 $ liftIvory_ (store ctr 0)
                   onTimeout 1 $ liftIvory $ do
                     v <- pinRead pin
                     c <- deref ctr
                     return $ do
                       branch v                 up
                       branch (c >=? threshold) fallen
      fallen  <- state $ onTimeout 0 $ do
                   liftIvory_ (emitV_ e false)
                   goto down
      return down
    taskInit $ do
      pinEnable pin
      pinSetMode pin gpio_mode_input
      begin debouncer

  return (snk c)
  where threshold = 50

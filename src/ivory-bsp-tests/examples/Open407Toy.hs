{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}

import Ivory.Language
import Ivory.HW.Module (hw_moduledef)

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
  bsnk <- button btn1
  task "buttonControlledLED" $ ledController [led4] bsnk
  where
  led1 = LED pinD12 ActiveHigh
  led2 = LED pinD13 ActiveHigh
  led3 = LED pinD14 ActiveHigh
  led4 = LED pinD15 ActiveHigh
  btn1 = pinC7 -- center button of 4-way rocker switch

main :: IO ()
main = compilePlatforms conf [("open407vc", Twr app)]
  where
  conf = searchPathConf [HW.searchDir, BSP.searchDir]


button :: GPIOPin -> Tower p (ChannelSink 2 (Stored IBool))
button pin = do
  c <- channelWithSize
  task "btn" $ do
    taskModuleDef $ hw_moduledef
    e <- withChannelEmitter (src c) "btnstate"
    ctr <- taskLocalInit "changectr" (ival (0 :: Uint32))
    let inc = do
          c' <- deref ctr
          store ctr (c'+1)
          return (c'+1)
        reset = store ctr 0
        pressed = pinRead pin >>= \v -> return (iNot v)
    debouncer <- stateMachine "debouncer" $ mdo
      down    <- state $ period 1 $ liftIvory $ do
                   v <- pressed
                   return $ branch v rising
      rising  <- state $ do
                   entry $ liftIvory_ reset
                   period 1 $ liftIvory $ do
                     v <- pressed
                     c' <- inc
                     return $ do
                       branch (iNot v)           down
                       branch (c' >=? threshold) risen
      risen   <- state $ entry $ do
                   liftIvory_ (emitV_ e true)
                   goto up
      up      <- state $ period 1 $ liftIvory $ do
                   v <- pressed
                   return $ branch (iNot v) falling
      falling <- state $ do
                   entry $ liftIvory_ reset
                   period 1 $ liftIvory $ do
                     v <- pressed
                     c' <- inc
                     return $ do
                       branch v                  up
                       branch (c' >=? threshold) fallen
      fallen  <- state $ entry $ do
                   liftIvory_ (emitV_ e false)
                   goto down
      return down
    taskInit $ do
      pinEnable pin
      pinSetMode pin gpio_mode_input
      pinSetPUPD pin gpio_pupd_pullup
      begin debouncer

  return (snk c)
  where threshold = 50

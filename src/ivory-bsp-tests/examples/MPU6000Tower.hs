{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module MPU6000Tower where

import Data.Char (ord)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import Ivory.HW.Module
import Ivory.BSP.STM32F4.GPIO
import Ivory.BSP.STM32F4.UART
import Ivory.BSP.STM32F4.UART.Tower
import Ivory.BSP.STM32F4.RCC
import Ivory.BSP.STM32F4.SPI
import Ivory.BSP.STM32F4.SPI.Tower
import Ivory.BSP.STM32F4.Signalable

import Platforms
import LEDTower (ledController)

import qualified MPU6000

mpu6k :: SPIDevice
mpu6k = SPIDevice
  { spiDevPeripheral    = spi1
  , spiDevCSPin         = pinB0
  , spiDevClockHz       = 500000
  , spiDevCSActive      = ActiveLow
  , spiDevClockPolarity = ClockPolarityLow
  , spiDevClockPhase    = ClockPhase1
  , spiDevBitOrder      = MSBFirst
  , spiDevName          = "mpu6k"
  }

app ::  forall p . (ColoredLEDs p, BoardHSE p, STM32F4Signal p) => Tower p ()
app = do
  (uarti, uarto) <- uartTower uart1 115200 (Proxy :: Proxy 128)
  (toSpi, fromSpi) <- spiTower [mpu6k]
  mpu6kCtl toSpi fromSpi uarto

mpu6kCtl :: forall p
        . (BoardHSE p, STM32F4Signal p)
       => ChannelSource (Struct "spi_transaction_request")
       -> ChannelSink   (Struct "spi_transaction_result")
       -> ChannelSource (Stored Uint8) -- Stdout
       -> Tower p ()
mpu6kCtl toDriver fromDriver toDebug = task "mpu6kCtl" $ do
  spiResult <- withChannelEvent fromDriver "fromDriver"
  spiRequest <- withChannelEmitter toDriver "toDriver"
  debugEmitter  <- withChannelEmitter toDebug  "toDebug"
  taskModuleDef $ hw_moduledef
  let putc :: (GetAlloc eff ~ Scope cs) => Char -> Ivory eff ()
      putc c = do
        r <- local (ival (fromIntegral (ord c)))
        emit_ debugEmitter (constRef r)
      puts :: (GetAlloc eff ~ Scope cs) => String -> Ivory eff ()
      puts str = mapM_ putc str
      putdig :: (GetAlloc eff ~ Scope cs) => Uint8 -> Ivory eff ()
      putdig d = do  -- Put an integer between 0 and 10
        r <- local (ival (d + (fromIntegral (ord '0'))))
        emit_ debugEmitter (constRef r)
      platform = (Proxy :: Proxy p)
  (state :: Ref Global (Stored Uint8)) <- taskLocal "state"
  taskInit $ do
    let max_syscall_priority = (12::Uint8)
    store state 0

  whoamiresult <- taskLocal "whoamiresult"
  sensorsresult <- taskLocal "sensorsresult"

  startEvent <- withPeriodicEvent (Milliseconds 1000)
  handle startEvent "startEvent" $ \_ -> do
        s <- deref state
        when (s ==? 0) $ do
          store state 1
          MPU6000.disableI2C spiRequest
          puts "mpu6k disabling i2c\n"
        when (s ==? 2) $ do
          MPU6000.getWhoAmI spiRequest
          puts "get whoami\n"
          store state 3
        when (s ==? 4) $ do
          store state 5
          MPU6000.wake spiRequest
          puts "waking device\n"
        when (s ==? 6) $ do
          store state 7
          MPU6000.setScale spiRequest
          puts "setting gyro scale\n"
        when (s ==? 8) $ do -- begin getsensors
          store state 9 -- Fetching sensors
          MPU6000.getSensors spiRequest
          puts "getsensors\n"

  handle spiResult "spiResult" $ \result -> do
        s <- deref state
        cond_
          [ s ==? 3 ==> do
              refCopy whoamiresult result
              store state (s+1)
          , s <? 9 ==> do
              -- XXX what about basic data validity checks?
              store state (s+1)
          , s ==? 9 ==> do
              refCopy sensorsresult result
              store state 8 -- Ready to grab sensors again.
          , true ==> -- Error
              store state 0
          ]

        res <- deref (result ~> resultcode)
        cond_
          [ res >? 0 ==> do
              puts "transaction error: "
              putdig res
              puts "\n"
              store state 0 -- RESET STATE MACHINE
          , res ==? 0 ==> do
              puts "transaction successful\n"
          ]





{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module SPITower where

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
  , spiDevCSPin         = pinB1
  , spiDevClockHz       = 500000
  , spiDevCSActive      = ActiveLow
  , spiDevClockPolarity = ClockPolarityLow
  , spiDevClockPhase    = ClockPhase1
  , spiDevBitOrder      = MSBFirst
  }

app ::  forall p . (ColoredLEDs p, BoardHSE p, STM32F4Signal p) => Tower p ()
app = do
  (uarti, uarto) <- uartTower uart1 115200 (Proxy :: Proxy 128)
  (toSig, froSig) <- spiTower spi1
  task "spiCtl" $ spiCtl spi1 mpu6k toSig froSig uarto

spiCtl :: forall p
        . (BoardHSE p, STM32F4Signal p)
       => SPIPeriph
       -> SPIDevice
       -> ChannelSource (Struct "spi_transmission")
       -> ChannelSink   (Struct "spi_transaction_result")
       -> ChannelSource (Stored Uint8)
       -> Task p ()
spiCtl spi device toSig froSPIDriver chdbg = do
  eSig   <- withChannelEmitter  toSig   "toSig"
  eDbg   <- withChannelEmitter  chdbg   "chdbg"
  taskModuleDef $ hw_moduledef
  let putc :: (GetAlloc eff ~ Scope cs) => Char -> Ivory eff ()
      putc c = local (ival (fromIntegral (ord c))) >>= \r -> emit_ eDbg (constRef r)
      puts :: (GetAlloc eff ~ Scope cs) => String -> Ivory eff ()
      puts str = mapM_ putc str
      putdig :: (GetAlloc eff ~ Scope cs) => Uint8 -> Ivory eff ()
      putdig d = do  -- Put an integer between 0 and 10
        r <- local (ival (d + (fromIntegral (ord '0'))))
        emit_ eDbg (constRef r)
      platform = (Proxy :: Proxy p)
  (state :: Ref Global (Stored Uint8)) <- taskLocal "state"
  taskInit $ do
    spiInit spi
    let max_syscall_priority = (12::Uint8)
    spiInitISR spi (max_syscall_priority + 1)
    spiDeviceInit device
    store state 0

  startEvent <- withPeriodicEvent (Milliseconds 1000)
  handle startEvent "startEvent" $ \_ -> do
        s <- deref state
        when (s ==? 0) $ do
          store state 1
          MPU6000.getWhoAmI eSig
          spiDeviceBegin platform mpu6k
          puts "initializing mpu6k\n"
        when (s ==? 2) $ do
          store state 3
          MPU6000.disableI2C eSig
          spiDeviceBegin platform mpu6k
          puts "disabling i2c\n"
        when (s ==? 4) $ do
          store state 5
          MPU6000.wake eSig
          spiDeviceBegin platform mpu6k
          puts "waking device\n"
        when (s ==? 6) $ do
          store state 7
          MPU6000.setScale eSig
          spiDeviceBegin platform mpu6k
          puts "setting gyro scale\n"
        when (s ==? 8) $ do -- begin getsensors
          store state 9 -- Fetching sensors
          MPU6000.getSensors eSig
          spiDeviceBegin platform mpu6k
          puts "getsensors\n"

  spiEvent <- withChannelEvent froSPIDriver "fromSpiDriver"
  handle spiEvent "spiEvent" $ \result -> do
        spiDeviceEnd mpu6k
        s <- deref state
        cond_
          [ s <? 9 ==>
              store state (s+1)
          , s ==? 9 ==> do
              comment "XXX do something here with sensor data"
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





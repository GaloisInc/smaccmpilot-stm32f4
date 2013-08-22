{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module SPITower where

import Data.Monoid (mconcat)
import Data.Char (ord)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.HW
import Ivory.BitData

import Ivory.HW.Module
import Ivory.BSP.STM32F4.GPIO
import Ivory.BSP.STM32F4.UART
import Ivory.BSP.STM32F4.SPI
import Ivory.BSP.STM32F4.SPI.Tower

import Ivory.BSP.STM32F4.SPI.Regs
import Ivory.BSP.STM32F4.SPI.Peripheral

import Platforms
import LEDTower (ledController)
import UARTTower (echoPrompt)

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

greeting :: String
greeting = "spi console. 1 to start:"

app ::  forall p . (ColoredLEDs p) => Tower p ()
app = do
  -- Red led : pinB14
  -- Blue led : pinB15

  -- red <- channel
  -- task "redLed"  $ ledController [pinB14] (snk red)
  start  <- channel
  task "blueLed"   $ ledController [blue] (snk start)

  (uarti :: Channel 128 (Stored Uint8)) <- channelWithSize
  (uarto :: Channel 128 (Stored Uint8)) <- channelWithSize
  uartTower uart1 115200 (snk uarti) (src uarto)
  echoPrompt greeting    (src uarti) (snk uarto) (src start)

  (toSig, froSig) <- spiTower spi1
  task   "spiCtl" $ spiCtl    spi1 mpu6k toSig froSig (snk start) (src uarto)
  where
  blue = blueLED (undefined :: p)

spiCtl :: (SingI n, SingI m, SingI o, SingI q)
       => SPIPeriph
       -> SPIDevice
       -> ChannelSource n (Struct "spi_transmission")
       -> ChannelSink   m (Struct "spi_transaction_result")
       -> ChannelSink   o (Stored IBool)
       -> ChannelSource q (Stored Uint8)
       -> Task p ()
spiCtl spi device toSig froSig chStart chdbg = do
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
  (state :: Ref Global (Stored Uint8)) <- taskLocal "state"
  taskInit $ do
    spiInit spi
    spiInitISR spi 191
    spiDeviceInit device
    store state 0

  onChannelV chStart "startSignal" $ \v -> do
        s <- deref state
        when (s ==? 0) $ do
          store state 1
          MPU6000.getWhoAmI eSig
          spiDeviceBegin mpu6k
          puts "\ninitializing mpu6k\n"
        when (s ==? 2) $ do
          store state 3
          MPU6000.disableI2C eSig
          spiDeviceBegin mpu6k
          puts "\ndisabling i2c\n"
        when (s ==? 4) $ do
          store state 5
          MPU6000.wake eSig
          spiDeviceBegin mpu6k
          puts "\nwaking device\n"
        when (s ==? 6) $ do
          store state 7
          MPU6000.setScale eSig
          spiDeviceBegin mpu6k
          puts "\nsetting gyro scale\n"
        when (s ==? 8) $ do -- begin getsensors
          store state 9 -- Fetching sensors
          MPU6000.getSensors eSig
          spiDeviceBegin mpu6k
          puts "\ngetsensors\n"

  onChannel froSig "eventFromSignal" $ \result -> do
        spiDeviceEnd mpu6k
        s <- deref state
        cond_
          [ s <? 9 ==>
              store state (s+1)
          , s ==? 9 ==>
              -- XXX do something with fetched sensors here.
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





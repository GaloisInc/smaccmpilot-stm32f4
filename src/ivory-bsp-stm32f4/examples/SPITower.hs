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

import Ivory.BSP.STM32F4.SPI.Regs
import Ivory.BSP.STM32F4.SPI.Peripheral

import LEDTower (ledController)
import UARTTower (echoPrompt)
import SPITypes

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

app ::  Tower ()
app = do
  -- Red led : pinB14
  -- Blue led : pinB15

  -- red <- channel
  -- task "redLed"  $ ledController [pinB14] (snk red)
  start  <- channel
  task "blueLed"   $ ledController [pinB15] (snk start)

  (uarti :: Channel 128 (Stored Uint8)) <- channelWithSize
  (uarto :: Channel 128 (Stored Uint8)) <- channelWithSize
  uartTower uart1 115200 (snk uarti) (src uarto)
  echoPrompt greeting    (src uarti) (snk uarto) (src start)

  toSig  <- channel
  froSig <- channel
  task   "spiCtl" $ spiCtl spi1 mpu6k (src toSig) (snk froSig) (snk start) (src uarto)
  signal "spiSig" $ spiSig spi1       (snk toSig) (src froSig)

  addDepends spiTowerTypes
  addModule spiTowerTypes

spiCtl :: (SingI n, SingI m, SingI o, SingI p)
       => SPIPeriph
       -> SPIDevice
       -> ChannelSource n (Struct "spi_transmission")
       -> ChannelSink   m (Struct "spi_transaction_result")
       -> ChannelSink   o (Stored IBool)
       -> ChannelSource p (Stored Uint8)
       -> Task ()
spiCtl spi device toSig froSig chStart chdbg = do
  eSig   <- withChannelEmitter  toSig   "toSig"
  rSig   <- withChannelReceiver froSig  "froSig"
  rStart <- withChannelReceiver chStart "chStart"
  eDbg   <- withChannelEmitter  chdbg   "chdbg"
  taskModuleDef $ const hw_moduledef
  taskBody $ \sch -> do
    let putc c = local (ival (fromIntegral (ord c))) >>= \r -> emit_ sch eDbg (constRef r)
        puts str = mapM_ putc str
        putdig d = do  -- Put an integer between 0 and 10
          r <- local (ival (d + (fromIntegral (ord '0'))))
          emit_ sch eDbg (constRef r)
    spiInit spi
    spiInitISR spi 191
    spiDeviceInit device
    (state :: Ref (Stack s) (Stored Uint8)) <- local (ival 0)
    eventLoop sch $ mconcat
      [ onChannelV rStart $ \v -> do
          s <- deref state
          when (s ==? 0) $ do
            store state 1
            MPU6000.getWhoAmI sch eSig
            spiDeviceBegin mpu6k
            puts "\ninitializing mpu6k\n"
          when (s ==? 2) $ do
            store state 3
            MPU6000.disableI2C sch eSig
            spiDeviceBegin mpu6k
            puts "\ndisabling i2c\n"
          when (s ==? 4) $ do
            store state 5
            MPU6000.wake sch eSig
            spiDeviceBegin mpu6k
            puts "\nwaking device\n"
          when (s ==? 6) $ do
            store state 7
            MPU6000.setScale sch eSig
            spiDeviceBegin mpu6k
            puts "\nsetting gyro scale\n"
          when (s ==? 8) $ do -- begin getsensors
            store state 9 -- Fetching sensors
            MPU6000.getSensors sch eSig
            spiDeviceBegin mpu6k
            puts "\ngetsensors\n"

      , onChannel rSig $ \result -> do
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
      ]

spiSig :: (SingI n, SingI m)
       => SPIPeriph
       -> ChannelSink   n (Struct "spi_transmission")
       -> ChannelSource m (Struct "spi_transaction_result")
       -> Signal ()
spiSig spi froCtl toCtl = do
  eCtl <- withChannelEmitter  toCtl "toCtl"
  rCtl <- withChannelReceiver froCtl "froCtl"
  signalName $ spiISRHandlerName spi
  signalModuleDef $ const hw_moduledef
  (activestate :: Ref Global (Stored IBool)) <- signalLocal "activestate"
  (txstate     :: Ref Global (Struct "spi_transmission")) <- signalLocal "txstate"
  (txidx       :: Ref Global (Stored (Ix 128))) <- signalLocal "txidx"
  (rxstate     :: Ref Global (Struct "spi_transaction_result")) <- signalLocal "rxstate"
  (rxleft      :: Ref Global (Stored Uint8)) <- signalLocal "rxleft"
  signalModuleDef $ \_sch -> private $ do
    depend spiTowerTypes
  signalBody $ \sch -> do
    active <- deref activestate
    unless active $ do
      got <- sigReceive sch rCtl txstate
      ifte_ got (do
        expected <- deref (txstate ~> tx_len)
        store txidx  0
        store activestate true
        store rxleft (safeCast expected)
        store (rxstate ~> rx_idx) 0
        ) 
        -- The ISR should only go off when inactive if a new
        -- ctl has been posted to rCtl.
        (postError 1 sch eCtl)

    -- then check if there is a new transaction available on
    -- the rCtl channel
    sr <- getReg (spiRegSR spi)
    cond_
      [ bitToBool (sr #. spi_sr_rxne) ==> do
          -- Got rxinterrupt, so 
          dr <- spiGetDR spi
          remaining <- rxStore rxstate rxleft dr
          -- Check if we have received all of the bytes expected
          ifte_ (remaining >? 0)
            -- If there are bytes remaining, enable the tx interrupt
            (spiSetTXEIE spi) $ do
                -- Otherwise, we're done receiving, so disable the
                -- receive interrupt
                spiClearRXNEIE spi
                -- set the isr state to take a new message again next time
                store activestate false
                -- Send eCtl signal indicating transaction complete.
                transactionComplete rxstate sch eCtl
      , bitToBool (sr #. spi_sr_txe) ==> do
          -- Got tx interrupt: disable it
          spiClearTXEIE spi
          -- Check if we have sent all of the bytes expected
          txremain <- txRemaining txstate txidx
          when (txremain >? 0) $ do
            -- Enable rx interrupt, allowing state machine to continue.
            spiSetRXNEIE spi
            -- Get the next byte, and transmit it.
            outgoing <- txPop txstate txidx
            spiSetDR spi outgoing
      ]

  where
  postError ecode sch ch = do
    r <- local (istruct [ resultcode .= (ival ecode)])
    emit_ sch ch (constRef r)
  transactionComplete r sch ch = do
    store (r ~> resultcode) 0 -- Success
    emit_ sch ch (constRef r)

txRemaining :: Ref s (Struct "spi_transmission") -> Ref s' (Stored (Ix 128))
            -> Ivory eff (Ix 128)
txRemaining  txstate txidx = do
  len <- deref (txstate ~> tx_len)
  idx <- deref txidx
  return (len - idx)

txPop :: Ref s (Struct "spi_transmission") -> Ref s' (Stored (Ix 128))
            -> Ivory eff Uint8
txPop txstate txidx = do
  -- Invariant: txidx <= (txstate ~> tx_len)
  idx <- deref txidx
  v <- deref ((txstate ~> tx_buf) ! idx)
  store txidx (idx + 1)
  return v

rxStore :: Ref s (Struct "spi_transaction_result") -> Ref s (Stored Uint8)
        -> Uint8 -> Ivory eff Uint8
rxStore rxstate rxleft v = do
  offs <- deref (rxstate ~> rx_idx)
  store ((rxstate ~> rx_buf) ! offs) v
  store (rxstate ~> rx_idx) (offs + 1)
  left <- deref rxleft
  store rxleft (left - 1)
  return (left - 1)





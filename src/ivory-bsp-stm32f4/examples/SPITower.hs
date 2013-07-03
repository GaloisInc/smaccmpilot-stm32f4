{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

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

[ivory|
struct spi_transmission
  { tx_buf  :: Array 128 (Stored Uint8)
  ; tx_len  :: Stored (Ix 128)
  }
|]

[ivory|
struct spi_transaction_result
  { resultcode :: Stored Uint8
  ; rx_buf     :: Array 128 (Stored Uint8)
  ; rx_idx     :: Stored (Ix 128)
  }
|]

spiTowerTypes :: Module
spiTowerTypes = package "spiTowerTypes" $ do
  defStruct (Proxy :: Proxy "spi_transmission")
  defStruct (Proxy :: Proxy "spi_transaction_result")

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
    state <- local (ival 0)
    eventLoop sch $ mconcat
      [ onChannel rStart $ \_ -> do
          s <- deref state
          when (s ==? 0) $ do
            store state 1
            mpu6kGetWhoAmI sch eSig
            spiDeviceBegin mpu6k
            puts "\nstart\n"

      , onChannel rSig $ \result -> do
          s <- deref state
          when (s ==? 1) $ do
            spiDeviceEnd mpu6k
            store state 0

          res <- deref (result ~> resultcode)
          (s :: Uint32) <- deref state
          cond_
            [ res >? 0 ==> do
                puts "transaction error: "
                putdig res
                puts "\n"
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
  let unique n = (n ++ (spiISRHandlerName spi))
      isr_act_area :: MemArea (Stored IBool)
      isr_act_area = area (unique "isract") Nothing
      tx_state_area :: MemArea (Struct "spi_transmission")
      tx_state_area = area (unique "txstate") Nothing
      rx_state_area :: MemArea (Struct "spi_transaction_result")
      rx_state_area = area (unique "rxstate") Nothing
      rx_left_area :: MemArea (Stored Uint8)
      rx_left_area = area (unique "rxleft") Nothing
      tx_idx_area :: MemArea (Stored (Ix 128))
      tx_idx_area = area (unique "txidx") Nothing
  signalModuleDef $ \_sch -> private $ do
    depend spiTowerTypes
    defMemArea isr_act_area
    defMemArea tx_state_area
    defMemArea rx_state_area
    defMemArea rx_left_area
    defMemArea tx_idx_area
  signalBody $ \sch -> do
    let activestate = addrOf isr_act_area
        txstate     = addrOf tx_state_area
        rxstate     = addrOf rx_state_area
        rxleft      = addrOf rx_left_area
        txidx       = addrOf tx_idx_area

    active <- deref activestate
    unless active $ do
      got <- sigReceive sch rCtl txstate
      ifte_ got (do
        expected <- deref (txstate ~> tx_len)
        store rxleft (safeCast expected)
        store txidx  0
        store activestate true
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

mpu6kGetWhoAmI :: (SingI n, eff `AllocsIn` s)
               => TaskSchedule
               -> ChannelEmitter n (Struct "spi_transmission")
               -> Ivory eff ()
mpu6kGetWhoAmI sch ch = do
  tx <- local (istruct [ tx_buf .= iarray [ ival (0x75 .| 0x80)
                                          , ival 0 ]
                       , tx_len .= ival 2 ])
  emit_ sch ch (constRef tx)




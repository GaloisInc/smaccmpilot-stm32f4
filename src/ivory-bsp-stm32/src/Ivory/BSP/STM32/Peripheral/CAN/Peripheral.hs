{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
--
-- Peripheral.hs --- CAN peripheral driver for the STM32F4.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.CAN.Peripheral where

import Control.Monad (when, forM_)
import Data.Ratio
import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.Peripheral.CAN.Regs
import Ivory.BSP.STM32.Peripheral.GPIOF4
import Ivory.BSP.STM32.PlatformClock
import Ivory.HW
import Ivory.Language

data CANTXRegs = CANTXRegs
  { canRegTIR      :: BitDataReg CAN_TIR
  , canRegTDTR     :: BitDataReg CAN_TDTR
  , canRegTDLR     :: BitDataReg CAN_TDLR
  , canRegTDHR     :: BitDataReg CAN_TDHR
  }

data CANPeriph i = CANPeriph
  { canRegMCR      :: BitDataReg CAN_MCR
  , canRegMSR      :: BitDataReg CAN_MSR
  , canRegTSR      :: BitDataReg CAN_TSR
  , canRegRF0R     :: BitDataReg CAN_RFR
  , canRegRF1R     :: BitDataReg CAN_RFR
  , canRegIER      :: BitDataReg CAN_IER
  , canRegESR      :: BitDataReg CAN_ESR
  , canRegBTR      :: BitDataReg CAN_BTR
  , canRegTX       :: [CANTXRegs]
  , canRegRI0R     :: BitDataReg CAN_RIR
  , canRegRDT0R    :: BitDataReg CAN_RDTR
  , canRegRDL0R    :: BitDataReg CAN_RDLR
  , canRegRDH0R    :: BitDataReg CAN_RDHR
  , canRegRI1R     :: BitDataReg CAN_RIR
  , canRegRDT1R    :: BitDataReg CAN_RDTR
  , canRegRDL1R    :: BitDataReg CAN_RDLR
  , canRegRDH1R    :: BitDataReg CAN_RDHR
  , canRCCEnable   :: forall eff . Ivory eff ()
  , canRCCDisable  :: forall eff . Ivory eff ()
  , canIntTX       :: i
  , canIntRX0      :: i
  , canIntRX1      :: i
  , canIntSCE      :: i
  , canName        :: String
  }

mkCANPeriph :: Integer -- Base
            -> (forall eff . Ivory eff ()) -- RCC Enable
            -> (forall eff . Ivory eff ()) -- RCC Disable
            -> i -- transmit interrupt
            -> i -- receive FIFO 0 interrupt
            -> i -- receive FIFO 1 interrupt
            -> i -- error/status change interrupt
            -> String -- Name
            -> CANPeriph i
mkCANPeriph base rccen rccdis txint rx0int rx1int sceint n =
  CANPeriph
    { canRegMCR      = reg 0x000 "mcr"
    , canRegMSR      = reg 0x004 "msr"
    , canRegTSR      = reg 0x008 "tsr"
    , canRegRF0R     = reg 0x00C "rf0r"
    , canRegRF1R     = reg 0x010 "rf1r"
    , canRegIER      = reg 0x014 "ier"
    , canRegESR      = reg 0x018 "esr"
    , canRegBTR      = reg 0x01C "btr"
    -- 0x020-0x17F reserved
    , canRegTX       =
      [ CANTXRegs
        { canRegTIR  = reg 0x180 "ti0r"
        , canRegTDTR = reg 0x184 "tdt0r"
        , canRegTDLR = reg 0x188 "tdl0r"
        , canRegTDHR = reg 0x18C "tdh0r" }
      , CANTXRegs
        { canRegTIR  = reg 0x190 "ti1r"
        , canRegTDTR = reg 0x194 "tdt1r"
        , canRegTDLR = reg 0x198 "tdl1r"
        , canRegTDHR = reg 0x19C "tdh1r" }
      , CANTXRegs
        { canRegTIR  = reg 0x1A0 "ti2r"
        , canRegTDTR = reg 0x1A4 "tdt2r"
        , canRegTDLR = reg 0x1A8 "tdl2r"
        , canRegTDHR = reg 0x1AC "tdh2r" }
      ]
    , canRegRI0R     = reg 0x1B0 "ri0r"
    , canRegRDT0R    = reg 0x1B4 "rdt0r"
    , canRegRDL0R    = reg 0x1B8 "rdl0r"
    , canRegRDH0R    = reg 0x1BC "rdh0r"
    , canRegRI1R     = reg 0x1C0 "ri1r"
    , canRegRDT1R    = reg 0x1C4 "rdt1r"
    , canRegRDL1R    = reg 0x1C8 "rdl1r"
    , canRegRDH1R    = reg 0x1CC "rdh1r"
    , canRCCEnable   = rccen
    , canRCCDisable  = rccdis
    , canIntTX       = txint
    , canIntRX0      = rx0int
    , canIntRX1      = rx1int
    , canIntSCE      = sceint
    , canName        = n
    }
  where
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> String -> BitDataReg d
  reg offs name = mkBitDataRegNamed (base + offs) (n ++ "->" ++ name)

loopUntil :: GetBreaks (AllowBreak eff) ~ Break => Ivory (AllowBreak eff) IBool -> Ivory eff ()
loopUntil p = forever $ do
  done <- p
  ifte_ done breakOut (return ())

data BitTiming = BitTiming
  { bit_timing_sample_point_error :: Rational
  , bit_timing_baud_rate_prescaler :: Integer
  , bit_timing_time_segment_1 :: Integer
  , bit_timing_time_segment_2 :: Integer
  } deriving (Show, Eq)

-- compare timings by sample point error, breaking ties by preferring lower clock speeds
instance Ord BitTiming where
  compare a b = compare (view a) (view b)
    where view timing = (bit_timing_sample_point_error timing, negate $ bit_timing_baud_rate_prescaler timing)

legalTimings :: Integer -> Integer -> [BitTiming]
legalTimings pclk bitrate =
  [ BitTiming
    { bit_timing_sample_point_error = abs (sample_after % clocks_per_bit - sample_fraction)
    , bit_timing_baud_rate_prescaler = brp
    , bit_timing_time_segment_1 = t_seg1
    , bit_timing_time_segment_2 = t_seg2
    }
  | let apb_clocks_per_bit = pclk % bitrate
  -- We could compute legal prescaler values more directly, but this
  -- runs at compile-time so it isn't performance-critical. Also, this
  -- is how http://www.bittiming.can-wiki.info/ does it.
  , brp <- [1..1024]
  , let exact_clocks_per_bit = apb_clocks_per_bit / toRational brp
  , let clocks_per_bit = round exact_clocks_per_bit
  -- ensure there are enough time quanta per bit
  -- XXX: this is not specified in the reference manual but http://www.bittiming.can-wiki.info/ claims it's required
  , clocks_per_bit >= 8 && clocks_per_bit <= 25
  -- ensure the prescaled clock is accurate to 0.1%
  , abs (exact_clocks_per_bit - toRational clocks_per_bit) < 0.001
  -- choose ts1 and ts2 to set the sample point
  , let sample_fraction = 0.875
  , let closest_sample_after = round (toRational clocks_per_bit * sample_fraction)
  -- push sample point earlier if necessary to fit in ts1
  , let sample_after = min 17 closest_sample_after
  -- ensure the sample point is still >50% after adjustment
  , sample_after * 2 > clocks_per_bit
  -- ensure ts1 and ts2 fit in the corresponding CAN_BTR fields
  , let t_seg1 = sample_after - 1 -- subtract off one time quantum for SYNC_SEG
  , let t_seg2 = clocks_per_bit - sample_after
  , t_seg1 >= 1 && t_seg1 <= 16
  , t_seg2 >= 1 && t_seg2 <= 8
  ]

canInit :: (STM32Interrupt i, PlatformClock p, GetAlloc eff ~ Scope cs, Break ~ GetBreaks (AllowBreak eff))
        => CANPeriph i -> Integer -> GPIOPin -> GPIOPin -> Proxy p -> Ivory eff ()
canInit periph bitrate rxpin txpin platform = do
  canRCCEnable periph
  forM_ [rxpin, txpin] $ \ p -> do
    pinEnable        p
    pinSetOutputType p gpio_outputtype_pushpull
    pinSetPUPD       p gpio_pupd_none
    pinSetAF         p gpio_af9 -- All CAN peripherals connect to af9
    pinSetMode       p gpio_mode_af

  modifyReg (canRegMCR periph) $ do
    setBit can_mcr_inrq
    clearBit can_mcr_sleep
  loopUntil $ do
    msr <- getReg (canRegMSR periph)
    return $ getBitDataField can_msr_inak msr /=? fromRep 0

  let pclk = clockPClk1Hz $ platformClockConfig platform
  let timings = legalTimings pclk bitrate
  when (null timings) $ fail $ "no legal bxCAN bit timings for " ++ show bitrate ++ "bps with " ++ show pclk ++ "Hz APB clock"

  let best = minimum timings
  modifyReg (canRegBTR periph) $ do
    clearBit can_btr_silm
    clearBit can_btr_lbkm
    setField can_btr_sjw $ fromRep 0
    setField can_btr_ts2 $ fromRep $ fromInteger $ bit_timing_time_segment_2 best - 1
    setField can_btr_ts1 $ fromRep $ fromInteger $ bit_timing_time_segment_1 best - 1
    setField can_btr_brp $ fromRep $ fromInteger $ bit_timing_baud_rate_prescaler best - 1

  modifyReg (canRegMCR periph) $ clearBit can_mcr_inrq

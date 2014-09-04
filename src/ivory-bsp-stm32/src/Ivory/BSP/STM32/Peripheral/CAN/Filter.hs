{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

--
-- Filter.hs --- CAN receive-filter configuration for the STM32F4.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.CAN.Filter where

import Control.Monad (forM_)
import Data.Array
import Ivory.BSP.STM32.Peripheral.CAN.Regs
import Ivory.HW
import Ivory.Language hiding ((!))

data CANPeriphFilters = CANPeriphFilters
  { canRegFMR      :: BitDataReg CAN_FMR
  , canRegFM1R     :: BitDataReg CAN_FM1R
  , canRegFS1R     :: BitDataReg CAN_FS1R
  , canRegFFA1R    :: BitDataReg CAN_FFA1R
  , canRegFA1R     :: BitDataReg CAN_FA1R
  , canRegFiRx32   :: Array (Integer, Integer) (BitDataReg CAN_FiRx32)
  , canRegFiRx16   :: Array (Integer, Integer) (BitDataReg CAN_FiRx16)
  , canRCCEnableF  :: forall eff . Ivory eff ()
  , canRCCDisableF :: forall eff . Ivory eff ()
  }

mkCANPeriphFilters :: Integer -- Base
                   -> (forall eff . Ivory eff ()) -- RCC Enable
                   -> (forall eff . Ivory eff ()) -- RCC Disable
                   -> CANPeriphFilters
mkCANPeriphFilters base rccen rccdis =
  CANPeriphFilters
    { canRegFMR      = reg 0x200 "fmr"
    , canRegFM1R     = reg 0x204 "fm1r"
    -- 0x208 reserved
    , canRegFS1R     = reg 0x20C "fs1r"
    -- 0x210 reserved
    , canRegFFA1R    = reg 0x214 "ffa1r"
    -- 0x218 reserved
    , canRegFA1R     = reg 0x214 "fa1r"
    -- 0x220-0x23F reserved
    , canRegFiRx32   = filterRegs 0x240 ((0, 0), (27, 1)) "32"
    , canRegFiRx16   = filterRegs 0x240 ((0, 0), (27, 1)) "16"
    , canRCCEnableF  = rccen
    , canRCCDisableF = rccdis
    }
  where
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> String -> BitDataReg d
  reg offs name = mkBitDataRegNamed (base + offs) ("can->" ++ name)

  filterRegs :: IvoryIOReg (BitDataRep a) => Integer -> ((Integer, Integer), (Integer, Integer)) -> String -> Array (Integer, Integer) (BitDataReg a)
  filterRegs offs ix width = listArray ix [ reg (offs + 4 * (i * 2 + x)) ("f" ++ show i ++ "r" ++ show x ++ width) | (i, x) <- range ix ]

canFilterInit :: CANPeriphFilters -> Ivory eff ()
canFilterInit periph = do
  -- ensure the CAN master is active
  canRCCEnableF periph

  -- set up one filter for CAN1 and one for CAN2
  modifyReg (canRegFMR periph) $ do
    setBit can_fmr_finit
    setField can_fmr_can2sb $ fromRep 1
  -- put both filters in Identifier Mask mode
  modifyReg (canRegFM1R periph) $ do
    clearBit $ can_fm1r_fbm #> bitIx 0
    clearBit $ can_fm1r_fbm #> bitIx 1
  -- put both filters in Single 32-bit Scale configuration
  modifyReg (canRegFS1R periph) $ do
    setBit $ can_fs1r_fsc #> bitIx 0
    setBit $ can_fs1r_fsc #> bitIx 1
  -- assign matching messages to FIFO 0 in the appropriate CAN1/CAN2 peripheral
  modifyReg (canRegFFA1R periph) $ do
    clearBit $ can_ffa1r_ffa #> bitIx 0
    clearBit $ can_ffa1r_ffa #> bitIx 1
  -- mark all bits as "don't care", so the filter matches any message
  forM_ [0, 1] $ \ i -> forM_ [0, 1] $ \ x ->
    setReg (canRegFiRx32 periph ! (i, x)) clear
  -- activate the new filters; ensure unused entries are disabled
  modifyReg (canRegFA1R periph) $ sequence_
    [ (if i < 2 then setBit else clearBit) (can_fa1r_fact #> bitIx i)
    | i <- [0 .. bitLength (undefined #. can_fa1r_fact) - 1] ]
  -- allow the peripherals to start receiving packets with the new filters
  modifyReg (canRegFMR periph) $ do
    clearBit can_fmr_finit

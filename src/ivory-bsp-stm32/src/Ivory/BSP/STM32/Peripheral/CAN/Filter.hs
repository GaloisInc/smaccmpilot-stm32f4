{-# LANGUAGE DataKinds #-}
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
    , canRegFA1R     = reg 0x21C "fa1r"
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

data CANFilterID32 = CANFilterID32
  { canFilterSTID32 :: Bits 11
  , canFilterEXID32 :: Bits 18
  , canFilterIDE32 :: Bool
  , canFilterRTR32 :: Bool
  }

data CANFilterID16 = CANFilterID16
  { canFilterSTID16 :: Bits 11
  , canFilterEXID16 :: Bits 3
  , canFilterIDE16 :: Bool
  , canFilterRTR16 :: Bool
  }

data CANFilterContents
  = CANFilter16 CANFilterID16 CANFilterID16 CANFilterID16 CANFilterID16
  | CANFilter32 CANFilterID32 CANFilterID32

data CANFilterBankMode = CANFilterMask | CANFilterList

data CANFIFO = CANFIFO0 | CANFIFO1

data CANFilterBank
  = CANFilterBank CANFIFO CANFilterBankMode CANFilterContents

canFilterInit :: CANPeriphFilters -> [CANFilterBank] -> [CANFilterBank] -> Ivory eff ()
canFilterInit periph can1banks can2banks = do
  let banks = zip [0..] $ can1banks ++ can2banks
  if length banks <= 28 then return () else fail "too many filter banks passed to canFilterInit"

  -- ensure the CAN master is active
  canRCCEnableF periph

  modifyReg (canRegFMR periph) $ do
    setBit can_fmr_finit
    setField can_fmr_can2sb $ fromRep $ fromIntegral $ length can1banks

  -- deactivate any existing filters
  modifyReg (canRegFA1R periph) $ sequence_
    [ clearBit $ can_fa1r_fact #> bitIx i | i <- [0..27] ]

  forM_ banks $ \ (i, CANFilterBank _ _ c) -> case c of
    CANFilter16 f1 f2 f3 f4 -> do
      let dual16 x y = do
          setField can_firx16_stid0 $ canFilterSTID16 x
          (if canFilterIDE16 x then setBit else clearBit) can_firx16_ide0
          (if canFilterRTR16 x then setBit else clearBit) can_firx16_rtr0
          setField can_firx16_exid0 $ canFilterEXID16 x
          setField can_firx16_stid1 $ canFilterSTID16 y
          (if canFilterIDE16 y then setBit else clearBit) can_firx16_ide1
          (if canFilterRTR16 y then setBit else clearBit) can_firx16_rtr1
          setField can_firx16_exid1 $ canFilterEXID16 y
      setReg (canRegFiRx16 periph ! (toInteger i, 0)) $ dual16 f1 f2
      setReg (canRegFiRx16 periph ! (toInteger i, 1)) $ dual16 f3 f4
    CANFilter32 f1 f2 -> do
      let single32 x = do
          setField can_firx32_stid $ canFilterSTID32 x
          setField can_firx32_exid $ canFilterEXID32 x
          (if canFilterIDE32 x then setBit else clearBit) can_firx32_ide
          (if canFilterRTR32 x then setBit else clearBit) can_firx32_rtr
      setReg (canRegFiRx32 periph ! (toInteger i, 0)) $ single32 f1
      setReg (canRegFiRx32 periph ! (toInteger i, 1)) $ single32 f2

  modifyReg (canRegFM1R periph) $ sequence_
    [ (case m of CANFilterMask -> clearBit; CANFilterList -> setBit) $ can_fm1r_fbm #> bitIx i
    | (i, CANFilterBank _ m _) <- banks ]

  modifyReg (canRegFFA1R periph) $ sequence_
    [ (case f of CANFIFO0 -> clearBit; CANFIFO1 -> setBit) $ can_ffa1r_ffa #> bitIx i
    | (i, CANFilterBank f _ _) <- banks ]

  modifyReg (canRegFS1R periph) $ sequence_
    [ (case c of CANFilter16{} -> clearBit; CANFilter32{} -> setBit) $ can_fs1r_fsc #> bitIx i
    | (i, CANFilterBank _ _ c) <- banks ]

  -- activate the new filters
  modifyReg (canRegFA1R periph) $ sequence_
    [ setBit $ can_fa1r_fact #> bitIx i | (i, _) <- banks ]

  -- allow the peripherals to start receiving packets with the new filters
  modifyReg (canRegFMR periph) $ do
    clearBit can_fmr_finit

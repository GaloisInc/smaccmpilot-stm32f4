{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
--
-- Peripheral.hs --- I2C peripheral driver for the STM32F4.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32F4.I2C.Peripheral where

import Ivory.Language
import Ivory.HW
import Ivory.BitData

import Ivory.BSP.STM32F4.Interrupt
import Ivory.BSP.STM32F4.GPIO
import Ivory.BSP.STM32F4.MemoryMap
import Ivory.BSP.STM32F4.RCC

import Ivory.BSP.STM32F4.I2C.Regs

data I2CPeriph = I2CPeriph
  { i2cRegCR1      :: BitDataReg I2C_CR1
  , i2cRegCR2      :: BitDataReg I2C_CR2
  , i2cRegOAR1     :: BitDataReg I2C_OAR1
  , i2cRegOAR2     :: BitDataReg I2C_OAR2
  , i2cRegDR       :: BitDataReg I2C_DR
  , i2cRegSR1      :: BitDataReg I2C_SR1
  , i2cRegSR2      :: BitDataReg I2C_SR2
  , i2cRegCCR      :: BitDataReg I2C_CCR
  , i2cRegTRISE    :: BitDataReg I2C_TRISE
  , i2cRegFLTR     :: BitDataReg I2C_FLTR
  , i2cRCCEnable   :: forall eff . Ivory eff ()
  , i2cRCCDisable  :: forall eff . Ivory eff ()
  , i2cIntEvent    :: Interrupt
  , i2cIntError    :: Interrupt
  , i2cName        :: String
  }

mkI2CPeriph :: Integer -- Base
            -> BitDataField RCC_APB1ENR Bit -- RCC Bit
            -> Interrupt -- event interrupt
            -> Interrupt -- error interrupt
            -> String -- Name
            -> I2CPeriph
mkI2CPeriph base rccfield evtint errint n =
  I2CPeriph
    { i2cRegCR1     = reg 0x00 "cr1"
    , i2cRegCR2     = reg 0x04 "cr2"
    , i2cRegOAR1    = reg 0x08 "oar1"
    , i2cRegOAR2    = reg 0x0C "oar2"
    , i2cRegDR      = reg 0x10 "dr"
    , i2cRegSR1     = reg 0x14 "sr1"
    , i2cRegSR2     = reg 0x18 "sr2"
    , i2cRegCCR     = reg 0x1C "ccr"
    , i2cRegTRISE   = reg 0x20 "trise"
    , i2cRegFLTR    = reg 0x24 "fltr"
    , i2cRCCEnable  = rccEnable  regRCC_APB1ENR rccfield
    , i2cRCCDisable = rccDisable regRCC_APB1ENR rccfield
    , i2cIntEvent   = evtint
    , i2cIntError   = errint
    , i2cName       = n
    }
  where
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> String -> BitDataReg d
  reg offs name = mkBitDataRegNamed (base + offs) (n ++ "->" ++ name)

i2c1 :: I2CPeriph
i2c1 = mkI2CPeriph i2c1_periph_base rcc_apb1en_i2c1 I2C1_EV I2C1_ER "i2c1"

i2c2 :: I2CPeriph
i2c2 = mkI2CPeriph i2c3_periph_base rcc_apb1en_i2c2 I2C2_EV I2C2_ER "i2c2"

i2c3 :: I2CPeriph
i2c3 = mkI2CPeriph i2c3_periph_base rcc_apb1en_i2c3 I2C3_EV I2C3_ER "i2c3"

i2cInit :: (BoardHSE p, GetAlloc eff ~ Scope cs)
        => I2CPeriph -> GPIOPin -> GPIOPin -> Proxy p -> Ivory eff ()
i2cInit periph sda scl platform = do
  i2cRCCEnable periph
  pinsetup sda
  pinsetup scl

  -- Reset and clear peripheral
  modifyReg (i2cRegCR1 periph) $ setBit   i2c_cr1_swrst
  modifyReg (i2cRegCR1 periph) $ clearBit i2c_cr1_swrst

  modifyReg (i2cRegCR2 periph) $ do
    setField i2c_cr2_freq (fromRep (42 :: Uint8))

  pclk1 <- getFreqPClk1 platform
  ccr   <- calcHSCCR pclk1
  modifyReg (i2cRegCCR periph) $ do
    setBit i2c_ccr_fastmode
    setBit i2c_ccr_duty
    setField i2c_ccr_ccr (fromRep ccr)

  where
  calcHSCCR :: Uint32 -> Ivory eff Uint16
  calcHSCCR pclk = do
    -- Need to divide clock to use 25 cycles (DUTY mode is 16 low + 9 high)
    -- at 400khz. Clock divider must be at least 1.
    v <- assign $ castWith 0 (pclk `iDiv` (400000 * 25))
    assign ((v <? 1) ? (1, v))

  pinsetup :: GPIOPin -> Ivory eff ()
  pinsetup p = do
    pinEnable        p
    pinSetOutputType p gpio_outputtype_opendrain
    pinSetPUPD       p gpio_pupd_none
    pinSetAF         p gpio_af4 -- All I2C map AFs map to af4
    pinSetMode       p gpio_mode_af


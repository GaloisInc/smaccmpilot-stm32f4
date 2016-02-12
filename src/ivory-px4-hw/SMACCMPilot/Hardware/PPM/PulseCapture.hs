{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module SMACCMPilot.Hardware.PPM.PulseCapture
  ( pulseCaptureTower
  ) where


import Ivory.Language
import Ivory.Tower
import Ivory.HW
import Ivory.Stdlib

import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Peripheral.ATIM18
import Ivory.BSP.STM32.Peripheral.GPIOF4

import SMACCMPilot.Hardware.PPM.PulseCapture.Types

pulseCaptureTower :: (STM32Interrupt i)
                  => (e -> ClockConfig)
                  -> ATIM     -- atim1
                  -> GPIOPin  -- pinA10
                  -> GPIO_AF  -- af_tim1
                  -> i        -- TIM1_CC
                  -> ChanInput ('Struct "pulse_capture")
                  -> Tower e ()
pulseCaptureTower tocc (ATIM {..}) pin af int pulse_capture_chan = do
  cc <- fmap tocc getEnv
  -- time bound is just a guess (as pretty much always)
  isr <- signalUnsafe
            (Interrupt int)
            (Microseconds 250)
            (interrupt_disable int)

  monitor "pulse_capture_timer" $ do
    monitorModuleDef $ hw_moduledef
    handler systemInit "pulse_capture_init" $ callback $ const $ do
      atimRCCEnable
      pinEnable  pin
      pinSetAF   pin af
      pinSetPUPD pin gpio_pupd_pullup
      pinSetMode pin gpio_mode_af

      modifyReg atimRegDIER      $ setBit atim_dier_cc3ie
      modifyReg atimRegCCMR2_ICM $ setField atim_ccmr2_icm_cc3s ccs_mode_in1
      modifyReg atimRegCCER      $ do setBit atim_ccer_cc3e
                                      setBit atim_ccer_cc3p
                                      setBit atim_ccer_cc3np
      modifyReg atimRegCNT       $ setField atim_16_data (fromRep 0)

      let psc = fromInteger (((clockSysClkHz cc) `div` 1000000) - 1)
      modifyReg atimRegPSC       $ setField atim_psc_psc (fromRep psc)

      modifyReg atimRegARR       $ setField atim_16_data (fromRep 0xFFFF)

      interrupt_enable int

      modifyReg atimRegCR1       $ setBit atim_cr1_cen

    has_count <- state "has_count"
    count     <- stateInit "count" (ival (0 :: Uint16))
    handler isr "pulse_capture" $ do
      pc_e <- emitter pulse_capture_chan  1
      callback $ const $ do
        sr <- getReg atimRegSR
        ccr <- (getReg atimRegCCR3)
        let latest = toRep (ccr #. atim_16_data)
        cond_
          [ (bitToBool ( sr #. atim_sr_cc3of)) ==> do
              out <- local (istruct [ width .= ival 0
                                    , missed .= ival true])
              emit pc_e (constRef out)
          , (bitToBool ( sr #. atim_sr_cc3if)) ==> do
              hc <- deref has_count
              ifte_ hc
                (do c <- deref count
                    store count latest
                    out <- local (istruct [ width .= ival (latest - c)
                                          , missed .= ival false ])
                    emit pc_e (constRef out))
                (do store count latest
                    store has_count true)
          ]
        setReg atimRegSR $ do
          clearBit atim_sr_cc3of
          clearBit atim_sr_cc3if
        interrupt_enable int

  towerModule pulseCaptureTypeModule
  towerDepends pulseCaptureTypeModule


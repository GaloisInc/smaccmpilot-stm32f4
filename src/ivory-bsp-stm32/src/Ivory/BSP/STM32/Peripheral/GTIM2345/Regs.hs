{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- GPIORegs.hs --- General Purpose Timer (TIM2 to TIM5) registers.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.GTIM2345.Regs where

import Ivory.Language
import Ivory.BSP.STM32.Peripheral.GTIM2345.RegTypes

[ivory|
 bitdata GTIM_CR1 :: Bits 32 = gtim_cr1
  { _             :: Bits 22
  , gtim_cr1_ckd  :: Bits 2
  , gtim_cr1_arpe :: Bit
  , gtim_cr1_cms  :: Bits 2
  , gtim_cr1_dir  :: Bit
  , gtim_cr1_opm  :: Bit
  , gtim_cr1_urs  :: Bit
  , gtim_cr1_udis :: Bit
  , gtim_cr1_cen  :: Bit
  }
|]

[ivory|
 bitdata GTIM_CR2 :: Bits 32 = gtim_cr2
  { _             :: Bits 25
  , gtim_cr2_ti1s :: Bit
  , gtim_cr2_mms  :: Bits 2
  , gtim_cr2_ccds :: Bit
  , _             :: Bits 3
  }
|]

[ivory|
 bitdata GTIM_SMCR  :: Bits 32 = gtim_smcr
  { _               :: Bits 16
  , gtim_smcr_etp   :: Bit
  , gtim_smcr_ece   :: Bit
  , gtim_smcr_etps  :: Bits 2
  , gtim_smcr_etf   :: Bits 4
  , gtim_smcr_msm   :: Bit
  , gtim_smcr_ts    :: Bits 3
  , _               :: Bit
  , gtim_smcr_sms   :: Bits 3
  }
|]

[ivory|
 bitdata GTIM_DIER  :: Bits 32 = gtim_dier
  { _               :: Bits 17
  , gtim_dier_tde   :: Bit
  , gtim_dier_comde :: Bit
  , gtim_dier_cc4de :: Bit
  , gtim_dier_cc3de :: Bit
  , gtim_dier_cc2de :: Bit
  , gtim_dier_cc1de :: Bit
  , gtim_dier_ude   :: Bit
  , _               :: Bit
  , gtim_dier_tie   :: Bit
  , _               :: Bit
  , gtim_dier_cc4ie :: Bit
  , gtim_dier_cc3ie :: Bit
  , gtim_dier_cc2ie :: Bit
  , gtim_dier_cc1ie :: Bit
  , gtim_dier_uie   :: Bit
  }
|]

[ivory|
 bitdata GTIM_SR    :: Bits 32 = gtim_sr
  { _               :: Bits 19
  , gtim_sr_cc4of   :: Bit
  , gtim_sr_cc3of   :: Bit
  , gtim_sr_cc2of   :: Bit
  , gtim_sr_cc1of   :: Bit
  , _               :: Bits 2
  , gtim_sr_tif     :: Bit
  , _               :: Bit
  , gtim_sr_cc4if   :: Bit
  , gtim_sr_cc3if   :: Bit
  , gtim_sr_cc2if   :: Bit
  , gtim_sr_cc1if   :: Bit
  , gtim_sr_uif     :: Bit
  }
|]

[ivory|
 bitdata GTIM_EGR   :: Bits 32 = gtim_egr
  { _               :: Bits 25
  , gtim_egr_tg     :: Bit
  , _               :: Bit
  , gtim_egr_cc4g   :: Bit
  , gtim_egr_cc3g   :: Bit
  , gtim_egr_cc2g   :: Bit
  , gtim_egr_cc1g   :: Bit
  , gtim_egr_ug     :: Bit
  }
|]

[ivory|
 bitdata GTIM_CCMR1_OCM    :: Bits 32 = gtim_ccmr1_ocm
  { _                      :: Bits 16
  , gtim_ccmr1_ocm_oc2ce   :: Bit
  , gtim_ccmr1_ocm_oc2m    :: CCMRMode
  , gtim_ccmr1_ocm_oc2pe   :: Bit
  , gtim_ccmr1_ocm_oc2fe   :: Bit
  , gtim_ccmr1_ocm_cc2s    :: CCSMode
  , gtim_ccmr1_ocm_oc1ce   :: Bit
  , gtim_ccmr1_ocm_oc1m    :: CCMRMode
  , gtim_ccmr1_ocm_oc1pe   :: Bit
  , gtim_ccmr1_ocm_oc1fe   :: Bit
  , gtim_ccmr1_ocm_cc1s    :: CCSMode
  }
|]

[ivory|
 bitdata GTIM_CCMR1_ICM    :: Bits 32 = gtim_ccmr1_icm
  { _                      :: Bits 16
  , gtim_ccmr1_icm_ic2f    :: Bits 4
  , gtim_ccmr1_icm_ic2psc  :: Bits 2
  , gtim_ccmr1_icm_cc2s    :: CCSMode
  , gtim_ccmr1_icm_ic1f    :: Bits 4
  , gtim_ccmr1_icm_ic1psc  :: Bits 2
  , gtim_ccmr1_icm_cc1s    :: CCSMode
  }
|]

[ivory|
 bitdata GTIM_CCMR2_OCM    :: Bits 32 = gtim_ccmr2_ocm
  { _                      :: Bits 16
  , gtim_ccmr2_ocm_oc4ce   :: Bit
  , gtim_ccmr2_ocm_oc4m    :: CCMRMode
  , gtim_ccmr2_ocm_oc4pe   :: Bit
  , gtim_ccmr2_ocm_oc4fe   :: Bit
  , gtim_ccmr2_ocm_cc4s    :: CCSMode
  , gtim_ccmr2_ocm_oc3ce   :: Bit
  , gtim_ccmr2_ocm_oc3m    :: CCMRMode
  , gtim_ccmr2_ocm_oc3pe   :: Bit
  , gtim_ccmr2_ocm_oc3fe   :: Bit
  , gtim_ccmr2_ocm_cc3s    :: CCSMode
  }
|]

[ivory|
 bitdata GTIM_CCMR2_ICM    :: Bits 32 = gtim_ccmr2_icm
  { _                      :: Bits 16
  , gtim_ccmr2_icm_ic4f    :: Bits 4
  , gtim_ccmr2_icm_ic4psc  :: Bits 2
  , gtim_ccmr2_icm_cc4s    :: CCSMode
  , gtim_ccmr2_icm_ic3f    :: Bits 4
  , gtim_ccmr2_icm_ic3psc  :: Bits 2
  , gtim_ccmr2_icm_cc3s    :: CCSMode
  }
|]

[ivory|
 bitdata GTIM_CCER         :: Bits 32 = gtim_ccer
  { _                      :: Bits 16
  , gtim_ccer_cc4np        :: Bit
  , _                      :: Bit
  , gtim_ccer_cc4p         :: Bit
  , gtim_ccer_cc4e         :: Bit
  , gtim_ccer_cc3np        :: Bit
  , _                      :: Bit
  , gtim_ccer_cc3p         :: Bit
  , gtim_ccer_cc3e         :: Bit
  , gtim_ccer_cc2np        :: Bit
  , _                      :: Bit
  , gtim_ccer_cc2p         :: Bit
  , gtim_ccer_cc2e         :: Bit
  , gtim_ccer_cc1np        :: Bit
  , _                      :: Bit
  , gtim_ccer_cc1p         :: Bit
  , gtim_ccer_cc1e         :: Bit
  }
|]

[ivory|
 bitdata GTIM_PSC          :: Bits 32 = gtim_psc
  { _                      :: Bits 16
  , gtim_psc_psc           :: Bits 16
  }
|]

[ivory|
 bitdata GTIM_32           :: Bits 32 = gtim_32
  { gtim_32_data           :: Bits 32
  }
|]

[ivory|
 bitdata GTIM_16           :: Bits 32 = gtim_16
  { _                      :: Bits 16
  , gtim_16_data           :: Bits 16
  }
|]

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- Regs.hs --- bxCAN peripheral registers for the STM32F4.
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.CAN.Regs where

import Ivory.Language

----------------------------------------------------------------------
-- bxCAN Registers

[ivory|
 bitdata CAN_MCR :: Bits 32 = can_mcr
   { _                  :: Bits 15
   , can_mcr_dbf        :: Bit
   , can_mcr_reset      :: Bit
   , _                  :: Bits 7
   , can_mcr_ttcm       :: Bit
   , can_mcr_abom       :: Bit
   , can_mcr_awum       :: Bit
   , can_mcr_nart       :: Bit
   , can_mcr_rflm       :: Bit
   , can_mcr_txfp       :: Bit
   , can_mcr_sleep      :: Bit
   , can_mcr_inrq       :: Bit
   }

 bitdata CAN_MSR :: Bits 32 = can_msr
   { _                  :: Bits 20
   , can_msr_rx         :: Bit
   , can_msr_samp       :: Bit
   , can_msr_rxm        :: Bit
   , can_msr_txm        :: Bit
   , _                  :: Bits 3
   , can_msr_slaki      :: Bit
   , can_msr_wkui       :: Bit
   , can_msr_erri       :: Bit
   , can_msr_slak       :: Bit
   , can_msr_inak       :: Bit
   }

 bitdata CAN_TSR :: Bits 32 = can_tsr
   { can_tsr_low2       :: Bit
   , can_tsr_low1       :: Bit
   , can_tsr_low0       :: Bit
   , can_tsr_tme2       :: Bit
   , can_tsr_tme1       :: Bit
   , can_tsr_tme0       :: Bit
   , can_tsr_code       :: Bits 2
   , can_tsr_abrq2      :: Bit
   , _                  :: Bits 3
   , can_tsr_terr2      :: Bit
   , can_tsr_alst2      :: Bit
   , can_tsr_txok2      :: Bit
   , can_tsr_rqcp2      :: Bit
   , can_tsr_abrq1      :: Bit
   , _                  :: Bits 3
   , can_tsr_terr1      :: Bit
   , can_tsr_alst1      :: Bit
   , can_tsr_txok1      :: Bit
   , can_tsr_rqcp1      :: Bit
   , can_tsr_abrq0      :: Bit
   , _                  :: Bits 3
   , can_tsr_terr0      :: Bit
   , can_tsr_alst0      :: Bit
   , can_tsr_txok0      :: Bit
   , can_tsr_rqcp0      :: Bit
   }

 -- used for both CAN_RF0R and CAN_RF1R
 bitdata CAN_RFR :: Bits 32 = can_rfr
   { _                  :: Bits 26
   , can_rfr_rfom       :: Bit
   , can_rfr_fovr       :: Bit
   , can_rfr_full       :: Bit
   , _                  :: Bit
   , can_rfr_fmp        :: Bits 2
   }

 bitdata CAN_IER :: Bits 32 = can_ier
   { _                  :: Bits 14
   , can_ier_slkie      :: Bit
   , can_ier_wkuie      :: Bit
   , can_ier_errie      :: Bit
   , _                  :: Bits 3
   , can_ier_lecie      :: Bit
   , can_ier_bofie      :: Bit
   , can_ier_epvie      :: Bit
   , can_ier_ewgie      :: Bit
   , _                  :: Bit
   , can_ier_fovie1     :: Bit
   , can_ier_ffie1      :: Bit
   , can_ier_fmpie1     :: Bit
   , can_ier_fovie0     :: Bit
   , can_ier_ffie0      :: Bit
   , can_ier_fmpie0     :: Bit
   , can_ier_tmeie      :: Bit
   }

 bitdata CAN_ESR :: Bits 32 = can_esr
   { can_esr_rec        :: Bits 8
   , can_esr_tec        :: Bits 8
   , _                  :: Bits 9
   , can_esr_lec        :: Bits 3
   , _                  :: Bit
   , can_esr_boff       :: Bit
   , can_esr_epvf       :: Bit
   , can_esr_ewgf       :: Bit
   }

 bitdata CAN_BTR :: Bits 32 = can_btr
   { can_btr_silm       :: Bit
   , can_btr_lbkm       :: Bit
   , _                  :: Bits 4
   , can_btr_sjw        :: Bits 2
   , _                  :: Bit
   , can_btr_ts2        :: Bits 3
   , can_btr_ts1        :: Bits 4
   , _                  :: Bits 6
   , can_btr_brp        :: Bits 10
   }

 -- used for all three CAN_TIxR
 bitdata CAN_TIR :: Bits 32 = can_tir
   { can_tir_stid       :: Bits 11
   , can_tir_exid       :: Bits 18
   , can_tir_ide        :: Bit
   , can_tir_rtr        :: Bit
   , can_tir_txrq       :: Bit
   }

 -- used for all three CAN_TDTxR
 bitdata CAN_TDTR :: Bits 32 = can_tdtr
   { can_tdtr_time      :: Bits 16
   , _                  :: Bits 7
   , can_tdtr_tgt       :: Bit
   , _                  :: Bits 4
   , can_tdtr_dlc       :: Bits 4
   }

 -- used for all three CAN_TDLxR
 bitdata CAN_TDLR :: Bits 32 = can_tdlr
   { can_tdlr_data3     :: Bits 8
   , can_tdlr_data2     :: Bits 8
   , can_tdlr_data1     :: Bits 8
   , can_tdlr_data0     :: Bits 8
   }

 -- used for all three CAN_TDHxR
 bitdata CAN_TDHR :: Bits 32 = can_tdhr
   { can_tdhr_data7     :: Bits 8
   , can_tdhr_data6     :: Bits 8
   , can_tdhr_data5     :: Bits 8
   , can_tdhr_data4     :: Bits 8
   }

 -- used for both CAN_RIxR
 bitdata CAN_RIR :: Bits 32 = can_rir
   { can_rir_stid       :: Bits 11
   , can_rir_exid       :: Bits 18
   , can_rir_ide        :: Bit
   , can_rir_rtr        :: Bit
   , _                  :: Bit
   }

 -- used for both CAN_RDTxR
 bitdata CAN_RDTR :: Bits 32 = can_rdtr
   { can_rdtr_time      :: Bits 16
   , can_rdtr_fmi       :: Bits 8
   , _                  :: Bits 4
   , can_rdtr_dlc       :: Bits 4
   }

 -- used for both CAN_RDLxR
 bitdata CAN_RDLR :: Bits 32 = can_rdlr
   { can_rdlr_data3     :: Bits 8
   , can_rdlr_data2     :: Bits 8
   , can_rdlr_data1     :: Bits 8
   , can_rdlr_data0     :: Bits 8
   }

 -- used for both CAN_RDHxR
 bitdata CAN_RDHR :: Bits 32 = can_rdhr
   { can_rdhr_data7     :: Bits 8
   , can_rdhr_data6     :: Bits 8
   , can_rdhr_data5     :: Bits 8
   , can_rdhr_data4     :: Bits 8
   }

 bitdata CAN_FMR :: Bits 32 = can_fmr
   { _                  :: Bits 18
   , can_fmr_can2sb     :: Bits 6
   , _                  :: Bits 7
   , can_fmr_finit      :: Bit
   }

 bitdata CAN_FM1R :: Bits 32 = can_fm1r
   { _                  :: Bits 4
   , can_fm1r_fbm       :: BitArray 28 Bit
   }

 bitdata CAN_FS1R :: Bits 32 = can_fs1r
   { _                  :: Bits 4
   , can_fs1r_fsc       :: BitArray 28 Bit
   }

 bitdata CAN_FFA1R :: Bits 32 = can_ffa1r
   { _                  :: Bits 4
   , can_ffa1r_ffa      :: BitArray 28 Bit
   }

 bitdata CAN_FA1R :: Bits 32 = can_fa1r
   { _                  :: Bits 4
   , can_fa1r_fact      :: BitArray 28 Bit
   }

 bitdata CAN_FiRx32 :: Bits 32 = can_firx32
   { can_firx32_stid    :: Bits 11
   , can_firx32_exid    :: Bits 18
   , can_firx32_ide     :: Bit
   , can_firx32_rtr     :: Bit
   , _                  :: Bit
   }

 bitdata CAN_FiRx16 :: Bits 32 = can_firx16
   { can_firx16_stid1   :: Bits 11
   , can_firx16_rtr1    :: Bit
   , can_firx16_ide1    :: Bit
   , can_firx16_exid1   :: Bits 3
   , can_firx16_stid0   :: Bits 11
   , can_firx16_rtr0    :: Bit
   , can_firx16_ide0    :: Bit
   , can_firx16_exid0   :: Bits 3
   }

|]

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Hardware.PX4IO.Types.Regs where

import Ivory.Language
import SMACCMPilot.Comm.Ivory.Types.Px4ioStatus
import SMACCMPilot.Comm.Ivory.Types.Px4ioAlarms

[ivory|
 bitdata PX4IO_STATUS :: Bits 16 = px4io_status
  { _                          :: Bits 3
  , px4io_status_safety_off    :: Bit
  , px4io_status_failsafe      :: Bit
  , px4io_status_init_ok       :: Bit
  , px4io_status_arm_sync      :: Bit
  , px4io_status_mixer_ok      :: Bit
  , px4io_status_raw_pwm       :: Bit
  , px4io_status_fmu_ok        :: Bit
  , px4io_status_rc_sbus       :: Bit
  , px4io_status_rc_dsm        :: Bit
  , px4io_status_rc_ppm        :: Bit
  , px4io_status_rc_ok         :: Bit
  , px4io_status_outputs_armed :: Bit
  , px4io_status_override      :: Bit
  }

 bitdata PX4IO_ALARMS :: Bits 16 = px4io_alarms
  { _                          :: Bits 8
  , px4io_alarms_vservo_fault  :: Bit
  , px4io_alarms_pwm_error     :: Bit
  , px4io_alarms_rc_lost       :: Bit
  , px4io_alarms_fmu_lost      :: Bit
  , px4io_alarms_acc_current   :: Bit
  , px4io_alarms_servo_current :: Bit
  , px4io_alarms_temperature   :: Bit
  , px4io_alarms_vbatt_low     :: Bit
  }
|]

px4ioStatusIval :: Uint16 -> Init (Struct "px4io_status")
px4ioStatusIval v = istruct
  [ safety_off    .= ival (bitToBool (r #. px4io_status_safety_off))
  , failsafe      .= ival (bitToBool (r #. px4io_status_safety_off))
  , init_ok       .= ival (bitToBool (r #. px4io_status_init_ok))
  , arm_sync      .= ival (bitToBool (r #. px4io_status_arm_sync))
  , mixer_ok      .= ival (bitToBool (r #. px4io_status_mixer_ok))
  , raw_pwm       .= ival (bitToBool (r #. px4io_status_raw_pwm))
  , fmu_ok        .= ival (bitToBool (r #. px4io_status_fmu_ok))
  , rc_sbus       .= ival (bitToBool (r #. px4io_status_rc_sbus))
  , rc_dsm        .= ival (bitToBool (r #. px4io_status_rc_dsm))
  , rc_ppm        .= ival (bitToBool (r #. px4io_status_rc_ppm))
  , rc_ok         .= ival (bitToBool (r #. px4io_status_rc_ok))
  , outputs_armed .= ival (bitToBool (r #. px4io_status_outputs_armed))
  , override      .= ival (bitToBool (r #. px4io_status_override))
  ]
  where
  r = fromRep v

px4ioAlarmsIval :: Uint16 -> Init (Struct "px4io_alarms")
px4ioAlarmsIval v = istruct
  [ vservo_fault  .= ival (bitToBool (r #. px4io_alarms_vservo_fault))
  , pwm_error     .= ival (bitToBool (r #. px4io_alarms_pwm_error))
  , rc_lost       .= ival (bitToBool (r #. px4io_alarms_rc_lost))
  , fmu_lost      .= ival (bitToBool (r #. px4io_alarms_fmu_lost))
  , acc_current   .= ival (bitToBool (r #. px4io_alarms_acc_current))
  , servo_current .= ival (bitToBool (r #. px4io_alarms_servo_current))
  , temperature   .= ival (bitToBool (r #. px4io_alarms_temperature))
  , vbatt_low     .= ival (bitToBool (r #. px4io_alarms_vbatt_low))
  ]
  where
  r = fromRep v


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Hardware.PX4IO.Types.Regs where

import Ivory.Language
import SMACCMPilot.Hardware.PX4IO.Types.Request
import SMACCMPilot.Comm.Ivory.Types.Px4ioStatus
import SMACCMPilot.Comm.Ivory.Types.Px4ioAlarms
import SMACCMPilot.Comm.Ivory.Types.RcInput
import SMACCMPilot.Comm.Ivory.Types.TimeMicros

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

px4ioStatusFromReg :: Uint16 -> Ref s ('Struct "px4io_status") -> Ivory eff ()
px4ioStatusFromReg v r = do
  p safety_off     px4io_status_safety_off
  p failsafe       px4io_status_failsafe
  p init_ok        px4io_status_init_ok
  p arm_sync       px4io_status_arm_sync
  p mixer_ok       px4io_status_mixer_ok
  p raw_pwm        px4io_status_raw_pwm
  p fmu_ok         px4io_status_fmu_ok
  p rc_sbus        px4io_status_rc_sbus
  p rc_dsm         px4io_status_rc_dsm
  p rc_ppm         px4io_status_rc_ppm
  p rc_ok          px4io_status_rc_ok
  p outputs_armed  px4io_status_outputs_armed
  p override       px4io_status_override
  where
  p lbl field = store (r ~> lbl) (bitToBool (fromRep v #. field))

px4ioAlarmsFromReg :: Uint16 -> Ref s ('Struct "px4io_alarms") -> Ivory eff ()
px4ioAlarmsFromReg v r = do
  p vservo_fault  px4io_alarms_vservo_fault
  p pwm_error     px4io_alarms_pwm_error
  p rc_lost       px4io_alarms_rc_lost
  p fmu_lost      px4io_alarms_fmu_lost
  p acc_current   px4io_alarms_acc_current
  p servo_current px4io_alarms_servo_current
  p temperature   px4io_alarms_temperature
  p vbatt_low     px4io_alarms_vbatt_low
  where
  p lbl field = store (r ~> lbl) (bitToBool (fromRep v #. field))

rcInputFromRegs :: TimeMicros
                -> Ref s1 ('Struct "px4io_request")
                -> Ref s3 ('Struct "rc_input")
                -> Ivory eff ()
rcInputFromRegs t input_req rc_input = do
  cnt <- deref (input_req ~> regs ! 0)
  i0  <- deref (input_req ~> regs ! 6)
  i1  <- deref (input_req ~> regs ! 7)
  i2  <- deref (input_req ~> regs ! 8)
  i3  <- deref (input_req ~> regs ! 9)
  i4  <- deref (input_req ~> regs ! 10)
  i5  <- deref (input_req ~> regs ! 11)
  store (rc_input ~> valid) (cnt >=? 6)
  store (rc_input ~> roll)     i0
  store (rc_input ~> pitch)    i1
  store (rc_input ~> throttle) i2
  store (rc_input ~> yaw)      i3
  store (rc_input ~> switch1)  i4
  store (rc_input ~> switch2)  i5
  store (rc_input ~> time)     t

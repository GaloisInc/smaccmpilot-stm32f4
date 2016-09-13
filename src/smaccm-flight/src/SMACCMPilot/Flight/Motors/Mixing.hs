{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Motors.Mixing
  ( throttlePassthrough
  , mixer
  ) where

import Ivory.Language
import Ivory.Stdlib

import SMACCMPilot.Flight.Platform
import qualified SMACCMPilot.Comm.Ivory.Types.ControlOutput    as C
import qualified SMACCMPilot.Comm.Ivory.Types.QuadcopterMotors as M

throttlePassthrough :: (GetAlloc eff ~ 'Scope cs)
      => ConstRef s1 ('Struct "control_output")
      -> Ivory eff (ConstRef ('Stack cs) ('Struct "quadcopter_motors"))
throttlePassthrough control = do
  t <- deref (control ~> C.throttle)
  out <- local (istruct [ M.frontleft  .= ival t
                        , M.frontright .= ival t
                        , M.backleft   .= ival t
                        , M.backright  .= ival t
                        ])
  return (constRef out)

idle :: IFloat
idle = 0.15

mixer :: (GetAlloc eff ~ 'Scope cs)
      => FlightMixer
      -> ConstRef s1 ('Struct "control_output")
      -> Ivory eff (ConstRef ('Stack cs) ('Struct "quadcopter_motors"))
mixer fmixer control = do
  throttle <- deref (control ~> C.throttle)
  pitch    <- deref (control ~> C.pitch)
  roll     <- deref (control ~> C.roll)
  yaw      <- deref (control ~> C.yaw)
  out <- axis_mix fmixer throttle pitch roll yaw
  throttle_floor throttle out
  sane_range out
  return (constRef out)

axis_mix :: (GetAlloc eff ~ 'Scope cs)
          => FlightMixer
          -> IFloat -- Throttle
          -> IFloat -- Pitch
          -> IFloat -- Roll
          -> IFloat -- Yaw
          -> Ivory eff (Ref ('Stack cs) ('Struct "quadcopter_motors"))
axis_mix fmixer throttle pitch roll yaw = do
  (fl, fr, bl, br) <- tpry_to_flfrblbr fmixer throttle pitch roll y
  lowbound <- assign $ floor4 fl fr bl br
  hibound  <- assign $ ceil4  fl fr bl br
  adj      <- assign $ motor_adj lowbound hibound
  local (istruct [ M.frontleft  .= ival (fl + adj)
                 , M.frontright .= ival (fr + adj)
                 , M.backleft   .= ival (bl + adj)
                 , M.backright  .= ival (br + adj)
                 ])
  where

  (y, _yextra) = yaw_constrain yaw (imax 0.1 (throttle/3))
  motor_adj :: IFloat -> IFloat -> IFloat
  motor_adj lowbound hibound =
    -- If largest motor is higher than 1.0, drop down so that motor is at 1.0
    -- Note that this may mean the lowest motor slips under idle
    (hibound >? 1.0) ? ((1.0-hibound)
      -- If lowest motor is lower than idle, raise so that motor is at idle
     ,(lowbound <? idle) ? ((idle-lowbound)
       -- If all motors are in bounds, do not adjust
      ,0.0))


tpry_to_flfrblbr :: FlightMixer
                 -> IFloat -- Throttle
                 -> IFloat -- Pitch
                 -> IFloat -- Roll
                 -> IFloat -- Yaw
                 -> Ivory eff (IFloat, IFloat, IFloat, IFloat)
tpry_to_flfrblbr QuadXMixer t pitch roll y = do
  fl <- assign $ t + p + r - y
  fr <- assign $ t + p - r + y
  bl <- assign $ t - p + r + y
  br <- assign $ t - p - r - y
  return (fl, fr, bl, br)
  where
  -- I guess these scale factor numbers should actually be sqrt 2, but they
  -- aren't, and if I changed them now I'd have to retune the pids. This should
  -- be fixed in the future I guess.
  p = 0.75 * pitch
  r = 0.75 * roll

tpry_to_flfrblbr IrisMixer t p r y = do
  front_pitch <- assign $ p * ifloat (sin (deg2rad front_angle_deg))
  front_roll  <- assign $ r * ifloat (cos (deg2rad front_angle_deg))

  rear_pitch  <- assign $ p * ifloat (sin (deg2rad rear_angle_deg))
  rear_roll   <- assign $ r * ifloat (cos (deg2rad rear_angle_deg))

  fl <- assign $ front_moment $ t + front_pitch + front_roll - y
  fr <- assign $ front_moment $ t + front_pitch - front_roll + y
  bl <- assign $ rear_moment  $ t - rear_pitch  + rear_roll  + y
  br <- assign $ rear_moment  $ t - rear_pitch  - rear_roll  - y

  return (fl, fr, bl, br)
  where

  -- These geometries are based on what the px4 project calls "dead
  -- cat".

  -- These are angles forwards and backwards of the center of mass,
  -- respectively. I do not have actual measurements of the iris to base these
  -- on.
  front_angle_deg = 27 -- deg
  rear_angle_deg  = 45 -- deg

  -- I presume the moment arm numbers are derived from the distance the motors
  -- are from the center of mass. I do not have actual measurements of the iris
  -- to base these on.
  front_moment = (* 1.000)
  rear_moment  = (* 0.964)

  deg2rad d = d / 180.0 * pi


yaw_constrain :: IFloat -> IFloat -> (IFloat, IFloat)
yaw_constrain input threshold =
  ( regions threshold           (-1*threshold)      input
  , regions (input - threshold) (input + threshold) 0.0)
  where
  regions over under inside =
    (input >? threshold) ? (over
     ,(input <? (-1*threshold)) ? (under
       ,inside))


throttle_floor :: IFloat -> Ref s ('Struct "quadcopter_motors") -> Ivory eff ()
throttle_floor thr m = do
  when (thr <? idle) $ do
    setidle M.frontleft
    setidle M.frontright
    setidle M.backleft
    setidle M.backright
  where
  setidle :: Label "quadcopter_motors" ('Stored IFloat) -> Ivory eff ()
  setidle lbl = store (m ~> lbl) idle

sane_range :: Ref s ('Struct "quadcopter_motors") -> Ivory eff ()
sane_range i = do
  sane M.frontleft
  sane M.frontright
  sane M.backleft
  sane M.backright
  where
  sane :: Label "quadcopter_motors" ('Stored IFloat) -> Ivory eff ()
  sane lbl = do
    v <- deref r
    cond_
      [ v <? 0.0 ==> store r 0
      , v >? 1.0 ==> store r 1.0
      ]
    where r = i ~> lbl

imin :: IvoryOrd a => a -> a -> a
imin a b = (a <? b)?(a,b)

imax :: IvoryOrd a => a -> a -> a
imax a b = (a >? b)?(a,b)

floor4 :: (IvoryOrd a) => a -> a -> a -> a -> a
floor4 a b c d = imin a (imin b (imin c d))
ceil4 :: (IvoryOrd a) => a -> a -> a -> a -> a
ceil4  a b c d = imax a (imax b (imax c d))

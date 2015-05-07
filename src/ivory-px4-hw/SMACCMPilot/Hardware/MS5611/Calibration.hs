{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMACCMPilot.Hardware.MS5611.Calibration where

import Ivory.Language
import SMACCMPilot.Comm.Ivory.Types.BarometerSample
import SMACCMPilot.Hardware.MS5611.Types
import SMACCMPilot.Time

measurement :: ConstRef s1 (Struct "ms5611_calibration")
            -> ConstRef s2 (Struct "ms5611_sample")
            -> Ref s3 (Struct "barometer_sample")
            -> Ivory eff ()
measurement cal sample meas = do
  c1 <- deref (cal ~> coeff ! 0)
  c2 <- deref (cal ~> coeff ! 1)
  c3 <- deref (cal ~> coeff ! 2)
  c4 <- deref (cal ~> coeff ! 3)
  c5 <- deref (cal ~> coeff ! 4)
  c6 <- deref (cal ~> coeff ! 5)

  d1 <- deref (sample ~> sample_pressure)
  d2 <- deref (sample ~> sample_temperature)

  -- Step 1: calculate temperature
  let -- Its safe to promote d2 to a positive signed number, because
      -- it only uses the low 24 bits of the uint32.
      d2_signed :: Sint32
      d2_signed = signCast d2
      -- It is always safe to promote a Uint16 to a Sint32.
      c5_sint32 :: Sint32
      c5_sint32 = safeCast c5

  (dT :: Sint32) <- assign (d2_signed - (c5_sint32 * (2^(8::Integer))))

  let twentyC :: Sint32
      twentyC = 2000
      -- intermediate value has 41 sig bits, will shift down to fit into 32
      c6_sint64 :: Sint64
      c6_sint64 = safeCast c6
      ts64 :: Sint32 -> Sint64
      ts64 t = (safeCast t * c6_sint64) `iDiv` (2^(23::Integer))
      tempsense :: Sint32 -> Sint32
      tempsense t = castWith 0 (ts64 t)

  (temp :: Sint32) <- assign (twentyC + tempsense dT)

  -- Step 2: calculate temperature compensated pressure
  -- Intermediate values require 41 sig bits
  let c2_sint64 :: Sint64
      c2_sint64 = safeCast c2
      c4_sint64 :: Sint64
      c4_sint64 = safeCast c4
      dT_sint64 :: Sint64
      dT_sint64 = safeCast dT
  (off :: Sint64) <- assign ( (c2_sint64 * 2^(16::Integer))
                            + ((c4_sint64 * dT_sint64) `iDiv` (2^(7::Integer))))
  -- Intermediate values require 41 sig bits
  let c1_sint64 :: Sint64
      c1_sint64 = safeCast c1
      c3_sint64 :: Sint64
      c3_sint64 = safeCast c3
  (sens :: Sint64) <- assign ( (c1_sint64 * 2^(15::Integer))
                             + ((c3_sint64 * dT_sint64) `iDiv` (2^(8::Integer))))

  -- Intermediate values require 58 sig bits
  let d1_sint64 :: Sint64
      d1_sint64 = safeCast d1
      pres_64 = ((((d1_sint64 * sens) `iDiv` (2^(21::Integer)))
                      - off) `iDiv` (2^(15::Integer)))
  (pres :: Sint32) <- assign (castWith 0 pres_64)

  t <- deref (sample ~> sample_time)

  store (meas ~> pressure)    ((safeCast pres) / 100)
  store (meas ~> temperature) ((safeCast temp) / 100)
  store (meas ~> time)        (timeMicrosFromITime t)


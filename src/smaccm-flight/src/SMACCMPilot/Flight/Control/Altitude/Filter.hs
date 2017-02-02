{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module SMACCMPilot.Flight.Control.Altitude.Filter where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

import SMACCMPilot.Flight.Types.MaybeFloat ()

-- | Return the change in a value, filtered by a low pass filter.
--
-- 'freq' and 'dt' must be known at Haskell run-time.
lowPassDeriv :: Float                         -- freq
             -> Float                         -- dt
             -> IFloat                        -- new value
             -> Ref s ('Struct "maybe_float")  -- prev value
             -> Ivory eff IFloat              -- filtered delta
lowPassDeriv freq dt x prev = do
  let rc     = 1.0 / (2.0 * pi * freq)
  let alpha  = ifloat (dt / (rc + dt))
  base      <- getMaybe (constRef prev) x
  dx        <- assign $ alpha * (x - base)
  setJust prev (base + dx)
  return dx

-- | Return a changing value filtered by a low pass filter.
--
-- 'freq' and 'dt' must be known at Haskell run-time.
lowPassFilter :: Float                        -- freq
              -> Float                        -- dt
              -> IFloat                       -- new value
              -> Ref s ('Struct "maybe_float") -- prev value
              -> Ivory eff IFloat             -- filtered value
lowPassFilter freq dt x prev = do
  _ <- lowPassDeriv freq dt x prev
  getMaybe (constRef prev) x

-- | Low pass filter with a dynamic timestep.
lowPassDeriv' :: Float
               -> IFloat
               -> IFloat
               -> Ref s ('Struct "maybe_float")
               -> Ivory eff IFloat
lowPassDeriv' freq dt x prev = do
  rc        <- assign (1.0 / (2.0 * pi * ifloat freq))
  alpha     <- assign (dt / (rc + dt))
  base      <- getMaybe (constRef prev) x
  dx        <- assign $ alpha * (x - base)
  setJust prev (base + dx)
  return (x - base)

-- | Low pass filter with a dynamic timestep.
lowPassFilter' :: Float
               -> IFloat
               -> IFloat
               -> Ref s ('Struct "maybe_float")
               -> Ivory eff IFloat
lowPassFilter' freq dt x prev = do
  rc        <- assign (1.0 / (2.0 * pi * ifloat freq))
  alpha     <- assign (dt / (rc + dt))
  base      <- getMaybe (constRef prev) x
  dx        <- assign $ alpha * (x - base)
  setJust prev (base + dx)
  return (base + dx)

-- Median Filter ---------------------------------------------------------------

data MedianFilter elt =
  MedianFilter
    { mf_init   :: forall eff . Ivory eff ()
    , mf_reset  :: forall eff . Ivory eff ()
    , mf_update :: forall eff . elt -> Ivory eff ()
    , mf_output :: forall eff . Ivory eff elt
    }

monitorMedianFilter :: forall len elt e .
  (ANat len, IvoryOrd elt, IvoryStore elt, IvoryZeroVal elt, Num elt)
  => Proxy len -> Monitor e (MedianFilter elt)
monitorMedianFilter _len = do
  mf_data        <- state "mf_data"
  mf_data_index  <- state "mf_data_index"
  mf_data_sorted <- state "mf_data_sorted"

  let named n = fmap showUnique $ freshname $ "median_filter_" ++ n
  reset_name  <- named "reset"
  update_name <- named "update"
  output_name <- named "output"

  let reset_proc = voidProc reset_name $ body $ do
        store mf_data_index 0
        arrayMap $ \(ix :: Ix len) -> do
          store (mf_data        ! ix) (0 :: elt)
          store (mf_data_sorted ! ix) (0 :: elt)

      update_proc = proc update_name $ \(x :: elt) -> body $ do
        data_ix <- deref mf_data_index
        store (mf_data ! data_ix) x

        -- data index wraps around
        ifte_ (data_ix <? (arrayLen mf_data - 1))
          (store mf_data_index (data_ix + 1))
          (store mf_data_index 0)

        -- bubble sort to keep the looping structure simple
        arrayMap $ \(_outer :: Ix len) -> do
          upTo 0 (arrayLen mf_data_sorted - 2) $ \y -> do
            y0 <- deref (mf_data_sorted ! y)
            y1 <- deref (mf_data_sorted ! (y + 1))
            when (y0 >? y1) $ do
              store (mf_data_sorted ! y)       y1
              store (mf_data_sorted ! (y + 1)) y0

        arrayCopy mf_data_sorted mf_data 0 (arrayLen mf_data)
        retVoid

      output_proc = proc output_name $ body $ do
        let medianIx = arrayLen mf_data `div` 2
        v <- deref (mf_data_sorted ! toIx (fromInteger medianIx :: Sint32))
        ret v

  monitorModuleDef $ do
    incl reset_proc
    incl update_proc
    incl output_proc

  return MedianFilter {
      mf_init   = call_ reset_proc
    , mf_reset  = call_ reset_proc
    , mf_update = call_ update_proc
    , mf_output = call output_proc
    }

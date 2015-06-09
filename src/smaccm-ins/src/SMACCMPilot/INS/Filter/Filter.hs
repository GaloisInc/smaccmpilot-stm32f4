{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.INS.Filter.Filter
  ( Filter(..)
  , SecondOrderFilterCoeffs(..)
  , ivory2ndOrderFilter
  , monitor2ndOrderFilter
  ) where

import Ivory.Language
import Ivory.Tower

data Filter =
  Filter
    { filter_init   :: forall eff . Ivory eff ()
    , filter_sample :: forall eff . IFloat -> Ivory eff ()
    , filter_out    :: forall eff . Ivory eff IFloat
    }

data SecondOrderFilterCoeffs =
  SecondOrderFilterCoeffs
    { a0 :: IFloat
    , a1 :: IFloat
    , a2 :: IFloat
    , b1 :: IFloat
    , b2 :: IFloat
    , gain :: IFloat
    }

monitor2ndOrderFilter :: SecondOrderFilterCoeffs-> Monitor e Filter
monitor2ndOrderFilter bw = do
  n <- freshname ""
  let (f, moddef) = ivory2ndOrderFilter (showUnique n) bw
  monitorModuleDef moddef
  return f

ivory2ndOrderFilter :: String -> SecondOrderFilterCoeffs -> (Filter, ModuleDef)
ivory2ndOrderFilter n SecondOrderFilterCoeffs {..} = (f, moddef)
  where
  f = Filter
    { filter_init = do
        store x_0 0
        store x_1 0
        store y_0 0
        store y_1 0
    , filter_sample = \input -> do
        x0 <- assign (input / gain)
        x2 <- deref x_1
        x1 <- deref x_0
        y2 <- deref y_1
        y1 <- deref y_0
        y0 <- assign (a0 * x0 + a1 * x1 + a2 * x2 + b1 * y1 + b2 * y2)
        store x_0 x0
        store x_1 x1
        store y_0 y0
        store y_1 y1
    , filter_out = deref y_0
    }

  x_0 = addrOf x_0_area
  x_1 = addrOf x_1_area
  y_0 = addrOf y_0_area
  y_1 = addrOf y_1_area

  named s = n ++ "_butter_" ++ s
  x_0_area = area (named "state_x0") Nothing
  x_1_area = area (named "state_x1") Nothing
  y_0_area = area (named "state_y0") Nothing
  y_1_area = area (named "state_y1") Nothing
  moddef = do
    defMemArea x_0_area
    defMemArea x_1_area
    defMemArea y_0_area
    defMemArea y_1_area

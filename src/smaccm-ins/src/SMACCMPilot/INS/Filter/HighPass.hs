{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.INS.Filter.HighPass
  ( monitorHighPass
  , ivoryHighPass
  ) where

import Ivory.Language
import Ivory.Tower
import SMACCMPilot.INS.Filter.Filter

-- Second order IIR with Butterworth coefficients for -3db frequency of omega/5
-- TODO: make cutoff frequency parameterizable
monitorHighPass :: Monitor e Filter
monitorHighPass = do
  n <- freshname "hpf"
  let (f, moddef) = ivoryHighPass (showUnique n)
  monitorModuleDef moddef
  return f

ivoryHighPass :: String -> (Filter, ModuleDef)
ivoryHighPass n = (f, moddef)
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

  -- i dont know how to derive these right now, so i cheated and used
  -- from http://www-users.cs.york.ac.uk/~fisher/cgi-bin/mkfscript
  a0 = 1
  a1 = -2
  a2 = 1
  b1 = 0.3695273774
  b2 = -0.1948147127
  gain = 2.55535034

  x_0 = addrOf x_0_area
  x_1 = addrOf x_1_area
  y_0 = addrOf y_0_area
  y_1 = addrOf y_1_area

  named s = n ++ "_hpf_" ++ s
  x_0_area = area (named "state_x0") Nothing
  x_1_area = area (named "state_x1") Nothing
  y_0_area = area (named "state_y0") Nothing
  y_1_area = area (named "state_y1") Nothing
  moddef = do
    defMemArea x_0_area
    defMemArea x_1_area
    defMemArea y_0_area
    defMemArea y_1_area

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.INS.Filter.Butterworth
  ( lowPassButterworth
  , highPassButterworth
  ) where

import SMACCMPilot.INS.Filter.Filter

-- i dont know how to derive the filter coefficients right now, so i cheated and
-- used http://www-users.cs.york.ac.uk/~fisher/cgi-bin/mkfscript
-- This both filters have a cutoff point of 0.2 omega
lowPassButterworth :: SecondOrderFilterCoeffs
lowPassButterworth = SecondOrderFilterCoeffs
  { a0 = 1
  , a1 = 2
  , a2 = 1
  , b1 = 0.3695273774
  , b2 = -0.1958147127
  , gain = 4.840925170
  }

highPassButterworth :: SecondOrderFilterCoeffs
highPassButterworth = SecondOrderFilterCoeffs
  { a0 = 1
  , a1 = -2
  , a2 = 1
  , b1 = 0.3695273774
  , b2 = -0.1948147127
  , gain = 2.55535034
  }

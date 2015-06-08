{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.INS.Filter.Filter
  ( Filter(..)
  ) where

import Ivory.Language

data Filter =
  Filter
    { filter_init   :: forall eff . Ivory eff ()
    , filter_sample :: forall eff . IFloat -> Ivory eff ()
    , filter_out    :: forall eff . Ivory eff IFloat
    }

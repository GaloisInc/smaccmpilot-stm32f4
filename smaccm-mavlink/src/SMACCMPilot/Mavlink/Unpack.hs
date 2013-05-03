{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

module SMACCMPilot.Mavlink.Unpack where

import Ivory.Language

class MavlinkUnpackableMsg t where
  unpackMsg :: ( Def ('[ Ref s1 (Struct t)
                       , ConstRef s2 (CArray (Stored Uint8))
                       ] :-> ())
                , Uint8 )


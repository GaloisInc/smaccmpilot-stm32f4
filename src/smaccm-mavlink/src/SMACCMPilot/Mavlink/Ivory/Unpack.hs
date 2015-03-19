{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

module SMACCMPilot.Mavlink.Ivory.Unpack where

import Ivory.Language

class MavlinkUnpackableMsg t where
  unpackMsg :: ( Def ('[ Ref s1 (Struct t)
                       , ConstRef s2 (CArray (Stored Uint8))
                       ] :-> ())
                , Uint8 )


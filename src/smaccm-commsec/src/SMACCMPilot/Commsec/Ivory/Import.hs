{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Commsec.Ivory.Import
  ( securePkg_init_enc
  , securePkg_init_dec
  , securePkg_encode
  , securePkg_decode
  , securePkg_header
  -- * Lower-level operations
  , securePkg_zero_enc
  , securePkg_zero_dec
  ) where

import Ivory.Language
import SMACCMPilot.Commsec.Sizes
import SMACCMPilot.Commsec.Ivory.Import.Types
import qualified SMACCMPilot.Commsec.Ivory.Error as E

securePkg_header :: String
securePkg_header = "commsec.h"

securePkg_init_enc :: Def ('[ Ref s1 (Struct "commsec_encode")
                            , Uint64          -- Encode Salt
                            , Ref s2 KeyArray -- Encode Key
                            ] :-> ())
securePkg_init_enc = importProc "securePkg_init_enc" securePkg_header

securePkg_init_dec :: Def ('[ Ref s1 (Struct "commsec_decode")
                            , Uint64          -- Decode Salt
                            , Ref s2 KeyArray -- Decode Key
                            ] :-> ())
securePkg_init_dec = importProc "securePkg_init_dec" securePkg_header

securePkg_encode :: Def ('[Ref s1 (Struct "commsec_encode")
                          , ConstRef s2 PlaintextArray
                          , Ref s3 CyphertextArray
                          ] :-> E.CommsecError)
securePkg_encode = importProc "securePkg_encode" securePkg_header

securePkg_decode :: Def ('[Ref s1 (Struct "commsec_decode")
                          , ConstRef s2 CyphertextArray
                          , Ref s3 PlaintextArray
                          ] :-> E.CommsecError)
securePkg_decode = importProc "securePkg_decode" securePkg_header

securePkg_zero_enc :: Def ('[ Ref s1 (Struct "commsec_encode")
                            ] :-> ())
securePkg_zero_enc = importProc "securePkg_zero_enc" securePkg_header

securePkg_zero_dec :: Def ('[ Ref s1 (Struct "commsec_decode")
                            ] :-> ())
securePkg_zero_dec = importProc "securePkg_zero_dec" securePkg_header


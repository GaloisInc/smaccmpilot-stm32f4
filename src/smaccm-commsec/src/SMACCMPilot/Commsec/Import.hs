{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Commsec.Import
  ( securePkg_init_enc
  , securePkg_init_dec
  , securePkg_enc_in_place
  , securePkg_dec
  , securePkg_zero_enc
  , securePkg_zero_dec
  , securePkg_header
  ) where

import Ivory.Language
import qualified SMACCMPilot.Communications  as C
import SMACCMPilot.Commsec.Import.Types
import qualified SMACCMPilot.Commsec.Error as E

securePkg_header :: String
securePkg_header = "commsec.h"

securePkg_init_enc :: Def ('[ Ref s1 (Struct "commsec_encode")
                            , Uint32          -- my ID
                            , Uint32          -- Encode Salt
                            , Ref s2 KeyArray -- Encode Key
                            ] :-> ())
securePkg_init_enc = importProc "securePkg_init_enc" securePkg_header

securePkg_init_dec :: Def ('[ Ref s1 (Struct "commsec_decode")
                            , Uint32          -- Decode Salt
                            , Ref s2 KeyArray -- Decode Key
                            ] :-> ())
securePkg_init_dec = importProc "securePkg_init_dec" securePkg_header

securePkg_enc_in_place :: Def ('[ Ref s1 (Struct "commsec_encode")
                                , Ref s2 C.CyphertextArray
                                , Uint32 -- Message start index (into cyphertext array)
                                , Uint32 -- Message length
                                ] :-> E.CommsecError)
securePkg_enc_in_place = importProc "securePkg_enc_in_place" securePkg_header

securePkg_dec :: Def ('[ Ref s1 (Struct "commsec_decode")
                       , Ref s2 C.CyphertextArray
                       , Uint32 -- Message length
                       ] :-> E.CommsecError)
securePkg_dec = importProc "securePkg_dec" securePkg_header

securePkg_zero_enc :: Def ('[ Ref s1 (Struct "commsec_encode")
                            ] :-> ())
securePkg_zero_enc = importProc "securePkg_zero_enc" securePkg_header

securePkg_zero_dec :: Def ('[ Ref s1 (Struct "commsec_decode")
                            ] :-> ())
securePkg_zero_dec = importProc "securePkg_zero_dec" securePkg_header


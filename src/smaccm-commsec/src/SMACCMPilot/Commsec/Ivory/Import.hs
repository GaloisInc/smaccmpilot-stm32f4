{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Commsec.Ivory.Import
  ( -- * Key Exchange Context Structure Management
      gke_init, gke_new_partner, gke_reset_ctx, gke_clear_ctx
    -- * Key exchange message production and ingest
    , gke_initiate_sts, gke_respond_sts, gke_response_ack_sts, gke_finish_sts
    -- * Symmetric 'steady-state' operation
    , gec_init_key, gec_init_keys, gec_encrypt, gec_decrypt, gec_zero_key
    -- * Types
    , PublicKey, PrivateKey, E.GecError, GecStsCtx, GecSymKey
  ) where

import Ivory.Language
import SMACCMPilot.Commsec.Sizes
import SMACCMPilot.Commsec.Ivory.Import.Types ()
import qualified SMACCMPilot.Commsec.Ivory.Error as E


gecKe_header :: String
gecKe_header = "gec-ke.h"

gec_header :: String
gec_header = "gec.h"

type GecStsCtx = Struct "gec_sts_ctx"
type GecSymKey = Struct "gec_sym_key"

--------------------------------------------------------------------------------
-- Key Exchange Functions
--------------------------------------------------------------------------------

-- @gek_init result meP meQ themP@  Constructs an initial state useful for key negotiation
-- between two particular parties 
gke_init :: Def ('[ Ref s4 (Struct "gec_sts_ctx")
                  , ConstRef s1 PublicKey
                  , ConstRef s2 PrivateKey
                  , ConstRef s3 PublicKey
                  ] :-> ())
gke_init = importProc "init_context" gecKe_header

-- | Using a previously initialized struct, reset the state and select a new
-- remote party for key exchange.
gke_new_partner :: Def ('[ Ref s4 (Struct "gec_sts_ctx")
                           , ConstRef s1 PublicKey
                           ] :-> ())
gke_new_partner = importProc "reset_partner" gecKe_header

-- | Reset the context for use in key re-negotiation with the same partner as
-- was previously set.
gke_reset_ctx :: Def ('[ Ref s1 (Struct "gec_sts_ctx")
                       ] :-> ())
gke_reset_ctx = importProc "reset_ctx" gecKe_header

-- | Zero out the memory of the structure.
gke_clear_ctx :: Def ('[ Ref s1 (Struct "gec_sts_ctx")
                       ] :-> ())
gke_clear_ctx = importProc "clear_ctx" gecKe_header


-- | Produces the first of three messages for STS key agreement and updates
-- the context.
gke_initiate_sts :: Def ('[ Ref s1 GecKeMessage1
                          , Ref s2 (Struct "gec_sts_ctx")
                          , ConstRef s3 GecKeRandomData
                          ] :-> E.GecError)
gke_initiate_sts = importProc "initiate_sts" gecKe_header

-- | Produces the second of three messages for STS key agreement and
-- updates the context.
gke_respond_sts :: Def ('[ ConstRef s1 GecKeMessage1
                         , Ref s2 GecKeMessage2
                         , Ref s3 (Struct "gec_sts_ctx")
                         , ConstRef s4 GecKeRandomData
                         ] :-> E.GecError)
gke_respond_sts = importProc "respond_sts" gecKe_header

-- | Produces the third of three messages for STS key agreement and
-- updates the context.  Also produces the resulting key material.
gke_response_ack_sts :: Def ('[ ConstRef s1 GecKeMessage2
                              , Ref s2 GecKeMessage3
                              , Ref s3 (Struct "gec_sts_ctx")
                              , Ref s4 KeyMaterial
                              ] :-> E.GecError)
gke_response_ack_sts = importProc "response_ack_sts" gecKe_header

-- | Consumes the third STS message, producing the resulting key material.
gke_finish_sts :: Def ('[ ConstRef s1 GecKeMessage3
                        , Ref s2 (Struct "gec_sts_ctx")
                        , Ref s3 KeyMaterial
                        ] :-> E.GecError)
gke_finish_sts = importProc "finish_sts" gecKe_header


--------------------------------------------------------------------------------
-- Symmetric Encryption Functions (for use in the 'steady-state')
--------------------------------------------------------------------------------

gec_init_key :: Def ('[ Ref s1 (Struct "gec_sym_key")
                      , ConstRef s2 KeyAndSaltArray
                      ] :-> E.GecError)
gec_init_key = importProc "gec_init_sym_key_conf_auth" gec_header

gec_init_keys :: Def ('[ Ref s1 (Struct "gec_sym_key")
                        , Ref s2 (Struct "gec_sym_key")
                        , ConstRef s3 KeyMaterial
                        ] :-> E.GecError)
gec_init_keys = importProc "gec_key_material_to_2_channels" gec_header

gec_encrypt :: Def ('[ Ref s1 (Struct "gec_sym_key")
                     , ConstRef s2 PlaintextArray
                     , Ref s3 CyphertextArray
                     ] :-> E.GecError)
gec_encrypt = importProc "gec_encrypt" gec_header

gec_decrypt :: Def ('[ Ref s1 (Struct "gec_sym_key")
                     , ConstRef s2 CyphertextArray
                     , Ref s3 PlaintextArray
                     ] :-> E.GecError)
gec_decrypt = importProc "gec_decrypt" gec_header

gec_zero_key :: Def ('[ Ref s1 (Struct "gec_sym_key")
                      ] :-> ())
gec_zero_key = importProc "gec_clear" gec_header

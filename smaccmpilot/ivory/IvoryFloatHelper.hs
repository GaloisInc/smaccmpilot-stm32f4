
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module IvoryFloatHelper where

import Ivory.Language

ivoryFloatHelperModule :: Module
ivoryFloatHelperModule = package "ivory_float_helper" $ do
  incl floatToInt16
  incl floatToUInt16
  incl int32ToFloat

floatToInt16 :: Def ('[ IFloat ] :-> Sint16 )
floatToInt16 = importProc "float_to_int16" "smaccmpilot/float_helper_impl"

floatToUInt16 :: Def ('[ IFloat ] :-> Uint16 )
floatToUInt16 = importProc "float_to_uint16" "smaccmpilot/float_helper_impl"

int32ToFloat :: Def ('[ Sint32 ] :-> IFloat )
int32ToFloat = importProc "int32_to_float" "smaccmpilot/float_helper_impl"

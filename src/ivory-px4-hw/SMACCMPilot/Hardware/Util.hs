{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SMACCMPilot.Hardware.Util where

import Ivory.Language

class (IvoryBits unsigned, Num unsigned, IvoryEq unsigned, IvoryExpr signed, SignCast unsigned signed, Num signed) => TwosComplement unsigned signed | signed -> unsigned, unsigned -> signed where
  twosComplement :: unsigned -> signed
  twosComplement v = ((v `iShiftR` fromIntegral (iBitSize v - 1)) ==? 1) ?
    ( negate (signCast (iComplement v)) - 1
    , signCast v)

instance TwosComplement Uint8 Sint8
instance TwosComplement Uint16 Sint16
instance TwosComplement Uint32 Sint32
instance TwosComplement Uint64 Sint64

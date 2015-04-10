
module SMACCMPilot.Commsec.Native.Error
  ( GecError(..)
  ) where

import qualified Data.ByteString as B

data GecError
  = CounterTooBig
  | CounterTooOld
  | UnknownID Int
  | BadTag B.ByteString B.ByteString
  | TooShort Int
  deriving (Eq, Show)


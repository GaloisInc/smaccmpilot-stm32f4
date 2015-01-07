
module SMACCMPilot.Commsec.Native.Error
  ( CommsecError(..)
  ) where

import qualified Data.ByteString as B

data CommsecError
  = CounterTooBig
  | CounterTooOld
  | UnknownID Int
  | BadTag B.ByteString B.ByteString
  | TooShort Int
  deriving (Eq, Show)


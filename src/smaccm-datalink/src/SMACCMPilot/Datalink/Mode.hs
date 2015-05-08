
module SMACCMPilot.Datalink.Mode where

import Ivory.Tower.Config
import SMACCMPilot.Commsec.SymmetricKey

data DatalinkMode
  = PlaintextMode
  | SymmetricCommsecMode SymmetricKey

datalinkModeParser :: ConfigParser DatalinkMode
datalinkModeParser = do
  ptmode <- subsection "args" $ subsection "plaintext_mode" $ bool
  case ptmode of
    True -> return PlaintextMode
    False -> fmap SymmetricCommsecMode symmetricKeyParser


module SMACCMPilot.Datalink.Client.Mode where

import Ivory.Tower.Config
import SMACCMPilot.Commsec.SymmetricKey

data ClientMode
  = PlaintextMode
  | SymmetricCommsecMode SymmetricKey

datalinkClientModeParser :: ConfigParser ClientMode
datalinkClientModeParser = do
  ptmode <- subsection "args" $ subsection "plaintext_mode" $ bool
  case ptmode of
    True -> return PlaintextMode
    False -> fmap SymmetricCommsecMode symmetricKeyParser

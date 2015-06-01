
module SMACCMPilot.Datalink.Mode where

import Ivory.Tower.Config
import SMACCMPilot.Commsec.SymmetricKey
import SMACCMPilot.Commsec.KeyExchange

data DatalinkMode
  = PlaintextMode
  | SymmetricCommsecMode DatalinkRole SymmetricKey
  | KeyExchangeMode      DatalinkRole PubKey PrivKey PubKey
  deriving (Eq, Show)

data DatalinkRole
  = DatalinkClient
  | DatalinkServer
  deriving (Eq, Show)

datalinkModeParser :: DatalinkRole -> ConfigParser DatalinkMode
datalinkModeParser role = do
  ptmode <- subsection "args" $ subsection "plaintext_mode" $ bool
  case ptmode of
    True -> return PlaintextMode
    False -> keyExchangeModeParser role
         <|> fmap (SymmetricCommsecMode role) symmetricKeyParser
         <?> "key_exchange or symmetric_key"

keyExchangeModeParser :: DatalinkRole -> ConfigParser DatalinkMode
keyExchangeModeParser DatalinkClient = subsection "key_exchange" $ do
  (c_pub,c_priv) <- subsection "client" keyPairParser
  s_pub <- subsection "server" pubKeyParser
  return $ KeyExchangeMode DatalinkClient c_pub c_priv s_pub
keyExchangeModeParser DatalinkServer = subsection "key_exchange" $ do
  (s_pub,s_priv) <- subsection "server" keyPairParser
  c_pub <- subsection "client" pubKeyParser
  return $ KeyExchangeMode DatalinkServer s_pub s_priv c_pub


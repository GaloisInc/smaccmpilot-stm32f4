{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.GCS.HIL where

import Ivory.Language
import Ivory.Tower

hilTranslator :: (SingI n, SingI m)
              => ChannelSink   n (Struct "hil_state_msg")
              -> ChannelSource m (Struct "sensors_result")
              -> DataSource      (Struct "position_result")
              -> Task p ()
hilTranslator hil sens pos = do
  return () -- XXX


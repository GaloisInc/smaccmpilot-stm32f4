{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Recovery
  ( recoveryTower
  ) where

-- | Tower-tasks managing fault/attack recovery mechanisms.

import qualified SMACCMPilot.Mavlink.Messages.VehCommsec as V
--import qualified SMACCMPilot.Communications              as Comm

import           Control.Monad

import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Tower

--------------------------------------------------------------------------------

recoveryTower :: DataSink (Struct "veh_commsec_msg") -- From Commsec/Decrypt task
              -> Tower p ()
recoveryTower commsec_info_snk = do

  -- XXX We might have multiple recovery tasks here doing multiple things.  For
  -- now, we just have one task, looking at commsec messages.
  task "commsecRecoveryTask" $ commsecRecoveryTask commsec_info_snk

commsecRecoveryTask :: DataSink (Struct "veh_commsec_msg") -- From Commsec/Decrypt task
                    -> Task p ()
commsecRecoveryTask commsec_info_snk = do
  commsec_info_snk_rx <- withDataReader commsec_info_snk "commsec_info_snk"

  onPeriod 20 $ \_now -> do
    commsecReader <- local izero
    readData commsec_info_snk_rx commsecReader
    void $ commsecMonitor commsecReader

  taskModuleDef $ do
    depend V.vehCommsecModule

  where
  -- XXX let's make up an arbitrary monitor here.  If we're received more than
  -- 20 bad messages in less than 30 seconds.
  commsecMonitor rx = do
    currBadMsgs <- rx ~>* V.bad_msgs
    return currBadMsgs

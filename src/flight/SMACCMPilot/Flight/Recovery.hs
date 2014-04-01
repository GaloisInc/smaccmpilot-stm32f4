{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Flight.Recovery
  ( recoveryTower
  ) where

-- | Tower-tasks managing fault/attack recovery mechanisms.

import qualified SMACCMPilot.Mavlink.Messages.VehCommsec as V
--import qualified SMACCMPilot.Communications              as Comm

import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Tower
import qualified SMACCMPilot.Flight.Types.CommsecStatus as C

--------------------------------------------------------------------------------

recoveryTower :: ChannelSink   (Struct "veh_commsec_msg") -- Commsec/Decrypt task
              -> ChannelSource (Stored C.CommsecStatus)  -- To GCS Transmit
              -> Tower p ()
recoveryTower commsec_info_snk monitor_result_src =

  -- XXX We might have multiple recovery tasks here doing multiple things.  For
  -- now, we just have one task, looking at commsec messages.
  task "commsecRecoveryTask"
    $ commsecRecoveryTask commsec_info_snk monitor_result_src

--------------------------------------------------------------------------------

type Idx    = Sint32
type BufLen = 30
type CirBuf = Array BufLen (Stored ITime)

alarm_threshold :: ITime
alarm_threshold = fromIMilliseconds (40000 :: Sint64)
--------------------------------------------------------------------------------

-- | True is OK, False is an alarm.
commsecRecoveryTask :: ChannelSink   (Struct "veh_commsec_msg")
                    -> ChannelSource (Stored C.CommsecStatus)
                    -> Task p ()
commsecRecoveryTask commsec_info_snk monitor_result_src = do
  commsec_info_snk_rx <- withChannelReader commsec_info_snk "commsec_info_snk"
  monitor_res_tx      <- withChannelEmitter monitor_result_src "comm_mon_res"

  -- For our property, we'll store timestamps in an array.
  cirBuf   <- taskLocalInit "cirBuf"  (iarray [] :: Init CirBuf)
  -- Points to the last written-to cell.
  idx      <- taskLocalInit "idx"     (ival 0 :: Init (Stored Idx))
  -- Have we filled the buffer once (at which point, "mod" semantics are used).
  bufFull  <- taskLocalInit "bufFull" (ival false)
  -- Number bad seen on last dataport read.
  numBad   <- taskLocalInit "numBad"  (ival 0 :: Init (Stored Uint32))
  -- Property result
  result   <- taskLocalInit "result" (ival C.secure)

  onPeriod (Milliseconds 20) $ \_now -> do
    commsecReader <- local izero
    _ <- chanRead commsec_info_snk_rx commsecReader
    commsecMonitor commsecReader cirBuf idx bufFull numBad result
    emit_ monitor_res_tx (constRef result)

  taskModuleDef $ do
    depend V.vehCommsecModule

  where
  -- XXX let's make up an arbitrary monitor here.  If we're received more than
  -- 10 bad messages in less than 20 seconds.
  commsecMonitor rx cirBuf idx bufFullRef prevBadRef resRef = do
    totalBadMsgs <- rx ~>* V.bad_msgs
    lastTime     <- getTime
    prevBad      <- deref prevBadRef
    let currBad  = totalBadMsgs - prevBad
    store prevBadRef totalBadMsgs
    -- Store new values and check properties
    newTimeStamps currBad lastTime idx cirBuf bufFullRef resRef

  newTimeStamps numBadVals t idxRef arr bufFullRef resRef =
    -- For each bad value, we'll store the current timestamp, if we missed
    -- capturing it.
    times (toIx numBadVals :: Ix BufLen)
      $ const
      $ do res <- deref resRef
           -- If res has already failed, do nothing.
           when (res ==? C.secure)
              (newTimeStamp t idxRef arr bufFullRef >>= store resRef)

  newTimeStamp :: (GetAlloc eff ~ Scope s)
               => ITime
               -> Ref s0 (Stored Idx)
               -> Ref s1 (Array BufLen (Stored ITime))
               -> Ref s2 (Stored IBool)
               -> Ivory eff C.CommsecStatus
  newTimeStamp t idxRef arr bufFullRef = do
    idx <- deref idxRef
    store (arr ! toIx idx) t
    res <- checkProp idx arr bufFullRef
    -- Update index
    let idx' = incrIdx idx arr
    store idxRef idx'
    -- Update buffer full Boolean
    bufFull <- deref bufFullRef
    store bufFullRef (bufFull .|| (idx' ==? 0))
    return (res ? (C.secure, C.alarm))

  incrIdx :: Idx -> Ref s CirBuf -> Idx
  incrIdx idx arr = (idx + 1) .% arrayLen arr

  -- False if it fails.
  checkProp :: (GetAlloc eff ~ Scope s)
            => Idx -> Ref s0 CirBuf -> Ref s1 (Stored IBool) -> Ivory eff IBool
  checkProp idx arr bufFullRef = do
    bufFull <- deref bufFullRef
    ifte (iNot bufFull) -- Haven't filled the buffer yet.
      (return true)
      (do tnow  <- deref (arr ! toIx idx)
          t0    <- deref (arr ! toIx (startIdx idx arr))
          return (tnow - t0 >? alarm_threshold))
    where
    -- Get the start index in the circular buffer.  Assumes buffer is full.
    startIdx = incrIdx

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

--------------------------------------------------------------------------------

recoveryTower :: DataSink (Struct "veh_commsec_msg") -- From Commsec/Decrypt task
              -> Tower p ()
recoveryTower commsec_info_snk = do

  -- XXX We might have multiple recovery tasks here doing multiple things.  For
  -- now, we just have one task, looking at commsec messages.
  task "commsecRecoveryTask" $ commsecRecoveryTask commsec_info_snk

type Time   = Uint32
type Idx    = Sint32
type BufLen = 30
type CirBuf = Array BufLen (Stored Time)

commsecRecoveryTask :: DataSink (Struct "veh_commsec_msg") -- From Commsec/Decrypt task
                    -> Task p ()
commsecRecoveryTask commsec_info_snk = do
  commsec_info_snk_rx <- withDataReader commsec_info_snk "commsec_info_snk"
  -- For our property, we'll store timestamps in an array.
  cirBuf   <- taskLocalInit "cirBuf"  (iarray [] :: Init CirBuf)
  -- Points to the last written-to cell.
  idx      <- taskLocalInit "idx"     (ival 0 :: Init (Stored Idx))
  -- Have we filled the buffer once (at which point, "mod" semantics are used).
  bufFull  <- taskLocalInit "bufFull" (ival false)
  -- Number bad seen on last dataport read.
  numBad   <- taskLocalInit "numBad" (ival 0 :: Init (Stored Uint32))

  onPeriod 20 $ \_now -> do
    commsecReader <- local izero
    readData commsec_info_snk_rx commsecReader
    commsecMonitor commsecReader cirBuf idx bufFull numBad

  taskModuleDef $ do
    depend V.vehCommsecModule

  where
  -- XXX let's make up an arbitrary monitor here.  If we're received more than
  -- 20 bad messages in less than 30 seconds.
  commsecMonitor rx cirBuf idx bufFullRef prevBadRef = do
    totalBadMsgs <- rx ~>* V.bad_msgs
    lastTime     <- rx ~>* V.time
    prevBad      <- deref prevBadRef
    let currBad  = totalBadMsgs - prevBad
    store prevBadRef totalBadMsgs
    -- Store new values and check properties
    newTimeStamps currBad lastTime idx cirBuf bufFullRef

  newTimeStamp :: (GetAlloc eff ~ Scope s)
               => Time
               -> Ref s0 (Stored Idx)
               -> Ref s1 (Array 30 (Stored Time))
               -> Ref s2 (Stored IBool)
               -> Ivory eff IBool
  newTimeStamp t idxRef arr bufFullRef = do
    idx <- deref idxRef
    let idx' = incrIdx idx arr
    bufFull <- deref bufFullRef
    let bf = (idx' ==? 0) .&& iNot bufFull
    store bufFullRef bf
    store idxRef idx'
    store (arr ! toIx idx') t
    checkProp idx' arr bf

  newTimeStamps numBadVals t idxRef arr bufFullRef =
    -- For each bad value, we'll store the current timestamp, if we missed
    -- capturing it.
    times (toIx numBadVals :: Ix BufLen) $ const $ newTimeStamp t idxRef arr bufFullRef

  -- Get the start index in the circular buffer.
  startIdx :: (GetAlloc eff ~ Scope s)
           => Idx -> Ref s0 CirBuf -> IBool -> Ivory eff Idx
  startIdx idx arr bufFull =
    ifte bufFull (return $ incrIdx idx arr) (return 0)

  incrIdx :: Idx -> Ref s CirBuf -> Idx
  incrIdx idx arr = (idx + 1) .% arrayLen arr

  checkProp :: (GetAlloc eff ~ Scope s)
            => Idx -> Ref s0 CirBuf -> IBool -> Ivory eff IBool
  checkProp idx arr bufFull =
    ifte (iNot bufFull .&& (idx ==? 0))
      --We've recorded no values.
      (return true)
      (do tnow <- deref (arr ! toIx idx)
          idx' <- startIdx idx arr bufFull
          t0   <- deref (arr ! toIx idx')
          return (tnow - t0 <=? 30000))

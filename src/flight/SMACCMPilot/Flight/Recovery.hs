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
type BufLen = 20
type CirBuf = Array BufLen (Stored Time)

commsecRecoveryTask :: DataSink (Struct "veh_commsec_msg") -- From Commsec/Decrypt task
                    -> Task p ()
commsecRecoveryTask commsec_info_snk = do
  commsec_info_snk_rx <- withDataReader commsec_info_snk "commsec_info_snk"
  -- -- For our property, we'll store timestamps in an array.
  -- cirBuf   <- taskLocalInit "cirBuf"  (iarray [] :: Init CirBuf)
  -- -- Points to the last written-to cell.
  -- idx      <- taskLocalInit "idx"     (ival 0 :: Init (Stored Idx))
  -- -- Have we filled the buffer once (at which point, "mod" semantics are used).
  -- bufFull  <- taskLocalInit "bufFull" (ival false)
  -- -- Number bad seen on last dataport read.
  -- numBad   <- taskLocalInit "numBad" (ival 0 :: Init (Stored Uint32))
  -- -- Property result
  -- result   <- taskLocalInit "result" (ival true)

  onPeriod 20 $ \_now -> do
    commsecReader <- local izero
    readData commsec_info_snk_rx commsecReader
    -- commsecMonitor commsecReader cirBuf idx bufFull numBad result

  taskModuleDef $ do
    depend V.vehCommsecModule

  -- where
  -- -- XXX let's make up an arbitrary monitor here.  If we're received more than
  -- -- 20 bad messages in less than 30 seconds.
  -- commsecMonitor rx cirBuf idx bufFullRef prevBadRef resRef = do
  --   totalBadMsgs <- rx ~>* V.bad_msgs
  --   lastTime     <- rx ~>* V.time
  --   prevBad      <- deref prevBadRef
  --   let currBad  = totalBadMsgs - prevBad
  --   store prevBadRef totalBadMsgs
  --   -- Store new values and check properties
  --   newTimeStamps currBad lastTime idx cirBuf bufFullRef resRef

  -- newTimeStamp :: (GetAlloc eff ~ Scope s)
  --              => Time
  --              -> Ref s0 (Stored Idx)
  --              -> Ref s1 (Array BufLen (Stored Time))
  --              -> Ref s2 (Stored IBool)
  --              -> Ivory eff IBool
  -- newTimeStamp t idxRef arr bufFullRef = do
  --   idx <- deref idxRef
  --   store (arr ! toIx idx) t
  --   res <- checkProp idx arr bufFullRef
  --   -- Update index
  --   let idx' = incrIdx idx arr
  --   store idxRef idx'
  --   -- Update buffer full Boolean
  --   bufFull <- deref bufFullRef
  --   let bf = bufFull .|| (idx' ==? 0)
  --   store bufFullRef bf
  --   return res

  -- newTimeStamps numBadVals t idxRef arr bufFullRef resRef =
  --   -- For each bad value, we'll store the current timestamp, if we missed
  --   -- capturing it.
  --   times (toIx numBadVals :: Ix BufLen)
  --     $ const
  --     $ do res <- deref resRef
  --          -- If res has already failed, do nothing.
  --          when res (newTimeStamp t idxRef arr bufFullRef >>= store resRef)

  -- incrIdx :: Idx -> Ref s CirBuf -> Idx
  -- incrIdx idx arr = (idx + 1) .% arrayLen arr

  -- -- False if it fails.
  -- checkProp :: (GetAlloc eff ~ Scope s)
  --           => Idx -> Ref s0 CirBuf -> Ref s1 (Stored IBool) -> Ivory eff IBool
  -- checkProp idx arr bufFullRef = do
  --   bufFull <- deref bufFullRef
  --   ifte (iNot bufFull) -- Haven't filled the buffer yet.
  --     (return true)
  --     (do tnow  <- deref (arr ! toIx idx)
  --         t0    <- deref (arr ! toIx (startIdx idx arr))
  --         return (tnow - t0 >? 30000))
  --   where
  --   -- Get the start index in the circular buffer.  Assumes buffer is full.
  --   startIdx = incrIdx

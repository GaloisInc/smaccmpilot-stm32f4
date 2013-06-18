
-- Put all includes, etc. in Tower () and out of tasks
-- Make use of queues easier by external tasks
-- unused vars in, e.g., taskbody_verify_updates_2 in tower.c

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Types
import CheckerTask
import Checker

import Ivory.Tower

import Ivory.Language
import qualified Ivory.Tower.Compile.FreeRTOS as F
import qualified Ivory.Compile.C.CmdlineFrontend as C


--------------------------------------------------------------------------------
-- Record Assigment
--------------------------------------------------------------------------------

legacyHdr :: String
legacyHdr = "legacy.h"

--------------------------------------------------------------------------------
-- Legacy tasks: wrappers for the tasks.
--------------------------------------------------------------------------------

-- Types
type Clk = Stored Sint32
type ClkEmitterType s = '[ConstRef s Clk] :-> ()

-- Externs
clkEmitter :: TaskSchedule -> ChannelEmitter Clk -> Def (ClkEmitterType s)
clkEmitter sch ch = proc "clkEmitter" $ \r -> body $ emit_ sch ch r

read_clock_block :: Def ('[ProcPtr (ClkEmitterType s)] :-> ())
read_clock_block = importProc "read_clock_block" legacyHdr

update_time_init :: Def ('[ProcPtr ('[AssignRef s] :-> ())] :-> ())
update_time_init = importProc "update_time_init" legacyHdr

update_time_block :: Def ('[Sint32] :-> ())
update_time_block = importProc "update_time_block" legacyHdr

-- Task wrapper: task reads a logical clock and passes the result to
-- updateTimeTask.
readClockTask :: ChannelSource Clk -> Task ()
readClockTask src = do
  clk <- withChannelEmitter src "clkSrc"
  taskModuleDef $ \sch -> (incl $ clkEmitter sch clk)
  p <- withPeriod 1000 -- once per sec
  taskBody $ \sch -> do
    eventLoop sch $ onTimer p $ \_now ->
      call_ read_clock_block $ procPtr $ clkEmitter sch clk

-- Task wrapper: task reads the channel and updates its local state witht the
-- time.
updateTimeTask :: ChannelSink Clk -> ChannelSource AssignStruct -> Task ()
updateTimeTask clk chk = do
  rx <- withChannelReceiver clk "timeRx"
  newVal <- withChannelEmitter chk "newVal"
  taskModuleDef $ \sch  -> incl $ recordEmit sch newVal
  taskModuleDef $ \_sch -> incl update_time_init
  taskBody $ \sch -> do
    call_ update_time_init $ procPtr $ recordEmit sch newVal
    eventLoop sch $ onChannel rx $ \time -> do
      t <- deref time
      call_ update_time_block t

--------------------------------------------------------------------------------

tasks :: Tower ()
tasks = do
  (chkSrc, chkSink) <- channel
  (clkSrc, clkSink) <- channel
  task "verify_updates" $ checkerTask chkSink
  task "readClockTask"  $ readClockTask clkSrc
  task "updateTimeTask" $ updateTimeTask clkSink chkSrc

--------------------------------------------------------------------------------

main :: IO ()
main = do
  let (_, objs) = F.compile tasks

  -- C.runCompiler objs C.initialOpts
  C.compileWith
    Nothing
    (Just [F.searchDir])
    (checksMod : objs)

  checker

  -- graphvizToFile "out.dot" asm

--------------------------------------------------------------------------------

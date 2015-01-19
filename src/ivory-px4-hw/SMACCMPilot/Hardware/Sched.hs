{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module SMACCMPilot.Hardware.Sched (
  Task, task, schedule
) where

import Control.Monad (forM, forM_)
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower

data Task req res = Task
  { taskName :: String
  , taskReq :: ChanOutput req
  , taskRes :: ChanInput res
  }

-- | Make a task with the given name for later scheduling.
task :: (IvoryArea req, IvoryArea res)
     => String
     -> Tower e (Task req res, ChanInput req, ChanOutput res)
task taskName = do
  (req, taskReq) <- channel
  (taskRes, res) <- channel
  return (Task { .. }, req, res)

data TaskState req res = TaskState
  { taskBase :: Task req res
  , taskPending :: Ref Global (Stored IBool)
  , taskLastReq :: Ref Global req
  }

-- | Multiplex a request/response bus across any number of tasks that
-- need to share it. Tasks may submit requests at any time, but only one
-- task's request will be submitted to the bus at a time. When that
-- request's response arrives, it is forwarded to the appropriate task
-- and the next waiting task's request is sent.
--
-- If multiple tasks have outstanding requests simultaneously, then this
-- component will round-robin between them to prevent starvation of any
-- task. It proceeds in the order that tasks are presented in the first
-- argument, so when coming out of idle, earlier tasks are higher
-- priority.
schedule :: (IvoryArea req, IvoryZero req, IvoryArea res, IvoryZero res, IvoryArea ready, IvoryZero ready)
         => [Task req res]
         -> ChanOutput ready
         -> ChanInput req
         -> ChanOutput res
         -> Tower e ()
schedule tasks ready reqChan resChan = do
  (doResetChan, resetChan) <- channel

  monitor "scheduler" $ do
    -- The "running" state is used to suppress coroutine resets except
    -- when the coroutine is idle. By initializing it to true, we prevent
    -- the coroutine from forwarding any requests to the underlying bus
    -- until we receive the "ready" signal. However, tasks can each queue
    -- up a request beforehand if desired.
    running <- stateInit "running" $ ival true

    handler ready "ready" $ do
      doReset <- emitter doResetChan 1
      callback $ const $ emitV doReset true

    -- Queue up to 1 request per task, which can arrive in any order. Wake
    -- up the coroutine if it's currently idle.
    states <- forM tasks $ \ taskBase@Task { .. } -> do
      taskPending <- state $ taskName ++ "_pending"
      taskLastReq <- state $ taskName ++ "_last_req"

      handler taskReq taskName $ do
        doReset <- emitter doResetChan 1
        callback $ \ req -> do
          was_pending <- deref taskPending
          assert $ iNot was_pending
          refCopy taskLastReq req
          store taskPending true

          was_running <- deref running
          unless was_running $ do
            store running true
            emitV doReset true

      return TaskState { .. }

    -- Look for a task that's ready to send a request. Once we've picked
    -- one, forward the request and wait for that response. Once the
    -- response arrives, forward it to the same task and then check for
    -- more pending tasks. Skip the ones we already checked this round to
    -- avoid starvation. Halt the coroutine when we don't find any more
    -- ready tasks, until a reset wakes us up again.
    coroutineHandler resetChan resChan "round_robin" $ do
      sendReq <- emitter reqChan 1
      emitters <- forM states $ \ st -> do
        e <- emitter (taskRes $ taskBase st) 1
        return (st, e)
      return $ CoroutineBody $ \ yield -> do
        forever $ do
          progress <- local $ ival false
          forM_ emitters $ \ (TaskState { .. }, e) -> do
            is_pending <- deref taskPending
            when is_pending $ do
              comment $ taskName taskBase ++ " sending request"
              store progress true
              emit sendReq $ constRef taskLastReq
              res <- yield
              comment $ taskName taskBase ++ " result received"
              store taskPending false
              emit e $ constRef res
          comment "only keep looping as long as we sent at least one request in each iteration"
          madeProgress <- deref progress
          unless madeProgress breakOut
        comment "nothing to do, so record that we need a reset on next request, and halt"
        store running false

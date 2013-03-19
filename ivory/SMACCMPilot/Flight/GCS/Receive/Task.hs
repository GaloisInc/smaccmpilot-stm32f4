{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module SMACCMPilot.Flight.GCS.Receive.Task
  ( gcsReceiveTask
  ) where

import Prelude hiding (last)

import Ivory.Language
import Ivory.Tower
import qualified Ivory.OS.FreeRTOS as OS
import SMACCMPilot.Util.Periodic

import qualified SMACCMPilot.Flight.Types.GCSStreamTiming as S
import SMACCMPilot.Flight.GCS.Stream (defaultPeriods)

gcsReceiveTask :: MemArea (Struct "usart")
               -> Source (Struct "gcsstream_timing")
               -> String -> Task
gcsReceiveTask usart s_src uniquename =
  withSource "streamperiods" s_src $ \streamPeriodSource ->
  let tDef = proc ("gcsReceiveTaskDef" ++ uniquename) $ body $ do
        s_periods <- local defaultPeriods
        source streamPeriodSource (constRef s_periods)
        forever $ do
          call_ OS.delay 10000
      mDefs = do
        depend OS.taskModule
        incl tDef
  in task tDef mDefs


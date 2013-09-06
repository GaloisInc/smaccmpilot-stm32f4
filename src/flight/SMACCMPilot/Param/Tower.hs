{-# LANGUAGE DataKinds #-}
--
-- Param/Tower.hs --- Parameter storage using Tower.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module SMACCMPilot.Param.Tower where

import Data.Traversable (Traversable, traverse)

import Ivory.Language
import Ivory.Tower

import SMACCMPilot.Param.Base

-- | Shorthand type for a data source of parameter values.
type ParamSource = DataSource (Stored IFloat)

-- | Shorthand type for a data sink of parameter values.
type ParamSink = DataSink   (Stored IFloat)

-- | Wrapper for a source/sink dataport pair.
data PortPair = PortPair
  { portPairSource :: ParamSource
  , portPairSink   :: ParamSink
  }

-- | Initialize a parameter with a Tower data port pair.
initTowerParams :: ParamT PortPair (Tower p) (a PortPair)
                -> Tower p (a PortPair, [Param PortPair])
initTowerParams x = paramInit go x
  where
    go :: String -> Float -> Tower p PortPair
    go _ v = do
      (psrc, psink) <- dataportInit (ival (ifloat v))
      return (PortPair psrc psink)

-- | Shorthand type for a reader for parameter values.
type ParamReader = DataReader (Stored IFloat)

-- | Shorthand type for a writer for parameter values.
type ParamWriter = DataWriter (Stored IFloat)

paramReader :: (DataPortable i, Traversable a)
            => a ParamSink -> Node i p (a ParamReader)
paramReader = traverse (\x -> withDataReader x "paramReader")

paramWriter :: (DataPortable i, Traversable a)
            => a ParamSource -> Node i p (a ParamWriter)
paramWriter = traverse (\x -> withDataWriter x "paramWriter")

paramRead :: (Traversable a) => a ParamReader -> Ivory (ProcEffects s ()) (a IFloat)
paramRead =
  traverse $ \d -> do
    x <- local (ival 0.0)
    readData d x
    deref x

{-
----------------------------------------------------------------------
-- Example

data PIDParams f = PIDParams
  { pidP :: Param f
  , pidI :: Param f
  , pidD :: Param f
  } deriving (Functor, Foldable, Traversable)

deriving instance Show f => Show (PIDParams f)

-- | Initialize PID parameters with default values.
pidParams :: Monad m => Float -> Float -> Float -> ParamT f m (PIDParams f)
pidParams p i d =
  PIDParams <$> param "P" p
            <*> param "I" i
            <*> param "D" d

data FlightParams f = FlightParams
  { flightStabRoll  :: PIDParams f
  , flightStabPitch :: PIDParams f
  , flightStabYaw   :: PIDParams f
  } deriving (Functor, Foldable, Traversable)

deriving instance Show f => Show (FlightParams f)

-- | Initialize flight parameters.
flightParams :: Monad m => ParamT f m (FlightParams f)
flightParams =
  FlightParams <$> group "STAB_RLL" (pidParams 4.0 0.1 0.0)
               <*> group "STAB_PIT" (pidParams 4.0 0.1 0.0)
               <*> group "STAB_YAW" (pidParams 1.0 0.1 0.0)

testTower :: Tower p ()
testTower = do
  (ports, paramList) <- initTowerParams flightParams
  let pSrc  = portPairSource <$> ports
  let pSink = portPairSink   <$> ports
  task "testReader" (testReader pSink)
  task "testWriter" (testWriter pSrc)

testReader :: FlightParams ParamSink -> Task p ()
testReader pSink = do
  pReader <- paramReader pSink

  onPeriod 1000 $ \_ -> do
    pid <- paramRead (flightStabRoll pReader)
    writes "P = "
    write  (paramData (pidP pid))
    writes " I = "
    write  (paramData (pidI pid))
    writes " D = "
    write  (paramData (pidD pid))
    writes "\r\n"

testWriter :: FlightParams ParamSource -> Task p ()
testWriter pSrc = do
  pWriter <- paramWriter pSrc
  x       <- taskLocal "x"

  onPeriod 1000 $ \now -> do
    store x (safeCast now)
    writeData (paramData . pidP . flightStabRoll $ pWriter) (constRef x)

test :: FlightParams ()
test = fst $ runId $ paramInit (\_ _ -> return ()) flightParams

main :: IO ()
main = compile defaultBuildConf testTower

-}

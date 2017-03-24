{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}

module SMACCMPilot.Flight.Law.Arming where

import           Ivory.Language
import           Ivory.Tower
import           Ivory.Stdlib
import qualified SMACCMPilot.Comm.Ivory.Types.ArmingMode      as A
import qualified SMACCMPilot.Comm.Ivory.Types.ArmingStatus    as A
import qualified SMACCMPilot.Comm.Ivory.Types.Tristate        as T

data ArmingInput a =
  ArmingInput
    { ai_name :: String
    , ai_chan :: ChanOutput a
    , ai_get  :: forall eff s . ConstRef s a -> Ivory eff T.Tristate
    , ai_set  :: Label "arming_status" ('Stored T.Tristate)
    }

-- | Constant arming inputs must always be non-negative in order to
-- both arm and stay armed. Initial arming inputs must be non-negative
-- in order to transition from safe to armed, but do not cause a
-- transition from armed to safe if they return to negative. This is
-- meant for arming inputs that we want to enforce for initial arming,
-- but that shouldn't kill the vehicle in flight, like a lossy sensor.
data ConstantArmingInput =
    forall a . (IvoryArea a, IvoryZero a) => ConstantArmingInput (ArmingInput a)

data InitialArmingInput =
    forall a . (IvoryArea a, IvoryZero a) => InitialArmingInput (ArmingInput a)

armingTower :: (IvoryArea a, IvoryZero a)
            => ArmingInput a -- CLOCK
            -> [InitialArmingInput]
            -> [ConstantArmingInput]
            -> ChanInput ('Stored A.ArmingMode)
            -> ChanInput ('Struct "arming_status")
            -> Tower e ()
armingTower ai_clk ai_inits ai_consts a_mode a_stat = monitor "arming_law" $ do
  a <- stateInit "arming_law" (ival A.safe)
  s <- stateInit "arming_inputs" $ istruct
        [ A.px4io      .= ival T.negative
        , A.rcinput    .= ival T.negative
        , A.sens_valid .= ival T.negative
        , A.telem      .= ival T.neutral
        ]

  let s_clk = s ~> (ai_set ai_clk)
  s_inits <- mapM (initAIState s) ai_inits
  s_consts <- mapM (constAIState s) ai_consts
  handler (ai_chan ai_clk) ("arming_input_clock_" ++ ai_name ai_clk) $ do
    em <- emitter a_mode 1
    es <- emitter a_stat 1
    callback $ \v -> do
      t <- ai_get ai_clk v
      store s_clk t
      calcArmingState a (map constRef s_inits) (map constRef (s_clk:s_consts))
      emit em (constRef a)
      emit es (constRef s)


calcArmingState :: Ref s ('Stored A.ArmingMode)
                -> [ConstRef 'Global ('Stored T.Tristate)]
                -> [ConstRef 'Global ('Stored T.Tristate)]
                -> Ivory eff ()
calcArmingState a initTs constTs = do
  mode <- deref a
  cond_ [ (mode ==? A.safe) ==> do
            ts <- mapM deref (constTs ++ initTs)
            let ts_low  = foldl (.||) false (map (==? T.negative) ts)
                ts_high = foldl (.||) false (map (==? T.positive) ts)
            cond_
              [ ts_low  ==> store a A.safe
              , ts_high ==> store a A.armed
              ]
          -- if we are already armed, then only consider the constant inputs
        , (mode ==? A.armed) ==> do
            ts <- mapM deref constTs
            let ts_low  = foldl (.||) false (map (==? T.negative) ts)
                ts_high = foldl (.||) false (map (==? T.positive) ts)
            cond_
              [ ts_low  ==> store a A.safe
              , ts_high ==> store a A.armed
              ]
        ]

constAIState :: Ref 'Global ('Struct "arming_status")
             -> ConstantArmingInput
             -> Monitor e (Ref 'Global ('Stored T.Tristate))
constAIState status (ConstantArmingInput ai) = do
  let set = status ~> (ai_set ai)
  handler (ai_chan ai) ("arming_input_new_" ++ ai_name ai) $ do
    callback $ \v -> do
      t <- ai_get ai v
      store set t
  return set

initAIState :: Ref 'Global ('Struct "arming_status")
            -> InitialArmingInput
            -> Monitor e (Ref 'Global ('Stored T.Tristate))
initAIState status (InitialArmingInput ai) = do
  let set = status ~> (ai_set ai)
  handler (ai_chan ai) ("arming_input_new_" ++ ai_name ai) $ do
    callback $ \v -> do
      t <- ai_get ai v
      store set t
  return set

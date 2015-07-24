{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module SMACCMPilot.Flight.Law.Arming where

import           Ivory.Language
import           Ivory.Tower
import           Ivory.Stdlib
import qualified SMACCMPilot.Comm.Ivory.Types.ArmingMode      as A
import qualified SMACCMPilot.Comm.Ivory.Types.Tristate        as T


data ArmingInput a =
  ArmingInput
    { ai_name :: String
    , ai_chan :: ChanOutput a
    , ai_get  :: forall eff s . ConstRef s a -> Ivory eff T.Tristate
    }

data SomeArmingInput = forall a . (IvoryArea a, IvoryZero a)
                    => SomeArmingInput (ArmingInput a)


armingTower :: SomeArmingInput -- CLOCK
            -> [SomeArmingInput]
            -> ChanInput (Stored A.ArmingMode)
            -> Tower e ()
armingTower (SomeArmingInput ai_clk) ai_rest a_mode = monitor "arming_law" $ do
  a <- stateInit "arming_law" (ival A.safe)
  s_clk <- state ("arming_input_clock_state_" ++ ai_name ai_clk)
  s_rest <- mapM someAIState ai_rest
  handler (ai_chan ai_clk) ("arming_input_clock_" ++ ai_name ai_clk) $ do
    e <- emitter a_mode 1
    callback $ \v -> do
      t <- ai_get ai_clk v
      store s_clk t
      calcArmingState a (map constRef (s_clk:s_rest))
      emit e (constRef a)


  where
  calcArmingState :: Ref s (Stored A.ArmingMode)
                  -> [ConstRef Global (Stored T.Tristate)]
                  -> Ivory eff ()
  calcArmingState a iis = do
    ts <- mapM deref iis
    let ts_low  = foldl (.||) false (map (==? T.negative) ts)
        ts_high = foldl (.||) false (map (==? T.positive) ts)
    cond_
      [ ts_low  ==> store a A.safe
      , ts_high ==> store a A.armed
      ]

  someAIState :: SomeArmingInput -> Monitor e (Ref Global (Stored T.Tristate))
  someAIState (SomeArmingInput ai) = do
    s <- stateInit ("arming_input_state_" ++ ai_name ai) (ival T.neutral)
    handler (ai_chan ai) ("arming_input_new_" ++ ai_name ai) $ do
      callback $ \v -> do
        t <- ai_get ai v
        store s t
    return s


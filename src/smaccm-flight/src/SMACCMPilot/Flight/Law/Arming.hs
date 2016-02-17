{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

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

data SomeArmingInput = forall a . (IvoryArea a, IvoryZero a)
                    => SomeArmingInput (ArmingInput a)


armingTower :: SomeArmingInput -- CLOCK
            -> [SomeArmingInput]
            -> ChanInput ('Stored A.ArmingMode)
            -> ChanInput ('Struct "arming_status")
            -> Tower e ()
armingTower (SomeArmingInput ai_clk) ai_rest a_mode a_stat = monitor "arming_law" $ do
  a <- stateInit "arming_law" (ival A.safe)
  s <- stateInit "arming_inputs" $ istruct
        [ A.accel_cal .= ival T.negative
        , A.mag_cal   .= ival T.negative
        , A.gyro_cal  .= ival T.negative
        , A.px4io     .= ival T.negative
        , A.rcinput   .= ival T.negative
        , A.sens_cal  .= ival T.negative
        , A.telem     .= ival T.neutral
        ]

  let s_clk = s ~> (ai_set ai_clk)
  s_rest <- mapM (someAIState s) ai_rest
  handler (ai_chan ai_clk) ("arming_input_clock_" ++ ai_name ai_clk) $ do
    em <- emitter a_mode 1
    es <- emitter a_stat 1
    callback $ \v -> do
      t <- ai_get ai_clk v
      store s_clk t
      calcArmingState a (map constRef (s_clk:s_rest))
      emit em (constRef a)
      emit es (constRef s)


  where
  calcArmingState :: Ref s ('Stored A.ArmingMode)
                  -> [ConstRef 'Global ('Stored T.Tristate)]
                  -> Ivory eff ()
  calcArmingState a iis = do
    ts <- mapM deref iis
    let ts_low  = foldl (.||) false (map (==? T.negative) ts)
        ts_high = foldl (.||) false (map (==? T.positive) ts)
    cond_
      [ ts_low  ==> store a A.safe
      , ts_high ==> store a A.armed
      ]

  someAIState :: Ref 'Global ('Struct "arming_status")
              -> SomeArmingInput
              -> Monitor e (Ref 'Global ('Stored T.Tristate))
  someAIState status (SomeArmingInput ai) = do
    let s = status ~> (ai_set ai)
    handler (ai_chan ai) ("arming_input_new_" ++ ai_name ai) $ do
      callback $ \v -> do
        t <- ai_get ai v
        store s t
    return s


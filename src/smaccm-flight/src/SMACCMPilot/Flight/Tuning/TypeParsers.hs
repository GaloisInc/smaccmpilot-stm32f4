{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Flight.Tuning.TypeParsers
  ( pidConfigParser
  , stabConfigParser
  , throttleUIParser
  , userInputTrimParser
  ) where

import Ivory.Language
import Ivory.Tower.Config
import GHC.Float (double2Float)

import qualified SMACCMPilot.Comm.Ivory.Types.PidConfig as PID
import qualified SMACCMPilot.Comm.Ivory.Types.StabConfig as Stab
import qualified SMACCMPilot.Comm.Ivory.Types.ThrottleUi as T
import qualified SMACCMPilot.Comm.Ivory.Types.UserInputTrim as UI

ifloatParser :: String -> ConfigParser IFloat
ifloatParser n = do
  v <- subsection n double
  return (ifloat (double2Float v))

pidConfigParser :: ConfigParser (Init (Struct "pid_config"))
pidConfigParser = do
  p_gain <- ifloatParser "p_gain"
  i_gain <- ifloatParser "i_gain" `withDefault` 0
  d_gain <- ifloatParser "d_gain" `withDefault` 0
  i_max  <- ifloatParser "i_max"  `withDefault` 0
  i_min  <- ifloatParser "i_min"  `withDefault` (-1 * i_max)
  return $ istruct
    [ PID.p_gain .= ival p_gain
    , PID.i_gain .= ival i_gain
    , PID.d_gain .= ival d_gain
    , PID.i_min  .= ival i_min
    , PID.i_max  .= ival i_max
    ]


stabConfigParser :: ConfigParser (Init (Struct "stab_config"))
stabConfigParser = do
  pos  <- subsection "pos"  pidConfigParser
  rate <- subsection "rate" pidConfigParser
  return $ istruct
    [ Stab.pos  .= pos
    , Stab.rate .= rate
    ]

throttleUIParser :: ConfigParser (Init (Struct "throttle_ui"))
throttleUIParser = do
  sens <- ifloatParser "sens"
  dead <- ifloatParser "dead"
  return $ istruct
    [ T.sens .= ival sens
    , T.dead .= ival dead
    ]

userInputTrimParser :: ConfigParser (Init (Struct "user_input_trim"))
userInputTrimParser = do
  t <- ifloatParser "throttle_minimum" `withDefault` 0
  r <- ifloatParser "roll"  `withDefault` 0
  p <- ifloatParser "pitch" `withDefault` 0
  y <- ifloatParser "yaw"   `withDefault` 0
  return $ istruct
    [ UI.throttle_minimum .= ival t
    , UI.roll             .= ival r
    , UI.pitch            .= ival p
    , UI.yaw              .= ival y
    ]

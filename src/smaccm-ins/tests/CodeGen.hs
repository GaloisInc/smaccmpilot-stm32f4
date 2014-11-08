{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}

import Control.Applicative
import Data.Foldable
import Data.Traversable
import qualified Ivory.Compile.C.CmdlineFrontend as C (compile)
import Ivory.Language
import Ivory.Stdlib
import IvoryCSE
import IvoryFilter.Types
import Prelude hiding (mapM, sequence_, sum)
import SMACCM.INS.Matrix (Pointwise, diagMat)
import SMACCM.INS.Pressure
import SMACCM.INS.Quat
import SMACCM.INS.SensorFusionModel
import SMACCM.INS.Vec3

instance HasAtan2 IDouble where
  arctan2 = atan2F

kalman_state :: MemArea (Struct "kalman_state")
kalman_state = area "kalman_state" Nothing

kalman_covariance :: MemArea (Struct "kalman_covariance")
kalman_covariance = area "kalman_covariance" Nothing

vec3FromArray :: IvoryArea v => Vec3 ((Ref s t -> Ref s (Array 3 v)) -> Ref s t -> Ref s v)
vec3FromArray = Vec3 ((! 0) .) ((! 1) .) ((! 2) .)

stateVectorFromStruct :: Ref s (Struct "kalman_state") -> StateVector (Ref s (Stored IDouble))
stateVectorFromStruct s = StateVector
  { stateOrient = Quat ((! 0) .) (Vec3 ((! 1) .) ((! 2) .) ((! 3) .)) <*> pure (~> orient)
  , stateVel = NED vec3FromArray <*> pure (~> vel)
  , statePos = NED vec3FromArray <*> pure (~> pos)
  , stateGyroBias = XYZ vec3FromArray <*> pure (~> gyro_bias)
  , stateWind = NED vec3FromArray <*> pure (~> wind)
  , stateMagNED = NED vec3FromArray <*> pure (~> mag_ned)
  , stateMagXYZ = XYZ vec3FromArray <*> pure (~> mag_xyz)
  } <*> pure s

stateVector :: StateVector (Ref Global (Stored IDouble))
stateVector = stateVectorFromStruct (addrOf kalman_state)

p :: StateVector (StateVector (Ref Global (Stored IDouble)))
p = stateVectorFromStruct <$> (StateVector
  { stateOrient = Quat ((! 0) .) (Vec3 ((! 1) .) ((! 2) .) ((! 3) .)) <*> pure (~> cov_orient)
  , stateVel = NED vec3FromArray <*> pure (~> cov_vel)
  , statePos = NED vec3FromArray <*> pure (~> cov_pos)
  , stateGyroBias = XYZ vec3FromArray <*> pure (~> cov_gyro_bias)
  , stateWind = NED vec3FromArray <*> pure (~> cov_wind)
  , stateMagNED = NED vec3FromArray <*> pure (~> cov_mag_ned)
  , stateMagXYZ = XYZ vec3FromArray <*> pure (~> cov_mag_xyz)
  } <*> pure (addrOf kalman_covariance))

storeRow :: (Foldable f, Pointwise f, IvoryStore a, IvoryExpr a) => f (Ref s (Stored a)) -> f a -> Ivory eff ()
storeRow vars vals = sequence_ $ liftA2 store vars vals

fixQuat :: Floating a => StateVector a -> StateVector a
fixQuat state = (pure id) { stateOrient = pure (/ vecMag (stateOrient state)) } <*> state

kalman_init :: Def ('[IDouble, IDouble, IDouble, IDouble, IDouble, IDouble, IDouble] :-> ())
kalman_init = cse $ proc "kalman_init" $ \ accX accY accZ magX magY magZ pressure -> body $ do
    let depth = negate $ pressureToHeight pressure
    let acc = XYZ $ Vec3 accX accY accZ
    let mag = XYZ $ Vec3 magX magY magZ
    let initialState = initDynamic acc mag (pure 0) 0 (pure 0) (ned 0 0 depth)
    storeRow stateVector initialState
    sequence_ $ liftA2 storeRow p kalmanP

kalman_predict :: Def ('[IDouble, IDouble, IDouble, IDouble, IDouble, IDouble, IDouble] :-> ())
kalman_predict = cse $ proc "kalman_predict" $ \ dt dax day daz dvx dvy dvz -> body $ do
  stateVectorTemp <- mapM deref stateVector
  pTemp <- mapM (mapM deref) p
  let distVector = DisturbanceVector { disturbanceGyro = xyz dax day daz, disturbanceAccel = xyz dvx dvy dvz }
  let speed = vecMag $ stateVel stateVectorTemp
  onGround <- assign $ speed <? 4
  let whenFlying v = onGround ? (0, v)
  let noise = (pure id) { stateWind = pure whenFlying, stateMagNED = pure whenFlying, stateMagXYZ = pure whenFlying } <*> processNoise dt
  let (stateVector', p') = updateProcess dt stateVectorTemp distVector pTemp $ diagMat noise
  storeRow stateVector $ fixQuat stateVector'
  sequence_ $ liftA2 storeRow p p'

applyUpdate :: IDouble -> (StateVector IDouble -> StateVector (StateVector IDouble) -> (IDouble, IDouble, StateVector IDouble, StateVector (StateVector IDouble))) -> Ivory eff ()
applyUpdate cov fusionStep = do
    stateVectorTemp <- mapM deref stateVector
    pTemp <- mapM (mapM deref) p
    let (innov, innovCov, stateVectorPreCorrected, p') = fusionStep stateVectorTemp pTemp
    let stateVector' = fixQuat stateVectorPreCorrected
    -- TODO: when innovCov < cov, add cov to the "right" elements of p
    when (innovCov >=? cov) $ do
      when (innov ^ (2 :: Int) / innovCov <? 5 ^ (2 :: Int)) $ do
        let save :: (Foldable f, Pointwise f) => (forall a. StateVector a -> f a) -> Ivory eff ()
            save sel = do
              storeRow (sel stateVector) (sel stateVector')
              sequence_ $ liftA2 storeRow (sel p) (sel p')
        save stateOrient
        save stateVel
        save statePos
        save stateGyroBias
        let speed = vecMag $ stateVel stateVectorTemp
        when (speed <? 4) $ do
          save stateWind
          save stateMagNED
          save stateMagXYZ

vel_measure :: Def ('[IDouble, IDouble, IDouble] :-> ())
vel_measure = cse $ proc "vel_measure" $ \ velN velE velD -> body $ sequence_ $ applyUpdate <$> velNoise <*> (fuseVel <*> velNoise <*> ned velN velE velD)

pos_measure :: Def ('[IDouble, IDouble, IDouble] :-> ())
pos_measure = cse $ proc "pos_measure" $ \ posN posE posD -> body $ sequence_ $ applyUpdate <$> posNoise <*> (fusePos <*> posNoise <*> ned posN posE posD)

tas_measure :: Def ('[IDouble] :-> ())
tas_measure = cse $ proc "tas_measure" $ \ tas -> body $ applyUpdate tasNoise $ fuseTAS tasNoise tas

mag_measure :: Def ('[IDouble, IDouble, IDouble] :-> ())
mag_measure = cse $ proc "mag_measure" $ \ magX magY magZ -> body $ sequence_ $ applyUpdate <$> magNoise <*> (fuseMag <*> magNoise <*> xyz magX magY magZ)

ins_module :: Module
ins_module = package "smaccm_ins" $ do
  defStruct (Proxy :: Proxy "kalman_state")
  defStruct (Proxy :: Proxy "kalman_covariance")
  defMemArea kalman_state
  defMemArea kalman_covariance
  incl kalman_init
  incl kalman_predict
  incl vel_measure
  incl pos_measure
  incl tas_measure
  incl mag_measure

main :: IO ()
main = C.compile [ ins_module ]

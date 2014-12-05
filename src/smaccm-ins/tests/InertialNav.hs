-- Test driver that consumes the same test inputs that Paul
-- Riseborough's InertialNav project uses.

import Control.Applicative
import Control.Arrow
import Control.Lens ((^.))
import Control.Monad
import Data.Foldable (toList)
import Data.Monoid
import Linear
import MonadLib
import Numeric.Estimator
import Numeric.Estimator.Model.Coordinate
import Numeric.Estimator.Model.SensorFusion
import Simulate

deg2rad :: Double -> Double
deg2rad deg = deg * pi / 180

-- Model noise parameters

processNoise :: Fractional a => a -> StateVector a
processNoise dt = fmap (^ (2 :: Int)) $ fmap (dt *) $ StateVector
    { stateOrient = pure 1.0e-9
    , stateVel = pure 1.0e-9
    , statePos = pure 1.0e-9
    , stateGyroBias = pure 5.0e-7
    , stateWind = pure 0.1
    , stateMagNED = pure 3.0e-4
    , stateMagXYZ = pure 3.0e-4
    }

distCovariance :: Fractional a => a -> DisturbanceVector a
distCovariance dt = fmap (^ (2 :: Int)) $ fmap (dt *) $ DisturbanceVector
    { disturbanceGyro = pure 1.4544411e-2
    , disturbanceAccel = pure 0.5
    }

velNoise :: Fractional a => NED a
velNoise = ned 0.04 0.04 0.08

posNoise :: Fractional a => NED a
posNoise = pure 4

tasNoise :: Fractional a => a
tasNoise = 2

magNoise :: Fractional a => XYZ a
magNoise = pure 1.4826

data IMU = IMU
  { imuTime :: Double
  , imuAngRate :: XYZ Double
  , imuAccel :: XYZ Double
  }
  deriving Show

readIMU :: [Double] -> Msg
readIMU [msec, angRateX, angRateY, angRateZ, accelX, accelY, accelZ] = IMUMsg $ IMU (msec / 1000) (xyz angRateX angRateY angRateZ) (xyz accelX accelY accelZ)
readIMU l = error $ "bad IMU line: " ++ show l

data MAG = MAG
  { magData :: XYZ Double
  , magBias :: XYZ Double
  }
  deriving Show

readMAG :: [Double] -> Msg
readMAG [_, mx, my, mz, bx, by, bz] = MAGMsg $ MAG (m + b) b
  where
  m = fmap (* 0.001) $ xyz mx my mz
  b = fmap (* negate 0.001) $ xyz bx by bz
readMAG l = error $ "bad MAG line: " ++ show l

data GPS = GPS
  { gpsStatus :: Int
  , gpsCourse :: Double
  , gpsGndSpd :: Double
  , gpsVelD :: Double
  , gpsLat :: Double
  , gpsLon :: Double
  , gpsHgt :: Double
  }
  deriving Show

readGPS :: [Double] -> Msg
readGPS [status, _, _, _, _, lat, lon, hgt, _, gndSpd, course, velD, _] = GPSMsg $ GPS (round status) (deg2rad course) gndSpd velD (deg2rad lat) (deg2rad lon - pi) hgt
readGPS l = error $ "bad GPS line: " ++ show l

calcVelNED :: GPS -> NED Double
calcVelNED gps = ned (gpsGndSpd gps * cos (gpsCourse gps)) (gpsGndSpd gps * sin (gpsCourse gps)) (gpsVelD gps)

data ADS = ADS
  { adsVtas :: Double
  , adsBaroHgt :: Double
  }
  deriving Show

readADS :: Double -> [Double] -> Msg
readADS eas2tas [_, _, _, _, _, _, eas, baroHgt, _] = ADSMsg $ ADS (eas2tas * eas) baroHgt
readADS _ l = error $ "bad ADS line: " ++ show l

readNumericTable :: String -> IO [[Double]]
readNumericTable filename = do
  contents <- readFile filename
  return $ map (map read . words) $ lines contents

data Msg
  = IMUMsg IMU
  | MAGMsg MAG
  | GPSMsg GPS
  | ADSMsg ADS
  deriving Show

toMsg :: Double -> ([Double] -> Msg) -> String -> IO [(Double, Msg)]
toMsg delay f = fmap (map $ \ (timestamp : rest) -> (timestamp - delay, f rest)) . readNumericTable

merge :: Ord k => [(k, v)] -> [(k, v)] -> [(k, v)]
merge [] r = r
merge l [] = l
merge ls@((lt, l) : ls') rs@((rt, r) : rs')
  | lt <= rt = (lt, l) : merge ls' rs
  | otherwise = (rt, r) : merge ls rs'

latchMsgs :: [Msg] -> (Maybe MAG, Maybe GPS, Maybe ADS)
latchMsgs msgs = let (Last mag, Last gps, Last ads) = mconcat $ map latchMsg msgs in (mag, gps, ads)
  where
  latchMsg (MAGMsg mag) = (Last $ Just mag, mempty, mempty)
  latchMsg (GPSMsg gps) = (mempty, Last $ Just gps, mempty)
  latchMsg (ADSMsg ads) = (mempty, mempty, Last $ Just ads)
  latchMsg _ = mempty

notReached :: Double -> (Double, Msg) -> Bool
notReached time (_, IMUMsg imu) = imuTime imu <= time
notReached _ _ = True

main :: IO ()
main = do
  [alignTime, startTime, endTime, _velDelay, posDelay, _hgtDelay, magDelay, tasDelay, eas2tas] <- fmap (map read . words) $ readFile "timing.txt"
  msgs <- fmap (foldr merge []) $ sequence
    [ toMsg 0 readIMU "IMU.txt"
    , toMsg magDelay readMAG "MAG.txt"
    , filter (\ (_, GPSMsg gps) -> gpsStatus gps > 2) <$> toMsg posDelay readGPS "GPS.txt"
    , toMsg tasDelay (readADS eas2tas) "NTUN.txt"
    ]

  let (align, (_, IMUMsg alignIMU) : running) = span (notReached alignTime) $ dropWhile (notReached startTime) msgs

  let (Just alignMag, Just alignGPS, Just alignADS) = latchMsgs $ map snd align

  let earthRadius = 6378145
  let calcPosNED gps = ned (earthRadius * (gpsLat gps - gpsLat alignGPS)) (earthRadius * cos (gpsLat alignGPS) * (gpsLon gps - gpsLon alignGPS)) (gpsHgt alignGPS - gpsHgt gps)

  let initialState = initDynamic (imuAccel alignIMU) (magData alignMag) (magBias alignMag) 0 (calcVelNED alignGPS) (pure 0)

  void $ runKalmanState (imuTime alignIMU) initialState $ do
    forM_ (takeWhile (notReached endTime) running) $ \ (_, msg) -> case msg of
      IMUMsg imu -> do
        (lasttime, KalmanFilter laststate _) <- get
        lift $ putStrLn $ unwords $ map show $ lasttime : toList laststate
        let dt = imuTime imu - lasttime
        runProcessModel dt (processNoise dt) (distCovariance dt) $ DisturbanceVector { disturbanceGyro = imuAngRate imu, disturbanceAccel = imuAccel imu }
        sets_ $ first $ const $ imuTime imu
      MAGMsg mag -> void $ runFuseMag magNoise $ magData mag
      GPSMsg gps -> do
        void $ runFuseVel velNoise $ calcVelNED gps
        void $ runFusePos posNoise $ calcPosNED gps
      ADSMsg ads -> do
        void $ runFuseTAS tasNoise $ adsVtas ads
        void $ runFuseHeight (nedToVec3 posNoise ^._z) $ adsBaroHgt alignADS - adsBaroHgt ads
    (lasttime, KalmanFilter laststate _) <- get
    lift $ putStrLn $ unwords $ map show $ lasttime : toList laststate

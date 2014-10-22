{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad (replicateM)
import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Foldable
import Data.Int
import Data.Monoid
import Data.Traversable
import Data.Word
import MonadLib (lift, get, sets_)
import Prelude hiding (sequence, sequence_)
import SMACCM.INS.SensorFusionModel
import SMACCM.INS.Simulate
import System.Environment

type PSASTimestamp = Double
data PSASMessage = PSASMessage
    { psasFourCC :: S.ByteString
    , psasTimestamp :: PSASTimestamp
    , psasData :: S.ByteString
    }

getMessage :: Get PSASMessage
getMessage = do
    fourcc <- getByteString 4
    time_hi <- getWord32be
    time_lo <- getWord16be
    let time_ns = (toInteger time_hi `shiftL` 16) .|. toInteger time_lo
    let time = fromIntegral time_ns * 1.0e-9
    datalen <- getWord16be
    body <- getByteString $ case fourcc of
        "MPL3" -> 6
        _ -> fromIntegral datalen
    return PSASMessage { psasFourCC = fourcc, psasTimestamp = time, psasData = body }

data ADISMessage = ADISMessage
    { adisGyro :: XYZ Double -- in radian/second
    , adisAcc :: XYZ Double -- in meters/second/second
    , adisMagn :: XYZ Double -- in milligauss
    }
    deriving Show

newtype MPL3Message = MPL3Message
    { mpl3Pressure :: Double -- in Pascals
    }
    deriving Show

data PSASMessageType = ADIS ADISMessage | MPL3 MPL3Message
    deriving Show

class (Integral from, Num to) => SignConvert from to | from -> to, to -> from where
    signed :: from -> to
    signed = fromIntegral

instance SignConvert Word8 Int8
instance SignConvert Word16 Int16
instance SignConvert Word32 Int32
instance SignConvert Word64 Int64

getMessageType :: PSASMessage -> Maybe PSASMessageType
getMessageType msg = do
    let scale by on = fmap (* by) $ fmap (fromIntegral . signed) on
    parser <- case psasFourCC msg of
        "ADIS" -> return $ do
            skip 2
            gyro <- sequence $ pure $ scale (0.05 * pi / 180) getWord16be
            acc <- sequence $ pure $ scale (0.00333 * 9.80665) getWord16be
            magn <- sequence $ pure $ scale (5.0e-8 * 10000000) getWord16be
            skip 4
            return $ ADIS $ ADISMessage { adisGyro = gyro, adisAcc = acc, adisMagn = magn }
        "MPL3" -> return $ do
            pressure <- scale (1.5625e-5 * 1000) getWord32be
            skip 2
            return $ MPL3 $ MPL3Message { mpl3Pressure = pressure }
        _ -> Nothing
    case pushEndOfInput $ runGetIncremental parser `pushChunk` psasData msg of
        Done left _ v | S.null left -> return v
        Fail _ _ err -> error $ "failed on " ++ show (psasFourCC msg) ++ ": " ++ err
        _ -> error $ "leftovers after " ++ show (psasFourCC msg)

pressureToHeight :: Floating a => a -> a
pressureToHeight pressure = (baseAltitude * lapseRate + baseTemperature / ((pressure / basePressure) ** (airGasConstant * lapseRate / (g_0 * airMass))) - baseTemperature) / lapseRate
    where
    basePressure = 101325 -- Pascals
    baseTemperature = 288.15 -- K
    lapseRate = -0.0065 -- K/m
    baseAltitude = 0 -- m
    airGasConstant = 8.31432 --  N-m/mol-K
    g_0 = 9.80665 -- m/s/s
    airMass = 0.0289644 -- kg/mol

initialMeasurements :: (Last PSASTimestamp, Last ADISMessage, Last MPL3Message) -> Get (PSASTimestamp, ADISMessage, MPL3Message)
initialMeasurements (Last (Just t), Last (Just a), Last (Just m)) = return (t, a, m)
initialMeasurements prev = do
    msg <- getMessage
    initialMeasurements $ prev `mappend` case getMessageType msg of
        Just (ADIS v) -> (Last (Just $ psasTimestamp msg), Last (Just v), mempty)
        Just (MPL3 v) -> (Last (Just $ psasTimestamp msg), mempty, Last (Just v))
        _ -> mempty

runPSAS :: KalmanState Get Double a -> Get (a, (PSASTimestamp, StateVector Double, StateVector (StateVector Double)))
runPSAS go = do
    (ts, adis, mpl3) <- initialMeasurements mempty
    let depth = negate $ pressureToHeight $ mpl3Pressure mpl3
    let initialState = initDynamic (adisAcc adis) (adisMagn adis) (pure 0) 0 (pure 0) (ned 0 0 depth)
    runKalmanState ts initialState go

psasFilter :: KalmanState Get Double (IO ())
psasFilter = do
    msg <- lift getMessage
    (lasttime, laststate, _) <- get
    case getMessageType msg of
        Just (ADIS v) -> do
            runProcessModel (psasTimestamp msg - lasttime) $ DisturbanceVector { disturbanceGyro = adisGyro v, disturbanceAccel = adisAcc v }
            magStatus <- runFuseMag $ adisMagn v
            sets_ $ \ (_, state, p) -> (psasTimestamp msg, state, p)
            return $ do
                putStrLn $ unwords $ "pre-mag" : map show (lasttime : toList laststate)
                putStrLn $ unwords $ "mag" : map show (toList (fmap fst magStatus) ++ toList (fmap snd magStatus))
        Just (MPL3 v) -> do
            (innov, innovVar) <- runFuseHeight $ negate $ pressureToHeight $ mpl3Pressure v
            return $ do
                putStrLn $ unwords $ "pre-mpl" : map show (lasttime : toList laststate)
                putStrLn $ unwords $ "mpl" : map show [innov, innovVar]
        _ -> return (return ())

main :: IO ()
main = do
    args <- getArgs
    logfile <- case args of
        [] -> L.getContents
        fname : _ -> L.readFile fname
    let (states, (finalTime, finalState, _)) = runGet (runPSAS $ replicateM (2048 * 3) psasFilter) logfile
    sequence_ states
    putStrLn $ unwords $ "final" : map show (finalTime : toList finalState)

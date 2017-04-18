{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}

module SMACCMPilot.Flight.PackedStatus (packedStatusTower) where

import           Ivory.Language
import           Ivory.Tower
import qualified SMACCMPilot.Comm.Ivory.Types.AltControlDebug     as U
import qualified SMACCMPilot.Comm.Ivory.Types.ArmingStatus        as U
import qualified SMACCMPilot.Comm.Ivory.Types.ControlLaw          as U
import qualified SMACCMPilot.Comm.Ivory.Types.PackedStatus        as P
import qualified SMACCMPilot.Comm.Ivory.Types.PositionSample      as U
import qualified SMACCMPilot.Comm.Ivory.Types.SensorsResult       as U
import           SMACCMPilot.Comm.Tower.Attr
import           SMACCMPilot.Comm.Tower.Interface.ControllableVehicle

data Copy sym = forall a. IvoryArea a => Copy (Label "packed_status" a) (Label sym a)

-- | When 'src' changes, copy the specified 'fields' to the given ref
-- and then emit its current contents to 'dst'.
save :: (AttrWritable dst, AttrReadable src, AttrNamed src, IvoryStruct sym)
     => Ref 'Global ('Struct "packed_status")
     -> dst ('Struct "packed_status")
     -> src ('Struct sym)
     -> [Copy sym]
     -> Monitor e ()
save packed dst src fields = attrHandler src $ do
  e <- attrEmitter dst
  callback $ \ val -> do
    mapM_ (\ (Copy dstl srcl) -> refCopy (packed ~> dstl) (val ~> srcl)) fields
    emit e $ constRef packed

packedStatusTower :: ControllableVehicleAttrs Attr -> Tower e ()
packedStatusTower attrs = monitor "pack_status" $ do
  packed <- state "last_packed_status"

  save packed (packedStatus attrs) (sensorsOutput attrs)
    [ Copy P.valid U.valid
    , Copy P.roll U.roll
    , Copy P.pitch U.pitch
    , Copy P.yaw U.yaw
    ]

  save packed (packedStatus attrs) (altControlDebug attrs)
    [ Copy P.alt_est U.alt_est ]

  save packed (packedStatus attrs) (gpsOutput attrs)
    [ Copy P.fix U.fix
    , Copy P.num_sv U.num_sv
    , Copy P.lat U.lat
    , Copy P.lon U.lon
    , Copy P.alt U.alt
    , Copy P.vground U.vground
    , Copy P.heading U.heading
    ]

  save packed (packedStatus attrs) (armingStatus attrs)
    [ Copy P.rcinput U.rcinput
    , Copy P.telem U.telem
    , Copy P.px4io U.px4io
    , Copy P.sens_valid U.sens_valid
    ]

  save packed (packedStatus attrs) (controlLaw attrs)
    [ Copy P.arming_mode U.arming_mode
    , Copy P.control_modes U.control_modes
    ]

  attrHandler (batteryVoltage attrs) $ do
    e <- attrEmitter (packedStatus attrs)
    callbackV $ \v -> do
      store (packed ~> P.battery_voltage) v
      emit e (constRef packed)

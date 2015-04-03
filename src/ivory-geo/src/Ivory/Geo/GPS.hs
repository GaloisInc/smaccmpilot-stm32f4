{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Ivory.Geo.GPS where

import Ivory.Language

-- |A (Latitude, Longitude) pair in radians.  THIS MUST BE IN RADIANS!
type Coordinate = (IDouble, IDouble)

-- |@distance c1 c2@ returns the distance between coordinates @c1@ and @c2@.
distance :: Coordinate -> Coordinate -> IDouble
distance (lat1,lon1) (lat2,lon2) =
    let deltaLat = lat2 - lat1
        deltaLon = lon2 - lon1
        a = sin (deltaLat / 2) ** 2 + cos lat1 * cos lat2 * (sin (deltaLon / 2)**2)
        c = 2 * atan2F (sqrt a) (sqrt (1 - a))
    in radiusOfEarth * c

-- |@heading a b@ gets the heading from a to b with north as 0 and east as
-- pi/2.
heading :: Coordinate -> Coordinate -> IDouble
heading (lat1,lon1) (lat2,lon2) =
    let diffLon = lon2 - lon1
    in atan2F (sin diffLon * cos lat2)
              (cos lat1 * sin lat2 - sin lat1 * cos lat2 * cos (diffLon))

-- |Radius of earth in meters.
radiusOfEarth :: IDouble
radiusOfEarth = idouble 6378700

degreesToRadians :: IDouble -> IDouble
degreesToRadians d = d*(pi/180)

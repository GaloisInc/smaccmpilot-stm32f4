
module SMACCMPilot.Time
  ( timeMicrosFromITime
  , iTimeFromTimeMicros
  ) where

import Ivory.Tower
import SMACCMPilot.Comm.Ivory.Types.TimeMicros

iTimeFromTimeMicros :: TimeMicros -> ITime
iTimeFromTimeMicros (TimeMicros m) = fromIMicroseconds m

timeMicrosFromITime :: ITime -> TimeMicros
timeMicrosFromITime = TimeMicros . toIMicroseconds

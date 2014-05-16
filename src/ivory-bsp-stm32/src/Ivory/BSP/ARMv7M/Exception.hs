
module Ivory.BSP.ARMv7M.Exception where

newtype IRQn = IRQn { unIRQn :: Integer }

data Exception
  = Reset
  | NonMaskable
  | HardFault
  | MemoryManagment
  | BusFault
  | UsageFault
  | SVCall
  | DebugMonitor
  | PendSV
  | SysTick
  deriving (Eq, Show, Enum)

exceptionIRQn :: Exception -> IRQn
exceptionIRQn Reset           = IRQn (-15)
exceptionIRQn NonMaskable     = IRQn (-14)
exceptionIRQn HardFault       = IRQn (-13)
exceptionIRQn MemoryManagment = IRQn (-12)
exceptionIRQn BusFault        = IRQn (-11)
exceptionIRQn UsageFault      = IRQn (-10)
exceptionIRQn SVCall          = IRQn (-5)
exceptionIRQn DebugMonitor    = IRQn (-4)
exceptionIRQn PendSV          = IRQn (-2)
exceptionIRQn SysTick         = IRQn (-1)

exceptionTable :: [Maybe Exception]
exceptionTable =
  [ Just Reset
  , Just NonMaskable
  , Just HardFault
  , Just MemoryManagment
  , Just BusFault
  , Just UsageFault
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Just SVCall
  , Just DebugMonitor
  , Nothing
  , Just PendSV
  , Just SysTick
  ]


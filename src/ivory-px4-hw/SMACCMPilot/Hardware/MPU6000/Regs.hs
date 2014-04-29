
module SMACCMPilot.Hardware.MPU6000.Regs where

data Reg
  = SelfTestGX
  | SelfTestGY
  | SelfTestGZ
  | SelfTestAccel
  | SampleRateDivider
  | Config
  | GyroConfig
  | AccelConfig
  | FifoEnable
  | I2cMasterControl
  | I2cSlave0Addr
  | I2cSlave0Reg
  | I2cSlave0Ctrl
  | I2cSlave1Addr
  | I2cSlave1Reg
  | I2cSlave1Ctrl
  | I2cSlave2Addr
  | I2cSlave2Reg
  | I2cSlave2Ctrl
  | I2cSlave3Addr
  | I2cSlave3Reg
  | I2cSlave3Ctrl
  | I2cSlave4Addr
  | I2cSlave4Reg
  | I2cSlave4DOut
  | I2cSlave4Ctrl
  | I2cSlave4DIn
  | I2cMasterStatus
  | IntPinConfig
  | IntEnable
  | IntStatus
  | AccelXoutH
  | AccelXoutL
  | AccelYoutH
  | AccelYoutL
  | AccelZoutH
  | AccelZoutL
  | TempOutH
  | TempOutL
  | GyroXoutH
  | GyroXoutL
  | GyroYoutH
  | GyroYoutL
  | GyroZoutH
  | GyroZoutL
  | ExtSensData0
  | ExtSensData1
  | ExtSensData2
  | ExtSensData3
  | ExtSensData4
  | ExtSensData5
  | ExtSensData6
  | ExtSensData7
  | ExtSensData8
  | ExtSensData9
  | ExtSensData10
  | ExtSensData11
  | ExtSensData12
  | ExtSensData13
  | ExtSensData14
  | ExtSensData15
  | ExtSensData16
  | ExtSensData17
  | ExtSensData18
  | ExtSensData19
  | ExtSensData20
  | ExtSensData21
  | ExtSensData22
  | ExtSensData23
  | I2cSlave0DOut
  | I2cSlave1DOut
  | I2cSlave2DOut
  | I2cSlave3DOut
  | I2cMasterDelayCtrl
  | SignalPathReset
  | UserControl
  | PowerManagment1
  | PowerManagment2
  | FifoCountH
  | FifoCountL
  | FifoReadWrite
  | WhoAmI
  deriving (Eq, Show)

regAddr :: Reg -> Integer
regAddr SelfTestGX = 0x0d
regAddr SelfTestGY = 0x0e
regAddr SelfTestGZ = 0x0f
regAddr SelfTestAccel = 0x10
regAddr SampleRateDivider = 0x19
regAddr Config = 0x1a
regAddr GyroConfig = 0x1b
regAddr AccelConfig = 0x1c
regAddr FifoEnable = 0x23
regAddr I2cMasterControl = 0x24
regAddr I2cSlave0Addr = 0x25
regAddr I2cSlave0Reg = 0x26
regAddr I2cSlave0Ctrl = 0x27
regAddr I2cSlave1Addr = 0x28
regAddr I2cSlave1Reg = 0x29
regAddr I2cSlave1Ctrl = 0x2a
regAddr I2cSlave2Addr = 0x2b
regAddr I2cSlave2Reg = 0x2c
regAddr I2cSlave2Ctrl = 0x2d
regAddr I2cSlave3Addr = 0x2e
regAddr I2cSlave3Reg = 0x2f
regAddr I2cSlave3Ctrl = 0x30
regAddr I2cSlave4Addr = 0x31
regAddr I2cSlave4Reg = 0x32
regAddr I2cSlave4DOut = 0x33
regAddr I2cSlave4Ctrl = 0x34
regAddr I2cSlave4DIn = 0x35
regAddr I2cMasterStatus = 0x36
regAddr IntPinConfig = 0x37
regAddr IntEnable = 0x38
regAddr IntStatus = 0x3a
regAddr AccelXoutH = 0x3b
regAddr AccelXoutL = 0x3c
regAddr AccelYoutH = 0x3d
regAddr AccelYoutL = 0x3e
regAddr AccelZoutH = 0x3f
regAddr AccelZoutL = 0x40
regAddr TempOutH = 0x41
regAddr TempOutL = 0x42
regAddr GyroXoutH = 0x42
regAddr GyroXoutL = 0x44
regAddr GyroYoutH = 0x45
regAddr GyroYoutL = 0x46
regAddr GyroZoutH = 0x47
regAddr GyroZoutL = 0x48
regAddr ExtSensData0 = 0x49
regAddr ExtSensData1 = 0x4a
regAddr ExtSensData2 = 0x4b
regAddr ExtSensData3 = 0x4c
regAddr ExtSensData4 = 0x4d
regAddr ExtSensData5 = 0x4e
regAddr ExtSensData6 = 0x4f
regAddr ExtSensData7 = 0x50
regAddr ExtSensData8 = 0x51
regAddr ExtSensData9 = 0x52
regAddr ExtSensData10 = 0x53
regAddr ExtSensData11 = 0x54
regAddr ExtSensData12 = 0x55
regAddr ExtSensData13 = 0x56
regAddr ExtSensData14 = 0x57
regAddr ExtSensData15 = 0x58
regAddr ExtSensData16 = 0x59
regAddr ExtSensData17 = 0x5a
regAddr ExtSensData18 = 0x5b
regAddr ExtSensData19 = 0x5c
regAddr ExtSensData20 = 0x5d
regAddr ExtSensData21 = 0x5e
regAddr ExtSensData22 = 0x5f
regAddr ExtSensData23 = 0x60
regAddr I2cSlave0DOut = 0x63
regAddr I2cSlave1DOut = 0x64
regAddr I2cSlave2DOut = 0x65
regAddr I2cSlave3DOut = 0x66
regAddr I2cMasterDelayCtrl = 0x67
regAddr SignalPathReset = 0x68
regAddr UserControl = 0x6a
regAddr PowerManagment1 = 0x6b
regAddr PowerManagment2 = 0x6c
regAddr FifoCountH = 0x72
regAddr FifoCountL = 0x73
regAddr FifoReadWrite = 0x74
regAddr WhoAmI = 0x75


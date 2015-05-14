{-# LANGUAGE DataKinds #-}

module SMACCMPilot.Hardware.RGBLED where


import SMACCMPilot.Comm.Ivory.Types.RgbLedSetting

import Ivory.BSP.STM32.Driver.I2C
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface

rgbLedManager :: BackpressureTransmit (Struct "i2c_transaction_request")
                                      (Struct "i2c_transaction_result")
              -> ChanOutput (Stored ITime)
              -> ChanOutput (Struct "rgb_led_setting")
              -> I2CDeviceAddr
              -> Tower e ()
rgbLedManager bp_i2c init_chan attr addr = do
  monitor "rgbled" $ do
    err <- stateInit "error" (ival (1 :: Uint8))
    expect_resp <- state "expect_response"

    handler init_chan "init" $ do
      o <- emitter req_chan 1
      callback $ const $ do
        store err 0
        store expect_resp true
        req <- local $ istruct
          [ tx_addr .= ival addr
          , tx_buf  .= iarray
            [ ival 0x84
            , ival 0x03
            ]
          , tx_len .= ival 2
          , rx_len .= ival 0
          ]
        emit o (constRef req)

    handler attr "set" $ do
      o <- emitter req_chan 1
      callback $ \v -> do
        e <- deref err
        resp <- deref expect_resp
        unless ((e >? 0) .|| resp)  $ do
          store expect_resp true
          r <- deref (v ~> red)
          g <- deref (v ~> green)
          b <- deref (v ~> blue)
          req <- local $ istruct
            [ tx_addr .= ival addr
            , tx_buf  .= iarray
                [ ival 0x81
                , ival (limit b)
                , ival 0x82
                , ival (limit g)
                , ival 0x83
                , ival (limit r)
                ]
            , tx_len  .= ival 6
            , rx_len  .= ival 0
            ]
          emit o (constRef req)

    handler res_chan "i2c_response" $ do
      callback $ \v -> do
        r <- deref expect_resp
        ifte_ r
          (do rc <- deref (v ~> resultcode)
              when (rc >? 0) (store err 3)
          )
          (store err 2)
        store expect_resp false
  where
  limit ledval = (ledval >? 0x0F) ? (0x0F, ledval)
  BackpressureTransmit req_chan res_chan = bp_i2c


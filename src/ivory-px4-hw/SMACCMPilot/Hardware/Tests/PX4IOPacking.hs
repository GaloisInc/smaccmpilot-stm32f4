{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module SMACCMPilot.Hardware.Tests.PX4IOPacking (app) where

import Ivory.Artifact
import Ivory.Language
import Ivory.Serialize
import Ivory.Stdlib

import SMACCMPilot.Hardware.PX4IO.Pack
import SMACCMPilot.Hardware.PX4IO.CRC
import SMACCMPilot.Hardware.PX4IO.Types.Buffer
import SMACCMPilot.Hardware.PX4IO.Types.Request
import SMACCMPilot.Hardware.PX4IO.Types.RequestCode

puts :: Def('[IString]':-> Sint32)
puts = importProc "puts" "stdio.h"

printf_uint8 :: Def('[IString, Uint8]':-> Sint32)
printf_uint8 = importProc "printf" "stdio.h"

printf_uint16 :: Def('[IString, Uint16]':-> Sint32)
printf_uint16 = importProc "printf" "stdio.h"

printf_sint32 :: Def('[IString, Sint32]':-> Sint32)
printf_sint32 = importProc "printf" "stdio.h"

printf_none :: Def('[IString]':-> Sint32)
printf_none = importProc "printf" "stdio.h"

print_request :: Def('[ConstRef s ('Struct "px4io_request")
                      ] ':-> ())
print_request = proc "print_request" $ \req -> body $ do
  p "px4io_request { code: "
  rcode <- deref (req ~> req_code)
  cond_
    [ rcode ==? request_read    ==> p "read"
    , rcode ==? request_write   ==> p "write"
    , rcode ==? request_ok      ==> p "ok"
    , rcode ==? request_corrupt ==> p "corrupt"
    , rcode ==? request_error   ==> p "error"
    ]
  p ", count: "
  deref (req ~> count) >>= p8
  p ", page: "
  deref (req ~> page) >>= p8
  p ", offs: "
  deref (req ~> offs) >>= p8
  p ", regs: [ "
  arrayMap $ \ix -> do
    deref ((req ~> regs) ! ix) >>= p16
    ifte_ (ix ==? (arrayLen (req ~> regs) - 1))
          (p " ")
          (p ", ")
  p "] }\n"
  where
  p = call_ printf_none
  p8 = call_ printf_uint8 "0x%02x"
  p16 = call_ printf_uint16 "0x%02x"

print_buf :: Def('[ConstRef s PX4IOBuffer] ':-> ())
print_buf = proc "print_buf" $ \buf -> body $ do
  p "px4io_buf [ "
  len <- deref (buf ~> stringLengthL)
  arrayMap $ \ix -> do
    -- this is incorrect for len=0 but who cares
    deref ((buf ~> stringDataL) ! ix) >>= p8
    ifte_ (fromIx ix ==? len - 1)
          (p " " >> breakOut)
          (p ", ")
  p "]"
  call_ printf_sint32 "(%d)" len
  p "\n"
  where
  p = call_ printf_none
  p8 = call_ printf_uint8 "0x%02x"


app :: ([Module], [Located Artifact])
app = (modules, artifacts)
  where
  modules = [ test_mod
            , px4ioPackModule
            , px4ioCRCModule
            , px4ioBufferTypesModule
            , px4ioRequestTypesModule
            ]

  artifacts = serializeArtifacts

  test_mod = package "test" $ do
    incl main_proc
    incl test_encode_proc
    incl test_decode_proc
    incl print_request
    incl print_buf
    incl puts
    incl printf_uint8
    incl printf_uint16
    incl printf_sint32
    incl printf_none
    depend px4ioPackModule
    depend px4ioCRCModule
    depend px4ioBufferTypesModule
    depend px4ioRequestTypesModule

  test_encode_proc :: Def('[ IString, ConstRef s ('Struct "px4io_request")
                           ] ':-> ())
  test_encode_proc = proc "test_encode" $ \tname t_req -> body $ do
    call_ printf_none "\n----\ntest "
    call_ printf_none tname
    call_ printf_none "\ninput request:\n"
    call_ print_request t_req

    buf <- local izero
    r <- call px4io_pack buf t_req
    ifte_ (iNot r) (do call_ printf_none "encode failed\n") $ do
      call_ printf_none "encoded buf:\n"
      call_ print_buf (constRef buf)


  test_decode_proc :: Def('[ IString, ConstRef s PX4IOBuffer ] ':-> ())
  test_decode_proc = proc "test_decode" $ \tname buf -> body $ do
    call_ printf_none "\n----\ntest "
    call_ printf_none tname
    call_ printf_none ":\ninput buf:\n"
    call_ print_buf  buf
    result <- local izero
    r <- call px4io_unpack buf result
    ifte_ (r /=? 0) (do call_ printf_uint8 "decode_failed with error %d \n" r) $ do
      call_ printf_none "decoded request:\n"
      call_ print_request (constRef result)

  run_encode_test :: (GetAlloc eff ~ 'Scope s)
           => IString
           -> Init ('Struct "px4io_request")
           -> Ivory eff ()
  run_encode_test n i = do
    v <- local i
    call_ test_encode_proc n (constRef v)

  run_decode_test :: (GetAlloc eff ~ 'Scope s)
           => IString
           -> [Uint8]
           -> Ivory eff ()
  run_decode_test n i8s = do
    v <- local (istruct [ stringDataL .= iarray (map ival i8s)
                        , stringLengthL .= ival (fromIntegral (length i8s))
                        ])
    call_ test_decode_proc n (constRef v)

  main_proc :: Def('[]':->Sint32)
  main_proc = proc "main" $ body $ do
    run_encode_test "izero" izero
    run_encode_test "read one" $ istruct
      [ req_code .= ival request_read
      , count .= ival 1
      , page .= ival 1
      , offs .= ival 1
      -- there should be no payload (just count_code, crc, page, offs)
      , regs .= iarray [ ival 0x3344, ival 0x5566 ]
      ]
    run_encode_test "write one" $ istruct
      [ req_code .= ival request_write
      , count .= ival 1
      , page .= ival 2
      , offs .= ival 2
      -- only 0x3344 should be written.
      , regs .= iarray [ ival 0x3344, ival 0x5566 ]
      ]
    run_encode_test "write two" $ istruct
      [ req_code .= ival request_write
      , count .= ival 2
      , page .= ival 3
      , offs .= ival 3
      -- both should be written.
      , regs .= iarray [ ival 0x3344, ival 0x5566 ]
      ]

    run_decode_test "read_two_res"
      [ 0x42, 0xe1, 0x03, 0x03, 0x33, 0x44, 0x55, 0x66 ]


    run_encode_test "expect [ 0x01 0x29 0x00 0x00 0x00 0x00 ]" $ istruct
      [ req_code .= ival request_read
      , count    .= ival 1
      , page     .= ival 0
      , offs     .= ival 0
      , regs     .= iarray [ ival 0 ]
      ]


    run_encode_test "expect [ 0x01 0x16 0x00 0x01 0x04 0x00 ]" $ istruct
      [ req_code .= ival request_read
      , count    .= ival 1
      , page     .= ival 0
      , offs     .= ival 1
      , regs     .= iarray [ ival 0x0400 ]
      ]

    run_encode_test "expect [ 0x41 0xB1 0x32 0x01 0x00 0x00 ]" $ istruct
      [ req_code .= ival request_write
      , count    .= ival 1
      , page     .= ival 0x32
      , offs     .= ival 1
      , regs     .= iarray [ ival 0 ]
      ]
    ret 0




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

puts :: Def('[IString]:-> Sint32)
puts = importProc "puts" "stdio.h"

printf_uint8 :: Def('[IString, Uint8]:-> Sint32)
printf_uint8 = importProc "printf" "stdio.h"

printf_uint16 :: Def('[IString, Uint16]:-> Sint32)
printf_uint16 = importProc "printf" "stdio.h"

printf_sint32 :: Def('[IString, Sint32]:-> Sint32)
printf_sint32 = importProc "printf" "stdio.h"

printf_none :: Def('[IString]:-> Sint32)
printf_none = importProc "printf" "stdio.h"

print_request :: Def('[ConstRef s (Struct "px4io_request")
                      ] :-> ())
print_request = proc "print_request" $ \req -> body $ do
  p "px4io_request { code: "
  rcode <- deref (req ~> req_code)
  cond_
    [ rcode ==? request_read    ==> p "read"
    , rcode ==? request_write   ==> p "write"
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

print_buf :: Def('[ConstRef s PX4IOBuffer] :-> ())
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

  test_encode_proc :: Def('[ IString, ConstRef s (Struct "px4io_request")
                    ] :-> ())
  test_encode_proc = proc "test_encode" $ \tname t_req -> body $ do
    call_ printf_none "test "
    call_ printf_none tname
    call_ printf_none ":\ninitial value:\n"
    call_ print_request t_req

    buf <- local izero
    r <- call px4io_pack buf t_req
    ifte_ (iNot r) (do call_ printf_none "encode failed\n") $ do
      call_ print_buf (constRef buf)
      {-
      result <- local izero
      r2 <- call px4io_unpack (constRef buf) result
      ifte_ (iNot r2) (do call_ printf_none "decode_failed\n") $ do
        call_ printf_none "final value:\n"
        call_ print_request (constRef result)
      -}
    return ()

  run_test :: (GetAlloc eff ~ Scope s)
           => IString
           -> Init (Struct "px4io_request")
           -> Ivory eff ()
  run_test n i = do
    v <- local i
    call_ test_encode_proc n (constRef v)

  main_proc :: Def('[]:->Sint32)
  main_proc = proc "main" $ body $ do
    run_test "izero" izero
    run_test "one" $ istruct
      [ req_code .= ival request_read
      , count .= ival 1
      , page .= ival 1
      , offs .= ival 1
      , regs .= iarray [ ival 0x3344, ival 0x5566 ]
      ]


    ret 0




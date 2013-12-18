{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

--
-- USART.hs --- HWF4 USART driver interface.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.HWF4.USART where

import Ivory.Language

[ivory|
-- | USART structure.
abstract struct usart "hwf4/usart.h"
|]

usartModule :: Module
usartModule = package "bsp_hwf4wrapper_usart" $ do
  inclHeader "hwf4/usart.h"
  defStruct (Proxy :: Proxy "usart")
  incl usartInit
  incl usartEnable
  incl usartWriteTimeout
  incl usartReadTimeout
  incl usartWrite
  incl usartRead
  incl usartAvailable
  incl usartIsTxPending
  incl usartPeek
  incl usartTxspace

  defMemArea usart1
  defMemArea usart2
  defMemArea usart3
  defMemArea usart4
  defMemArea usart5
  defMemArea usart6

usartInit :: Def('[ Ref s (Struct "usart")
                  , Uint32 -- baud
                  ] :-> IBool)
usartInit  = importProc "usart_init" "hwf4/usart.h"

usartEnable :: Def('[ Ref s (Struct "usart") ] :-> ())
usartEnable  = importProc "usart_enable" "hwf4/usart.h"

usartWrite :: Def('[ Ref s (Struct "usart")
                   , ConstRef s1 (CArray (Stored Uint8)) -- buf
                   , Uint32 -- len
                   ] :-> Sint32)
usartWrite  = importProc "usart_write" "hwf4/usart.h"

usartWriteTimeout :: Def('[ Ref s (Struct "usart")
                          , Uint32 -- timeout
                          , ConstRef s1 (CArray (Stored Uint8)) -- buf
                          , Uint32 -- len
                          ] :-> Sint32)
usartWriteTimeout  = importProc "usart_write_timeout" "hwf4/usart.h"

usartRead :: Def('[ Ref s (Struct "usart")
                  , Ref s1 (CArray (Stored Uint8)) -- buf
                  , Uint32 -- len
                  ] :-> Sint32)
usartRead  = importProc "usart_read" "hwf4/usart.h"

usartReadTimeout :: Def('[ Ref s (Struct "usart")
                         , Uint32 -- timeout
                         , Ref s1 (CArray (Stored Uint8)) -- buf
                         , Uint32 -- len
                         ] :-> Sint32)
usartReadTimeout  = importProc "usart_read_timeout" "hwf4/usart.h"

usartAvailable :: Def('[ Ref s (Struct "usart") ] :-> Uint32)
usartAvailable  = importProc "usart_available" "hwf4/usart.h"

usartIsTxPending :: Def('[ Ref s (Struct "usart") ] :-> IBool)
usartIsTxPending  = importProc "usart_is_tx_pending" "hwf4/usart.h"

usartPeek :: Def('[ Ref s (Struct "usart")
                  , Ref s1 (CArray (Stored Uint8)) -- buf, len 1
                  ] :-> IBool)
usartPeek  = importProc "usart_peek" "hwf4/usart.h"

usartTxspace :: Def('[ Ref s (Struct "usart") ] :-> Uint32)
usartTxspace  = importProc "usart_txspace" "hwf4/usart.h"


-- Memory Areas ----------------------------------------------------------------

usart1 :: MemArea (Struct "usart")
usart1  = importArea "_usart1" "hwf4/usart.h"

usart2 :: MemArea (Struct "usart")
usart2  = importArea "_usart2" "hwf4/usart.h"

usart3 :: MemArea (Struct "usart")
usart3  = importArea "_usart3" "hwf4/usart.h"

usart4 :: MemArea (Struct "usart")
usart4  = importArea "_uart4" "hwf4/usart.h"

usart5 :: MemArea (Struct "usart")
usart5  = importArea "_uart5" "hwf4/usart.h"

usart6 :: MemArea (Struct "usart")
usart6  = importArea "_usart6" "hwf4/usart.h"


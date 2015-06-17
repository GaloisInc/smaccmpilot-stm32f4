{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module SMACCMPilot.Hardware.Tests.CopterMotors (app) where

import Data.Char (ord)

import Ivory.Language
import Ivory.Stdlib

import Ivory.Tower

import SMACCMPilot.Datalink.HXStream.Tower

import SMACCMPilot.Hardware.Tests.Platforms

app :: (e -> PX4Platform)
    -> Tower e ()
app topx4 = do
  c <- channel
  (_o, i) <- px4ConsoleTower topx4
  motorcontrol_input i (fst c)

motorcontrol_input ::
         ChanOutput (Stored Uint8)
      -> ChanInput (Array 4 (Stored IFloat))
      -> Tower e ()
motorcontrol_input byte_istream motorstream = do

  frame_istream <- channel
  airDataDecodeTower "motorcontrol" byte_istream (fst frame_istream)

  monitor "motorcontrol_input" $ do
    handler (snd frame_istream) "control_frame" $ do
      e <- emitter motorstream 1
      callback $ \f -> do
        bs <- sequence $ zipWith ref_is_char
                                [f ! i | i <- [0,1,2,3,4,5]]
                                "motors"
        when (iand bs) $ do
          motors <- local (iarray [])
          ref_to_motor (f ! 6) (motors ! 0)
          ref_to_motor (f ! 7) (motors ! 1)
          ref_to_motor (f ! 8) (motors ! 2)
          ref_to_motor (f ! 9) (motors ! 3)
          emit e (constRef motors)

iand :: [IBool] -> IBool
iand (b:bs) = b .&& (iand bs)
iand [] = true

ref_is_char :: ConstRef s (Stored Uint8) -> Char -> Ivory eff IBool
ref_is_char r c = do
  v <- deref r
  return (v ==? fromIntegral (ord (c)))

ref_to_motor :: ConstRef s (Stored Uint8) -> Ref s2 (Stored IFloat) -> Ivory eff ()
ref_to_motor r m = do
  v <- deref r
  ifte_ (v <=? 100)
        (store m ((safeCast v) / 100.0))
        (store m 0)


{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Ivory.BSP.STM32F4.Signalable
  ( stm32f4SignalableInstance
  , module Ivory.BSP.STM32F4.Signalable.Class
  ) where

import Ivory.Tower

import Ivory.BSP.ARMv7M.Exception
import Ivory.BSP.STM32F4.Interrupt
import Ivory.BSP.STM32F4.Signalable.Class

import Language.Haskell.TH.Syntax

stm32f4SignalableInstance :: Name -> Q [Dec]
stm32f4SignalableInstance pt = do
  n <- newName "n"
  return
    [ InstanceD []
        (AppT (ConT ''Signalable) (ConT pt))
        [ stinstance
        , signalsdef
        , signalnamedef n
        , signalfromnamedef n
        ]
    , InstanceD []
       (AppT (ConT ''STM32F4Signal) (ConT pt))
       [ stm32f4signaldef n
       ]
    ]
  where
  stname = mkName ((nameBase pt) ++ "Signal")
  stinstance =
    DataInstD [] ''SignalType [ConT pt]
              [NormalC stname [(NotStrict, ConT ''IRQ)]]
              [''Eq, ''Show]
  signalsdef =
    ValD (VarP 'signals)
         (NormalB (AppE (AppE (VarE 'map) (ConE stname)) irqlist))
         []

  signalnamedef n =
    FunD 'signalName
         [Clause [ConP stname [VarP n]]
                 (NormalB (AppE (VarE 'irqHandlerName) (VarE n)))
                 []]

  signalfromnamedef n =
    FunD 'signalFromName
         [Clause [VarP n]
                 (NormalB (AppE (ConE stname)
                          (AppE (VarE 'irqFromHandlerName) (VarE n))))
                 []]

  stm32f4signaldef n =
    FunD 'stm32f4Signal
         [Clause [VarP n]
                 (NormalB (AppE (ConE stname) (VarE n)))
                 []]

  irqlist :: Exp
  irqlist = AppE (AppE
    (VarE '(++))
    (AppE (AppE (VarE 'map) (ConE 'Exception))
          (AppE (VarE 'enumFrom) (ConE 'Reset))))
    (AppE (AppE (VarE 'map) (ConE 'Interrupt))
                (AppE (VarE 'enumFrom) (ConE 'WWDG)))


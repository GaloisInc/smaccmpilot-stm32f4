{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Ivory.BSP.STM32.Signalable.TH
  ( stm32SignalableInstance
  ) where

import Ivory.Tower

import Ivory.BSP.STM32.Signalable.Class
import Language.Haskell.TH.Syntax

stm32SignalableInstance :: Name -> Q [Dec]
stm32SignalableInstance pt = do
  n <- newName "n"
  return
    [ InstanceD []
        (AppT (ConT ''Signalable) (ConT pt))
        [ stinstance
        , signalnamedef n
        ]
    , InstanceD []
       (AppT (ConT ''STM32Signal) (ConT pt))
       [ stm32signaldef n
       ]
    ]
  where
  stname = mkName ((nameBase pt) ++ "Signal")
  stinstance =
    DataInstD [] ''SignalType [ConT pt]
              [NormalC stname [(NotStrict, ConT ''IRQ)]]
              [''Eq, ''Show]
  signalnamedef n =
    FunD 'signalName
         [Clause [ConP stname [VarP n]]
                 (NormalB (AppE (VarE 'irqHandlerName) (VarE n)))
                 []]
  stm32signaldef n =
    FunD 'stm32Signal
         [Clause [VarP n]
                 (NormalB (AppE (ConE stname) (VarE n)))
                 []]


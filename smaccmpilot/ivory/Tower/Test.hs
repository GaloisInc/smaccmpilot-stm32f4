
module Tower.Test where

import Tower.FooBarAssembly

import Ivory.Language
import Ivory.ADL
import Ivory.ADL.Compile.FreeRTOS

towerModules :: [Module]
towerModules = compileADL fooBarAssembly

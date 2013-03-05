
module Tower.Test where

import Tower.FooBarAssembly

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.Compile.FreeRTOS

towerModules :: [Module]
towerModules = compileTower fooBarAssembly

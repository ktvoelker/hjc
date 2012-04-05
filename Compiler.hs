
module Compiler where

import HscTypes as Hs

import Ast as Js

compileModule :: Hs.ModGuts -> Js.Module
compileModule = const ()


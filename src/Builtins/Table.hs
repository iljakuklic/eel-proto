
module Builtins.Table (builtInsTable) where

import Parser.State
import Builtins.Builtins
import Sema.Term
import qualified Data.Map as M

builtInsTable :: SymTable m
builtInsTable = M.fromList [ (builtInName bi, FDBuiltIn bi) | bi <- allBuiltIns ]


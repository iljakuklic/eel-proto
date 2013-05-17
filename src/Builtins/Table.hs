{-
    EEL -- Extensible Experimental Language
    by Lukáš Kuklínek, 2013
-}

module Builtins.Table (builtInsTable) where

import Builtins.Builtins
import Sema.Term

import qualified Data.Map as M

-- | All the builtins as a map from symbols to builtin definition
builtInsTable :: SymTable m
builtInsTable = M.fromList [ (builtInName bi, FDBuiltIn bi) | bi <- allBuiltIns ]


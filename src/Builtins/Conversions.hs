
module Builtins.Conversions (termToString) where

import Sema.Term

termToString (TList _ xs) = map (\(TChar _ c) -> c) xs
termToString _ = undefined


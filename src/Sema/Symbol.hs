
module Sema.Symbol(Symbol(..), genSymbols) where

-- | Identifier symbol representation
newtype Symbol = Symbol String deriving (Eq, Ord)

instance Show Symbol where
    show (Symbol s) = s

-- | Generate an  infinite stream of symbols
genSymbols = map Symbol . tail . concat $ iterate (\xss -> [x:xs | x <- symChars, xs <- xss]) [""]
symChars  = ['a'..'d'] ++ ['g','h','m','n'] ++ ['p'..'v']

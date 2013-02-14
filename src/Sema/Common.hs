
module Sema.Common(Symbol(..), genSymbols) where

newtype Symbol = Symbol String deriving (Eq, Ord)

instance Show Symbol where
    show (Symbol s) = s

genSymbols = map Symbol . tail . concat $ iterate (\xss -> [x:xs | x <- symChars, xs <- xss]) [""]
symChars  = ['a'..'d'] ++ ['g','h','m','n'] ++ ['p'..'v']


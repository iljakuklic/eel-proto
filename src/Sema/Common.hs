
module Sema.Common where

newtype Symbol = Symbol String deriving Eq

instance Show Symbol where
    show (Symbol s) = s



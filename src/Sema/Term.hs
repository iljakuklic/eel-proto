
module Sema.Term (
        Term(..), FunctionCall(..), FunctionDef(..), BuiltIn(..),
        SymTable, Stack(..),
        getMeta, onStack
    ) where

import qualified Data.Map as M
import Data.Functor

import Sema.Common
import Builtins.Builtins

-- | Term Representation
data Term m
     = TFunc  m (FunctionCall)      -- ^ function invokation
     | TComp  m (Term m) (Term m)   -- ^ function composition
     | TQuot  m (Term m)            -- ^ anonymous function quotation
     | TInt   m Int                 -- ^ integer value
     | TFloat m Float               -- ^ floating-point value
     | TChar  m Char                -- ^ character value
     | TLeft  m (Term m)            -- ^ sum type: left case
     | TRight m (Term m)            -- ^ sum type: right case
     | TPair  m (Term m) (Term m)   -- ^ product value
     | TList  m [Term m]            -- ^ list of values
     | TUnit  m                     -- ^ unit type value

-- | function invokation
data FunctionCall
     = FCUser    Symbol    -- ^ user-defined function
     | FCBuiltIn BuiltIn   -- ^ built-in function

-- | function definition
data FunctionDef m
     = FDUser    (Term m)  -- ^ user-defined function
     | FDBuiltIn BuiltIn   -- ^ built-in function
     deriving (Show)

-- | Symbol table
type SymTable m = M.Map Symbol (FunctionDef m)
-- | Runtime stack
newtype Stack   = Stack [Term ()]

-- | get term metadata
getMeta (TFunc  m _)   = m
getMeta (TComp  m _ _) = m
getMeta (TQuot  m _)   = m
getMeta (TInt   m _)   = m
getMeta (TFloat m _)   = m
getMeta (TChar  m _)   = m
getMeta (TLeft  m _)   = m
getMeta (TRight m _)   = m
getMeta (TPair  m _ _) = m
getMeta (TList  m _)   = m
getMeta (TUnit  m)     = m

-- | perform a function on stack
onStack f stk = Stack (f (let Stack s = stk in s))

instance Show (Term m) where
    show (TFunc  _ f)   = show f
    show (TComp  _ f g) = show f ++ " " ++ show g
    show (TQuot  _ q)   = "[" ++ show q ++ "]"
    show (TInt   _ x)   = show x
    show (TFloat _ x)   = show x
    show (TChar  _ x)   = show x
    show (TRight _ x)   = "{" ++ show x ++ ">"
    show (TLeft  _ x)   = "<" ++ show x ++ "}"
    show (TPair  _ a b) = "{" ++ show a ++ "," ++ show b ++ "}"
    show (TList  _ xs)  = "(" ++ tail (init (show xs)) ++ ")"
    show (TUnit  _)     = "#"

instance Functor Term where
    fmap f (TFunc  m a)   = TFunc  (f m) a
    fmap f (TComp  m a b) = TComp  (f m) (fmap f a) (fmap f b)
    fmap f (TQuot  m a)   = TQuot  (f m) (fmap f a)
    fmap f (TInt   m a)   = TInt   (f m) a
    fmap f (TFloat m a)   = TFloat (f m) a
    fmap f (TChar  m a)   = TChar  (f m) a
    fmap f (TRight m a)   = TRight (f m) (fmap f a)
    fmap f (TLeft  m a)   = TLeft  (f m) (fmap f a)
    fmap f (TPair  m a b) = TPair  (f m) (fmap f a) (fmap f b)
    fmap f (TList  m as)  = TList  (f m) (fmap (fmap f) as)
    fmap f (TUnit  m)     = TUnit  (f m)

instance Show FunctionCall where
    show (FCUser s)    = show s
    show (FCBuiltIn b) = show b

instance Show Stack where
    show (Stack s) = "$" ++ show (reverse s) ++ "$"

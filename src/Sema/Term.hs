
module Sema.Term (
        Term(..), FunctionDef(..), BuiltIn(..),
        SymTable, Stack(..),
        getMeta, setMeta, modifyMeta, onStack
    ) where

import qualified Data.Map as M

import Sema.Common
import Builtins.Builtins

-- | Term Representation
data Term m
     = TFunc  m (Symbol)            -- ^ function invokation
     | TComp  m (Term m) (Term m)   -- ^ function composition
     | TQuot  m (Term m)            -- ^ anonymous function quotation
     | TInt   m Int                 -- ^ integer value
     | TFloat m Float               -- ^ floating-point value
     | TChar  m Char                -- ^ character value
     | TSumA  m (Term m)            -- ^ sum type: left case
     | TSumB  m (Term m)            -- ^ sum type: right case
     | TPair  m (Term m) (Term m)   -- ^ product value
     | TList  m [Term m]            -- ^ list of values
     | TUnit  m                     -- ^ unit type value

-- | function definition
data FunctionDef m
     = FDUser    (Term m)  -- ^ user-defined function
     | FDBuiltIn BuiltIn   -- ^ built-in function
     deriving (Show)

-- | Symbol table
type SymTable m = M.Map Symbol (FunctionDef m)
-- | Compile-time stack
newtype Stack m = Stack [Term m]

-- | get term metadata
getMeta (TFunc  m _)   = m
getMeta (TComp  m _ _) = m
getMeta (TQuot  m _)   = m
getMeta (TInt   m _)   = m
getMeta (TFloat m _)   = m
getMeta (TChar  m _)   = m
getMeta (TSumA  m _)   = m
getMeta (TSumB  m _)   = m
getMeta (TPair  m _ _) = m
getMeta (TList  m _)   = m
getMeta (TUnit  m)     = m

-- | set term metadata
setMeta (TFunc  _ a)   m = TFunc  m a
setMeta (TComp  _ a b) m = TComp  m a b
setMeta (TQuot  _ a)   m = TQuot  m a
setMeta (TInt   _ a)   m = TInt   m a
setMeta (TFloat _ a)   m = TFloat m a
setMeta (TChar  _ a)   m = TChar  m a
setMeta (TSumA  _ a)   m = TSumA  m a
setMeta (TSumB  _ a)   m = TSumB  m a
setMeta (TPair  _ a b) m = TPair  m a b
setMeta (TList  _ a)   m = TList  m a
setMeta (TUnit  _)     m = TUnit  m

-- | alter term metadata
modifyMeta f t = setMeta t . f $ getMeta t

-- | perform a function on stack
onStack f stk = Stack (f (let Stack s = stk in s))

instance Show (Term m) where
    show (TFunc  _ (Symbol "id"))   = ""
    show (TFunc  _ f)   = show f
    show (TComp  _ f g) = show f ++ " " ++ show g
    show (TQuot  _ q)   = "[ " ++ show q ++ "]"
    show (TInt   _ x)   = show x
    show (TFloat _ x)   = show x
    show (TChar  _ x)   = show x
    show (TSumA  _ x)   = "(A:" ++ show x ++ ")"
    show (TSumB  _ x)   = "(B:" ++ show x ++ ")"
    show (TPair  _ a b) = "({" ++ show a ++ "," ++ show b ++ "})"
    show (TList  _ xs)  = "("  ++ show xs ++ ")"
    show (TUnit  _)     = "#"

instance Functor Term where
    fmap f (TFunc  m a)   = TFunc  (f m) a
    fmap f (TComp  m a b) = TComp  (f m) (fmap f a) (fmap f b)
    fmap f (TQuot  m a)   = TQuot  (f m) (fmap f a)
    fmap f (TInt   m a)   = TInt   (f m) a
    fmap f (TFloat m a)   = TFloat (f m) a
    fmap f (TChar  m a)   = TChar  (f m) a
    fmap f (TSumA  m a)   = TSumA  (f m) (fmap f a)
    fmap f (TSumB  m a)   = TSumB  (f m) (fmap f a)
    fmap f (TPair  m a b) = TPair  (f m) (fmap f a) (fmap f b)
    fmap f (TList  m as)  = TList  (f m) (fmap (fmap f) as)
    fmap f (TUnit  m)     = TUnit  (f m)

instance Show (Stack m) where
    show (Stack s) = "$" ++ show (reverse s) ++ "$"


module Sema.Types(
    -- * Data structures
    Type(..), Type2(..),
    -- * Helper functions
    tUnit, tChar, tNum, tReal, tBool, tString, tMaybe
  ) where

import Sema.Common

-- | Type specification representation.
data Type v = TAtom Symbol                  -- ^ atomic type
            | TVar  v                       -- ^ type variable
            | TBin Type2 (Type v) (Type v)  -- ^ binary type
            | TList (Type v)                -- ^ list type
            deriving Eq

-- | Compound type variants (sum, product, function)
data Type2 = TSum | TProd | TFunc deriving Eq

-- functor
instance Functor Type where
    fmap f (TVar v) = TVar (f v)
    fmap f (TList t) = TList (fmap f t)
    fmap f (TBin c a b) = TBin c (fmap f a) (fmap f b)
    fmap f (TAtom n) = TAtom n

-- | Create atom type
tAtom name = TAtom (Symbol name)

tSum  = TBin TSum
tProd = TBin TProd
tFunc = TBin TFunc

-- | Unit type shortcut.
tUnit :: Type v
tUnit = tAtom "unit"

-- | Integer type shortcut.
tNum :: Type v
tNum = tAtom "num"

-- | Real (float) type shortcut.
tReal :: Type v
tReal = tAtom "real"

-- | Character type shortcut.
tChar :: Type v
tChar = tAtom "char"

-- | Maybe type shortcut
tMaybe :: Type v -> Type v
tMaybe t = tSum tUnit t

-- | Boolean type shortcut.
tBool :: Type v
tBool = tMaybe tUnit

-- | String type shortcut.
tString :: Type v
tString = TList tChar


-- show instances
instance (Eq v, Show v) => Show (Type v) where
  show = show' False
   where
    -- "syntax sugar" aliases
    show' p t               | t == tString = "string"           -- string  == [char]
    show' p t               | t == tBool   = "bool"             -- boolean == (unit | unit)
    show' p (TBin TSum u b) | u == tUnit = show' True b ++ "?"  -- maybe b == (unit | b)
    -- non-sugared rendering
    show' p (TAtom s)    = show s
    show' p (TList a)    = "[" ++ show' True a ++ "]"
    show' p (TVar v)     = '%' : show v
    show' p (TBin c a b) = parF $ show' pl a ++ t_op c ++ show' pr b
      where
        parF = case (p, c) of (False, TProd) -> id; _ -> parens
        p' TFunc = (False, False)
        p' TProd = (p, True)
        p' TSum  = (True, True)
        (pl, pr) = p' c
        parens str = "(" ++ str ++ ")"
        t_op c = case c of TSum -> " | "; TProd -> ", "; TFunc -> " -> "



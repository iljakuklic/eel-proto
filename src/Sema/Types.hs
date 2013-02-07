
module Sema.Types(
    -- * Data structures
    TRow(..), Type(..), Signature (..),
    -- * Helper functions
    tUnit, tChar, tNum, tReal, tBool, tString, tMaybe
  ) where

import Sema.Common

-- | Row types.
data TRow v = RCons (TRow v) (Type v) -- ^ constructed row
            | RVar v                  -- ^ row variable
            | RNil                    -- ^ empty row
            deriving Eq

-- | Type specification representation.
data Type v = TUnit                    -- ^ unit type
            | TAtom Symbol             -- ^ atomic type
            | TSum  (Type v) (Type v)  -- ^ sum type (tagged disjoint union)
            | TProd (Type v) (Type v)  -- ^ product type
            | TList (Type v)           -- ^ list type
            | TFunc (Signature v)      -- ^ function type
            | TVar  v                  -- ^ type variable
            deriving Eq

-- | Function type.
data Signature v = Signature (TRow v) (TRow v) deriving Eq

-- | Create atom type
tAtom name = TAtom (Symbol name)

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
tMaybe t = TSum tUnit t

-- | Boolean type shortcut.
tBool :: Type v
tBool = tMaybe tUnit

-- | String type shortcut.
tString :: Type v
tString = TList tChar


-- show instances
instance (Eq v, Show v) => Show (Type v) where
    -- "syntax sugar" aliases
    show t          | t == tString = "string"       -- string  == [char]
    show t          | t == tBool   = "bool"         -- boolean == (unit | unit)
    show (TSum u b) | u == tUnit   = show b ++ "?"  -- maybe b == (unit | b)
    -- non-sugared rendering
    show (TAtom s)   = show s
    show (TSum a b)  = binaryS a b " | "
    show (TProd a b) = binaryS a b ", "
    show (TList a)   = "[" ++ show a ++ "]"
    show (TFunc f)   = show f
    show (TVar v)    = '+' : show v

instance (Eq v, Show v) => Show (Signature v) where
    show (Signature rin rout) = parensS $ show rin ++ " -> " ++ show rout

instance (Eq v, Show v) => Show (TRow v) where
    show RNil = ""
    show (RVar v) = '#' : show v
    show (RCons r t) = show r ++ (case r of RNil -> ""; _ -> " . ") ++ show t

binaryS a b op = parensS $ show a ++ op ++ show b
parensS str = "(" ++ str ++ ")"


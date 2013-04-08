

module Sema.Types(
    -- * Data structures
    Type(..), Type2(..), Row,
    -- * Type manipulation functions
    toListUniq, tyRow, isTypeMono, isRowMono,
    -- * Type representation smart constructors
    tUnit, tChar, tProd, tFunc, tNum, tReal, tBool, tString, tMaybe
  ) where

import Sema.Common

import Prelude hiding (all)
import Data.Foldable
import Data.Monoid
import Data.List hiding (all)

-- | Type specification representation.
data Type v = TAtom Symbol                  -- ^ atomic type
            | TVar  v                       -- ^ type variable
            | TList (Type v)                -- ^ list type
            | TBin Type2 (Type v) (Type v)  -- ^ binary type
            deriving Eq

-- | Compound type variants (sum, product, function)
data Type2 = TSum | TProd | TFunc deriving Eq

-- | Row is a list of types
newtype Row v = Row [Type v] deriving (Eq)

instance Functor Type where
    fmap f (TVar v) = TVar (f v)
    fmap f (TList t) = TList (fmap f t)
    fmap f (TBin c a b) = TBin c (fmap f a) (fmap f b)
    fmap _f (TAtom n) = TAtom n

instance Foldable Type where
    foldMap f (TVar v) = f v
    foldMap f (TList t) = foldMap f t
    foldMap f (TBin _ a b) = foldMap f a <> foldMap f b
    foldMap _f (TAtom _) = mempty

-- forward row instances to list
instance Functor  Row where fmap    f (Row xs) = Row $ (fmap . fmap) f xs
instance Foldable Row where foldMap f (Row xs) = foldMap (foldMap f) xs

-- | Collect list of type variables
toListUniq :: (Eq a, Foldable t) => t a -> [a]
toListUniq = nub . toList

-- | Extract row spine from type (top of the stack in in the beginning)
tyRow :: (Eq v) => Type v -> Row v
tyRow = Row . tyRow'
  where
    tyRow' (TBin TProd t1 t2) = t2 : tyRow' t1
    tyRow' t = [t]

-- | Chack if type is monomorphic (no type variables)
isTypeMono :: Type v -> Bool
isTypeMono = null . toList

-- | Check if row is monomorphic (no type vars except row polymorphism)
isRowMono :: Row v -> Bool
isRowMono (Row xs) = all isTypeMono (init xs)

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
    show' _p t               | t == tString = "string"           -- string  == [char]
    show' _p t               | t == tBool   = "bool"             -- boolean == (unit | unit)
    show' _p (TBin TSum u b) | u == tUnit = show' True b ++ "?"  -- maybe b == (unit | b)
    -- non-sugared rendering
    show' _p (TAtom s)    = show s
    show' _p (TList a)    = "[" ++ show' True a ++ "]"
    show' _p (TVar v)     = '%' : show v
    show' p (TBin c a b) = parF $ show' pl a ++ t_op c ++ show' pr b
      where
        parF = case (p, c) of (False, TProd) -> id; _ -> parens
        p' TFunc = (False, False)
        p' TProd = (False, True)
        p' TSum  = (True, True)
        (pl, pr) = p' c
        parens str = "(" ++ str ++ ")"
        t_op cc = case cc of TSum -> " | "; TProd -> ", "; TFunc -> " -> "

instance (Eq v, Show v) => Show (Row v) where
  show (Row xs) = "[[" ++ show (reverse xs) ++ ">>"


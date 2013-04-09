

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
import Data.Traversable
import Control.Applicative
import Data.List hiding (all)

-- | Type specification representation.
data Type v = TyAtom Symbol                  -- ^ atomic type
            | TyVar  v                       -- ^ type variable
            | TyList (Type v)                -- ^ list type
            | TyBin Type2 (Type v) (Type v)  -- ^ binary type
            deriving Eq

-- | Compound type variants (sum, product, function)
data Type2 = TySum | TyProd | TyFunc deriving Eq

-- | Row is a list of types
newtype Row v = Row [Type v] deriving (Eq)

instance Functor  Type where fmap    = fmapDefault
instance Foldable Type where foldMap = foldMapDefault

instance Traversable Type where
    traverse _ (TyAtom a)    = pure (TyAtom a)
    traverse f (TyVar v)     = TyVar <$> f v
    traverse f (TyList t)    = TyList <$> traverse f t
    traverse f (TyBin t a b) = TyBin t <$> traverse f a <*> traverse f b

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
    tyRow' (TyBin TyProd t1 t2) = t2 : tyRow' t1
    tyRow' t = [t]

-- | Chack if type is monomorphic (no type variables)
isTypeMono :: Type v -> Bool
isTypeMono = null . toList

-- | Check if row is monomorphic (no type vars except row polymorphism)
isRowMono :: Row v -> Bool
isRowMono (Row xs) = all isTypeMono (init xs)

-- | Create atom type
tAtom name = TyAtom (Symbol name)

tSum  = TyBin TySum
tProd = TyBin TyProd
tFunc = TyBin TyFunc

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
tString = TyList tChar


-- show instances
instance (Eq v, Show v) => Show (Type v) where
  show = show' False
   where
    -- "syntax sugar" aliases
    show' _ t                 | t == tString = "string"           -- string  == [char]
    show' _ t                 | t == tBool   = "bool"             -- boolean == (unit | unit)
    show' _ (TyBin TySum u b) | u == tUnit = show' True b ++ "?"  -- maybe b == (unit | b)
    -- non-sugared rendering
    show' _ (TyAtom s)    = show s
    show' _ (TyList a)    = "[" ++ show' True a ++ "]"
    show' _ (TyVar v)     = '%' : show v
    show' p (TyBin c a b) = parF $ show' pl a ++ t_op c ++ show' pr b
      where
        parF = case (p, c) of (False, TyProd) -> id; _ -> parens
        p' TyFunc = (False, False)
        p' TyProd = (False, True)
        p' TySum  = (True, True)
        (pl, pr)  = p' c
        parens str = "(" ++ str ++ ")"
        t_op cc = case cc of TySum -> " | "; TyProd -> ", "; TyFunc -> " -> "

instance (Eq v, Show v) => Show (Row v) where
  show (Row xs) = "[[" ++ show (reverse xs) ++ ">>"


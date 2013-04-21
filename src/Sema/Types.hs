

module Sema.Types(
    -- * Data structures
    Type(..), Type2(..), TyPhaseEnum(..), Row,
    -- * Type manipulation functions
    toListUniq, tyRow, isTypeMono, isRowMono,
    -- * Type representation smart constructors
    tUnit, tChar, tSum, tList, tProd, tFunc, tInt, tFloat, tBool, tString, tMaybe,
    -- * Helpers
    TyVars, genTyVar, genTyVars
  ) where

import Sema.Symbol

import Prelude hiding (all, notElem)
import Data.Foldable
import Data.Traversable
import Control.Applicative
import Data.List hiding (all, notElem)


-- | Type variable supply
class TyVars v where
    -- | List of all possible variable names
    genTyVars :: [v]

instance TyVars Integer where genTyVars = [1..]
instance TyVars Symbol  where genTyVars = genSymbols

-- | generate a new type variable not already included in given type expression
genTyVar t = head [ v | v <- genTyVars, v `notElem` t ]


-- | Type specification representation.
data Type v = TyAtom Symbol                             -- ^ atomic type
            | TyVar  v                                  -- ^ type variable
            | TyList (Type v)                           -- ^ list type
            | TyBin (Type2 (Type v)) (Type v) (Type v)  -- ^ binary type
            | TyPhase TyPhaseEnum                       -- ^ phase type
            deriving Eq

-- Phase Types
data TyPhaseEnum = TyRun | TyCompile | TyParse deriving Eq

-- | Compound type variants (sum, product, function)
data Type2 v = TySum | TyProd | TyFunc v deriving Eq

-- | Row is a list of types
newtype Row v = Row [Type v] deriving (Eq)

-- boring typeclass instances...

instance Functor  Type where fmap    = fmapDefault
instance Foldable Type where foldMap = foldMapDefault
instance Traversable Type where
    traverse _ (TyAtom a)    = pure (TyAtom a)
    traverse f (TyVar v)     = TyVar <$> f v
    traverse f (TyList t)    = TyList <$> traverse f t
    traverse f (TyBin t a b) = flip TyBin <$> traverse f a <*> (traverse.traverse) f t <*> traverse f b
    traverse _ (TyPhase p)   = pure (TyPhase p)
instance Monad Type where
    return = TyVar
    (TyVar v)     >>= f = f v
    (TyAtom a)    >>= _ = TyAtom a
    (TyPhase p)   >>= _ = TyPhase p
    (TyList t)    >>= f = TyList (t >>= f)
    (TyBin t a b) >>= f = TyBin (fmap (>>= f) t) (a >>= f) (b >>= f)

instance Functor Type2 where fmap = fmapDefault
instance Foldable Type2 where foldMap = foldMapDefault
instance Traversable Type2 where
    traverse _ TySum = pure TySum; traverse _ TyProd = pure TyProd
    traverse f (TyFunc ph) = TyFunc <$> f ph

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
-- | Sum type shortcut
tSum  = TyBin TySum
-- | Product type shortcut
tProd = TyBin TyProd
-- | Function type shortcut
tFunc ph = TyBin (TyFunc ph)
-- | List type shortcut
tList = TyList

-- | Unit type shortcut.
tUnit :: Type v
tUnit = tAtom "U"

-- | Integer type shortcut.
tInt :: Type v
tInt = tAtom "I"

-- | Real (float) type shortcut.
tFloat :: Type v
tFloat = tAtom "F"

-- | Character type shortcut.
tChar :: Type v
tChar = tAtom "C"

-- | Maybe type shortcut
tMaybe :: Type v -> Type v
tMaybe t = tSum t tUnit

-- | Boolean type shortcut.
tBool :: Type v
tBool = tMaybe tUnit

-- | String type shortcut.
tString :: Type v
tString = tList tChar


-- show instances
instance (Eq v, Show v) => Show (Type v) where
  show = show' False
   where
    -- "syntax sugar" aliases
    show' _ t                 | t == tString = "S"                -- string  == [char]
    show' _ t                 | t == tBool   = "B"                -- boolean == (unit | unit)
    show' _ (TyBin TySum b u) | u == tUnit = show' True b ++ "?"  -- maybe b == (b | unit)
    -- non-sugared rendering
    show' _ (TyAtom s)    = show s
    show' _ (TyList a)    = "[" ++ show' True a ++ "]"
    show' _ (TyVar v)     = show v
    show' _ (TyPhase TyRun)     = "-"
    show' _ (TyPhase TyCompile) = "+"
    show' _ (TyPhase TyParse)   = "*"
    show' p (TyBin c a b) = parF $ show' pl a ++ t_op c ++ show' pr b
      where
        parF = case (p, c) of (False, TyProd) -> id; _ -> parens
        p' (TyFunc _) = (False, False)
        p' TyProd = (False, True)
        p' TySum = (True, True)
        (pl, pr) = p' c
        parens str = "(" ++ str ++ ")"
        t_op cc = case cc of TySum -> " | "; TyProd -> ", "; TyFunc ph -> " --" ++ show' False ph ++ "> "

instance (Eq v, Show v) => Show (Row v) where
  show (Row xs) = "[[" ++ show (reverse xs) ++ ">>"


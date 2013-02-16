
module Sema.Infer {-(TypeError(..), subst, unify)-} where

import Sema.Types
import Sema.Common

import Prelude hiding (notElem, elem, maximum, concat)
import qualified Data.Map as M
import Control.Monad.Error
import Control.Monad.State
import Data.Foldable
import Data.List hiding (notElem)

-- | Substitution type
type Substitution v = M.Map v (Type v)

-- | Type error encountered during the inference
data TypeError v = TEOccurs v (Type v)         -- occurs check failed
                 | TEUnify (Type v) (Type v)   -- unification failed

instance Error (TypeError v) where

instance (Show v, Eq v) => Show (TypeError v) where
    show (TEOccurs v t) = "Occurs check failed: " ++ show v ++ " = " ++ show t
    show (TEUnify  a b) = "Could not unify: " ++ show a ++ " = " ++ show b

-- | Type variable supply
class TyVars v where
    -- | List of all possible variable names
    genTyVars :: [v]

-- | generate a new type variable not already included in given type expression
genTyVar t = head [ v | v <- genTyVars, v `notElem` toList t ]

instance TyVars Integer where genTyVars = [1..]
instance TyVars Symbol  where genTyVars = genSymbols

-- | Type variable substitution
subst sm t = subst' t
  where
    subst' a@(TAtom _)  = a
    subst' v@(TVar n)   = maybe v id (M.lookup n sm)
    subst' (TBin b x y) = TBin b (subst' x) (subst' y)
    subst' (TList x)    = TList (subst' x)

-- | Type unification
--unify :: (Ord v, MonadError (TypeError v) m) => Type v -> Type v -> m (M.Map v (Type v))
unify u v | u == v = return M.empty
unify (TVar v) t = if v `notElem` t
    then return (M.singleton v t)
    else throwError (TEOccurs v t)
unify t v@(TVar _) = unify v t
unify (TList u) (TList v) = unify u v
unify (TBin u a b) (TBin v c d) | u == v = do
    s1 <- unify a c
    s2 <- unify (subst s1 b) (subst s1 d)
    return (M.union s2 (fmap (subst s2) s1))
unify t1 t2 = throwError (TEUnify t1 t2)

-- | rename variables in t2 that collide with variable names in t1
unCollide t1 t2 = subst sm t2
  where
    collisions = intersect (toList t1) (toList t2)
    used       = union (toList t1) (toList t2)
    availVars  = [ TVar v | v <- genTyVars, v `notElem` used ]
    sm         = M.fromList (zip collisions availVars)

-- | infer type of the empty function (= identity)
inferEmpty = TBin TFunc a a
    where a = TVar (head genTyVars)

-- | infer type of a quotation of the function f
inferQuotation f = TBin TFunc v (TBin TProd v f)
  where v = TVar (genTyVar f)

-- | infer type of the function composition g'(f(x)), written f g'
inferComposition f@(TBin TFunc a b) g' = do
    sm <- unify b c
    return (TBin TFunc (subst sm a) (subst sm d))
  where
    g@(TBin TFunc c d) = unCollide f g'


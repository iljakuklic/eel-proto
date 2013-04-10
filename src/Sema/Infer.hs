
module Sema.Infer {-(TypeError(..), subst, unify)-} where

import Sema.Types
import Sema.Common
import Sema.Error

import Prelude hiding (notElem, elem, maximum, concat)
import qualified Data.Map as M
import Control.Monad.Error
import Control.Monad.State
import Data.Foldable
import Data.List hiding (notElem)

-- | Substitution type
type Substitution v = M.Map v (Type v)

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
    subst' a@(TyAtom _)  = a
    subst' v@(TyVar n)   = maybe v id (M.lookup n sm)
    subst' (TyBin b x y) = TyBin b (subst' x) (subst' y)
    subst' (TyList x)    = TyList (subst' x)

-- | Substitution composition
substComp s1 s2 = (M.union s2 (fmap (subst s2) s1))

-- | Type unification
--unify :: (Ord v, MonadError (TypeError v) m) => Type v -> Type v -> m (M.Map v (Type v))
unify u v | u == v = return M.empty
unify (TyVar v) t = if v `notElem` t
    then return (M.singleton v t)
    else throwError (SEOccurs v t)
unify t v@(TyVar _) = unify v t
unify (TyList u) (TyList v) = unify u v
unify (TyBin u a b) (TyBin v c d) | u == v = do
    s1 <- unify a c
    s2 <- unify (subst s1 b) (subst s1 d)
    return (substComp s1 s2)
unify t1 t2 = throwError (SEUnify t1 t2)

-- | rename variables in t2 that collide with variable names in t1
unCollide t1 t2 = subst sm t2
  where
    collisions = intersect (toList t1) (toList t2)
    used       = union (toList t1) (toList t2)
    availVars  = [ TyVar v | v <- genTyVars, v `notElem` used ]
    sm         = M.fromList (zip collisions availVars)

-- | rename type variables to nice names
niceTyVars t = subst (M.fromList (zip (toListUniq t) (map TyVar genTyVars))) t

-- | infer type of a quotation of the function f
inferQuotation f = TyBin TyFunc v (TyBin TyProd v f)
  where v = niceTyVars $ TyVar (genTyVar f)

-- | infer type of the function composition g'(f(x)), written f g'
inferComposition f@(TyBin TyFunc a b) g' = do
    sm <- unify b c
    return $ niceTyVars (TyBin TyFunc (subst sm a) (subst sm d))
  where
    g@(TyBin TyFunc c d) = unCollide f g'

-- | infer type of pushing argument to the stack
inferPush t = let a = TyVar $ genTyVar t in a `tFunc` (a `tProd` t)



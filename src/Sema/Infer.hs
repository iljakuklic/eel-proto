
module Sema.Infer (infer) where

import Sema.Types
import Sema.Common
import Sema.Error
import Sema.Term

import Prelude hiding (notElem, elem, maximum, concat)
import qualified Data.Map as M
import Control.Monad.Error
import Control.Applicative
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

ta = TyVar (genTyVar tUnit)

instance TyVars Integer where genTyVars = [1..]
instance TyVars Symbol  where genTyVars = genSymbols

-- | Type variable substitution
subst :: (Ord v) => Substitution v -> Type v -> Type v
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

-- | Unification applied to types
typeUnify a b = fmap (flip subst a) (unify a b)

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
    let (TyBin TyFunc c d) = unCollide f g'
    sm <- unify b c
    return $ niceTyVars (TyBin TyFunc (subst sm a) (subst sm d))
inferComposition _ _ = error "Invalit composition inference"

-- | Main type inference engine
infer _ (TUnit _)    = return tUnit
infer _ (TInt  _ _)  = return tInt
infer _ (TChar _ _)  = return tChar
infer _ (TFloat _ _) = return tFloat
infer env (TList _ xs) = tList <$> (mapM (infer env) xs >>= foldM typeUnify ta)
infer env (TQuot _ f)  = inferQuotation <$> (infer env f)
infer env (TComp _ f g) = join (inferComposition <$> infer env f <*> infer env g)
infer env (TPair _ a b) = tProd <$> infer env a <*> infer env b
infer env (TSumA _ a)   = tSum <$> infer env a <*> pure (TyVar $ genTyVar ta)
infer env (TSumB _ b)   = tSum (TyVar $ genTyVar ta) <$> infer env b
infer env (TFunc _ f)  = maybe (throwError $ SESymbol f) return (lookup f env)

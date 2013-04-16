
module Sema.Infer (infer, inferVal) where

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
subst sm ty = ty >>= (\n -> maybe (TyVar n) id (M.lookup n sm))

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

-- | uncollide over types
unCollideT f t1 t2 = f t1 (unCollide t1 t2)

-- | rename type variables to nice names
niceTyVars t = subst (M.fromList (zip (toListUniq t) (map TyVar genTyVars))) t

-- | infer type of a literal being pushed to the stack
inferLiteral t = niceTyVars (v `tFunc` (v `tProd` t))
    where v = TyVar (genTyVar t)

-- | infer type of the function composition g'(f(x)), written f g'
inferComposition f@(TyBin TyFunc a b) g' = do
    let (TyBin TyFunc c d) = unCollide f g'
    sm <- unify b c
    return $ niceTyVars (TyBin TyFunc (subst sm a) (subst sm d))
inferComposition _ _ = error "Invalid composition inference"

-- | Main type inference engine
infer env (TComp _ f g) = join (inferComposition <$> infer env f <*> infer env g)
infer env (TQuot _ f)   = inferLiteral <$> (infer env f)
infer env (TFunc _ f)   = maybe (throwError $ SESymbol f) return (M.lookup f env)
infer env term          = inferLiteral <$> inferVal env term

inferVal env f@(TQuot _ _)   = infer env f
inferVal env f@(TFunc _ _)   = infer env f
inferVal env f@(TComp _ _ _) = infer env f
inferVal _ (TUnit _)    = return tUnit
inferVal _ (TInt  _ _)  = return tInt
inferVal _ (TChar _ _)  = return tChar
inferVal _ (TFloat _ _) = return tFloat
inferVal env (TList _ xs)  = tList <$> (mapM (inferVal env) xs >>= foldM typeUnify ta)
inferVal env (TPair _ a b) = unCollideT tProd <$> inferVal env a <*> inferVal env b
inferVal env (TSumA _ a)   = tSum <$> inferVal env a <*> pure (TyVar $ genTyVar ta)
inferVal env (TSumB _ b)   = tSum (TyVar $ genTyVar ta) <$> inferVal env b

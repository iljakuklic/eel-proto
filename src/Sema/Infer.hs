
module Sema.Infer (infer, inferVal) where

import Sema.Types
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

ta = TyVar (genTyVar tUnit)

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
unify (TyBin (TyFunc p) a b) (TyBin (TyFunc r) c d) = do
    s1 <- unify p r
    s2 <- unify (subst s1 a) (subst s1 c)
    let s12 = substComp s1 s2
    s3 <- unify (subst s12 b) (subst s12 d)
    return (substComp s12 s3)
unify (TyBin u a b) (TyBin v c d) | u == v = do
    s1 <- unify a c
    s2 <- unify (subst s1 b) (subst s1 d)
    return (substComp s1 s2)
unify t1 t2 = throwError (SEUnify t1 t2)

-- | Unification applied to types
typeUnify a b = fmap (flip subst a) (unify a b)

-- | Calculate resulting phase type expression
joinPhaseTypes (TyPhase p1) (TyPhase p2) = TyPhase <$> joinPhases p1 p2
joinPhaseTypes a@(TyVar _) b@(TyVar _) | a == b = return a
joinPhaseTypes a b = error $ "joinPhaseTypes: this should never happen: " ++ show a ++ " | " ++ show b

-- | Calculate phase of composition of functions with given phases
joinPhases p1 p2 | p1 == p2  = return p1
joinPhases TyCompile TyParse = return TyParse
joinPhases TyParse TyCompile = return TyParse
joinPhases _ _ = throwError SEPhase

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
inferLiteral t = niceTyVars (tFunc ph row pr)
    where row = TyVar (genTyVar t); pr = row `tProd` t; ph = TyVar (genTyVar pr)

-- | infer type of the function composition g(f(x)), written f g
inferComposition f@(TyBin (TyFunc ph1) a b) g@(TyBin (TyFunc _) _ _) = do
    let (TyBin (TyFunc ph2) c d) = unCollide f g
    sm <- unify (b `tSum` ph1) (c `tSum` ph2)
    ph <- joinPhaseTypes (subst sm ph1) (subst sm ph2)
    return $ niceTyVars (TyBin (TyFunc ph) (subst sm a) (subst sm d))
inferComposition _ _ = error "Invalid composition inference"

-- | Main type inference engine
infer = inferTerm

-- | Infer the type of a term
inferTerm env (TComp _ f g) = join (inferComposition <$> infer env f <*> infer env g)
inferTerm env (TQuot _ f)   = inferLiteral <$> (infer env f)
inferTerm env (TFunc _ f)   = maybe (throwError $ SESymbol f) return (M.lookup f env)
inferTerm env term          = inferLiteral <$> inferVal env term

-- | Infer the type of a stack value
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

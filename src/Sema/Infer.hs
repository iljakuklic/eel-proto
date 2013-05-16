
module Sema.Infer (infer, stackInfer, inferUnify) where

import Sema.Types
import Sema.Error
import Sema.Term
import Parser.State

import Prelude hiding (notElem, elem, maximum, concat)
import qualified Data.Map as M
import Control.Monad.Error
import Control.Applicative
import Data.Foldable
import Data.List hiding (notElem)

-- | Substitution type
type Substitution v = M.Map v (Type v)

ta = TyVar (genTyVar tUnit)
tb = TyVar (genTyVar ta)

-- | Type variable substitution
subst :: (Ord v) => Substitution v -> Type v -> Type v
subst sm ty = ty >>= (\n -> maybe (TyVar n) id (M.lookup n sm))

-- | Substitution composition
substComp s1 s2 = (M.union s2 (fmap (subst s2) s1))

-- | Type unification
--unify :: (Ord v, MonadError (TypeError v) m) => Type v -> Type v -> m (M.Map v (Type v))
unify u v | u == v = return M.empty
unify (TyPhase TyCompile) (TyPhase TyParse) = return M.empty
unify (TyPhase TyParse) (TyPhase TyCompile) = return M.empty
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

-- | infer unification substitution for the first type
inferUnify = unCollideT unify

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
infer = fst . inferTerm

-- | set type or corresponding error in the term metadata
setType' term err@(Left _) = (termModifyType (const $ HasType err) term, Left SEInherited)
setType' term (Right ty)   = setType'' term ty
-- | set type in the term metadata
setType'' term ty' = let ty = niceTyVars ty' in (termModifyType (const $ HasType (Right ty)) term, Right ty)

-- | Check if a type has been inferred already
hasType' (HasType _) = True
hasType' _           = False
-- | unsafe type getter
getType' (HasType t) = t
getType' NoType      = error "getType' NoType never happens"

-- | infer term type only if not done so already
inferTerm t | hasType' tyMeta = (t, getType' tyMeta)
    where tyMeta = mType . getMeta $ t
inferTerm (TComp m f g) = setType'  (TComp m f' g') (join (inferComposition <$> ft <*> gt))
    where (f', ft) = inferTerm f; (g', gt) = inferTerm g
inferTerm (TQuot m f) = setType'  (TQuot m f') (inferLiteral <$> ft)
    where (f', ft) = inferTerm f
inferTerm fun@(TFunc _ _ funDef) = setType'  fun (functionDefType funDef)
inferTerm term = setType'  term' (inferLiteral <$> ty)
    where (term', ty) = inferVal term

-- | infer value term type only if not done so already
inferVal t | hasType' tyMeta = (t, getType' tyMeta)
    where tyMeta = mType . getMeta $ t
inferVal   (TQuot _ t)    = inferTerm t
inferVal t@(TFunc _ _ _)  = inferTerm t
inferVal t@(TComp _ _ _)  = inferTerm t
inferVal t@(TUnit _)      = setType'' t tUnit
inferVal t@(TInt  _ _)    = setType'' t tInt
inferVal t@(TChar _ _)    = setType'' t tChar
inferVal t@(TFloat _ _)   = setType'' t tFloat
inferVal (TList m xs)     = setType'  (TList m xs') ty
    where (xs', ts') = unzip $ fmap (inferVal) xs
          ty = tList <$> (foldM ff ta ts')
          ff a b = join (typeUnify a <$> b)
inferVal (TPair m a b) = setType'  (TPair m a' b') (unCollideT tProd <$> at <*> bt)
    where (a', at) = inferVal a
          (b', bt) = inferVal b
inferVal (TSumA m a) = setType'  (TSumA m a') (unCollideT tSum <$> at <*> pure tb)
    where (a', at) = inferVal a
inferVal (TSumB m b) = setType'  (TSumB m b') (unCollideT tSum ta <$> bt)
    where (b', bt) = inferVal b

-- | Get the type of a stack
stackInfer ss@(Stack stk) = stkType'
  where
    stkTypes = mapM (getType' . mType . getMeta . fst . inferVal) stk
    stkType' = Data.List.foldr (flip (unCollideT tProd)) tUnit (either err id stkTypes)
    err = error ("Incorrect stack type!!!\n" ++ show ss)

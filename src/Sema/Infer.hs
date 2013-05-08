
module Sema.Infer (infer, coerce, coerceToInput, inferCoerce, stackInfer, appliesTo, inferValueForce, stackify, inferResultType) where

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
tc = TyVar (genTyVar tb)
anyFunc = tFunc tc ta tb

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

-- | get inferred type unsafely
getTypeI term = case mType (getMeta term) of
        NoType -> error "Type not inferred yet"
        HasType _ t -> t

-- | infer unification of a term with given type
inferUnify ty term = do
    sm <- unify ty (unCollide ty $ getTypeI term)
    return $ niceTyVars (subst sm ty)

-- | set type unsafely by unification
coerce' term ty = do
    t' <- inferUnify ty term
    return $ termModifyType (mod' t') term
  where mod' t _t = case mType (getMeta term) of NoType -> NoType; HasType t1 _ -> HasType t1 t

-- | infer type of a coercion of a term, assumes inferrence has been already performed
inferCoerce term ty = do
    term' <- coerce' term ty
    inferCoerce' term' (getTypeI term')
inferCoerce' (TComp m f g) tt@(TyBin (TyFunc ph) tf tg) = do
    let tx = TyVar $ genTyVar tt
    f' <- inferCoerce f (TyBin (TyFunc ph) tf tx)
    g' <- inferCoerce g (TyBin (TyFunc ph) tx tg)
    return (TComp m f' g')
inferCoerce' (TQuot m q) (TyBin (TyFunc _) _ (TyBin TyProd _ tq)) = do
    q' <- inferCoerce q tq
    return (TQuot m q')
inferCoerce' (TSumA m a) (TyBin TySum tta _) = do
    a' <- inferCoerce a tta
    return (TSumA m a')
inferCoerce' (TSumB m b) (TyBin TySum _ ttb) = do
    b' <- inferCoerce b ttb
    return (TSumB m b')
inferCoerce' (TPair m a b) (TyBin TyProd tta ttb) = do
    a' <- inferCoerce a tta
    b' <- inferCoerce b ttb
    return (TPair m a' b')
inferCoerce' (TList m xs) (TyList txs) = do
    xs' <- mapM (flip inferCoerce txs) xs
    return (TList m xs')
inferCoerce' t _ty = return t

-- | Main type inference engine
infer env = fst . inferTerm env
--infer env term = either err id (inferAndCoerce getTypeI env term)
--  where err e = error ("Inference recosntruction error: " ++ show e ++ "\nTerm: " ++ show term)

-- | infer type and force it to particular subtype
coerce ty env term = inferAndCoerce (const ty) env term

-- | infer and coerce based on previously inferred type
inferAndCoerce tyf env term = inferCoerce term' (tyf term')
  where term' = fst (inferTerm env term)

-- | coerce the function so that its input matches given type
coerceToInput env phaseT inpT term = inferAndCoerce (const ty) env term
    where
        tv = TyVar $ genTyVar inpT               -- fresh type var
        pv = TyVar $ genTyVar (inpT `tProd` tv)  -- fresh phase var
        -- type of a function consuming desired input
        ty = tFunc (maybe pv TyPhase phaseT) inpT tv

-- | set type or corresponding error in the term metadata
setType' dflt term err@(Left _) = (termModifyType (const $ HasType err dflt) term, Left SEInherited)
setType' _dfl term (Right ty)   = setType'' term ty
-- | set type in the term metadata
setType'' term ty' = let ty = niceTyVars ty' in (termModifyType (const $ HasType (Right ty) ty) term, Right ty)

-- | Check if a type has been inferred already
hasType' (HasType _ _) = True
hasType' _             = False
-- | unsafe type getter
getType' (HasType t _) = t
getType' NoType        = error "getType' NoType never happens"

-- | infer term type only if not done so already
inferTerm _env t | hasType' tyMeta = (t, getType' tyMeta)
    where tyMeta = mType . getMeta $ t
inferTerm env (TComp m f g) = setType' anyFunc (TComp m f' g') (join (inferComposition <$> ft <*> gt))
    where (f', ft) = inferTerm env f; (g', gt) = inferTerm env g
inferTerm env (TQuot m f) = setType' (inferLiteral anyFunc) (TQuot m f') (inferLiteral <$> ft)
    where (f', ft) = inferTerm env f
inferTerm _env fun@(TFunc _ _ funDef) = setType' anyFunc fun (functionDefType funDef)
inferTerm env term = setType' anyFunc term' (inferLiteral <$> ty)
    where (term', ty) = inferVal env term

-- | infer value term type only if not done so already
inferVal _env t | hasType' tyMeta = (t, getType' tyMeta)
    where tyMeta = mType . getMeta $ t
inferVal env   (TQuot _ t)    = inferTerm env t
inferVal env t@(TFunc _ _ _)  = inferTerm env t
inferVal env t@(TComp _ _ _)  = inferTerm env t
inferVal _   t@(TUnit _)      = setType'' t tUnit
inferVal _   t@(TInt  _ _)    = setType'' t tInt
inferVal _   t@(TChar _ _)    = setType'' t tChar
inferVal _   t@(TFloat _ _)   = setType'' t tFloat
inferVal env (TList m xs)     = setType' (tList ta) (TList m xs') ty
    where (xs', ts') = unzip $ fmap (inferVal env) xs
          ty = tList <$> (foldM ff ta ts')
          ff a b = join (typeUnify a <$> b)
inferVal env (TPair m a b) = setType' ta (TPair m a' b') (unCollideT tProd <$> at <*> bt)
    where (a', at) = inferVal env a
          (b', bt) = inferVal env b
inferVal env (TSumA m a) = setType' ta (TSumA m a') (unCollideT tSum <$> at <*> pure tb)
    where (a', at) = inferVal env a
inferVal env (TSumB m b) = setType' tb (TSumB m b') (unCollideT tSum ta <$> bt)
    where (b', bt) = inferVal env b

-- | Force value type re-inference
inferValueForce env t = fst . inferVal  env . mapMeta (%% mEps) $ t

-- | Get the type of a stack
stackInfer env ss@(Stack _t stk) = stkType' -- either err id stkType
  where
    stkTypes = mapM (getType' . mType . getMeta . fst . inferVal env) stk
    stkType' = Data.List.foldr (flip (unCollideT tProd)) tUnit (either err id stkTypes)
    err = error ("Incorrect stack type!!!\n" ++ show ss)

-- | Check if given function applies to the stack
appliesTo (TyBin (TyFunc _) fargs _) ty1 = unCollideT unify ty1 fargs
appliesTo t _ty1 = error ("Applying a non-function: " ++ show t)

-- | Adjust the type from term to value on the stack
stackify term = do
    env <- pTypeTable
    let term' = infer env term
    case mType (getMeta term') of
        NoType               -> error "Type not inferred... never happens"
        HasType (Right  t) _ | isValue term' ->
            case t of
                TyBin (TyFunc _ph) _ (TyBin TyProd _ ty) ->
                     return $ termModifyType (const $ HasType (Right ty) ty) term'
                _ -> return term'
        HasType (Left err) _ -> fail $ "Pushing an invalid term " ++ show term' ++ " / " ++ show err
        _                    -> return term'

-- | Get the type of the result stack after applying a function
inferResultType phase stkType funType = do
    let rVar = genTyVar stkType
    let rfType = tFunc phase stkType (TyVar rVar)
    let err = error "rVar not found, this shall never happen"
    sm <- unCollideT unify rfType funType
    return . maybe err id $ M.lookup rVar sm

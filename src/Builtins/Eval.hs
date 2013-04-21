
{-# LANGUAGE FlexibleInstances #-}

module Builtins.Eval (eval) where

import Sema.Term
import Sema.Common
import Sema.Infer
import Parser.State
import Backend.Eval
import Builtins.Conversions

import Data.Char

e = mEps

-- term evaluation
instance Evaluable (Term Meta) where
    eval (TFunc _ f) = lookupFunc f >>= eval
    eval (TComp _ f g) = eval f >> eval g
    eval q = push q

-- function definition
instance Evaluable (FunctionDef Meta) where
    eval (FDBuiltIn bi) = eval bi
    eval (FDUser term)  = eval term

-- builtin invokation
instance Evaluable BuiltIn where
    eval BIlet = error "Let not implemented"
    eval BIdef = do
        name <- pop; TQuot _ body' <- pop; env <- pTypeTable;
        let body = infer env body'
        case termType body of
            Left err -> fail ("ERROR while inferring type for '" ++ termToString name ++ "': " ++ show err)
            Right _  -> addFunc (Symbol $ termToString name) (FDUser body)
    eval BIfix = do
        funq@(TQuot m0 fun) <- pop
        let m = m0 %% m0
        push (TQuot m . TComp m funq . TFunc m $ Symbol "fix")
        eval fun
    eval BIdip = do TQuot _ f <- pop; x <- pop; eval f; push x
    eval BIsel = do
        TQuot _ fb <- pop; TQuot _ fa <- pop; x <- pop
        case x of
            (TSumA _ a) -> do push a; eval fa
            (TSumB _ b) -> do push b; eval fb
            _ -> error ("Sum type is not either Left or Right: " ++ show x)
    eval bi = evalPure (evalP bi)
      where
        -- Combinators
        evalP BIid  s       = s
        evalP BIid2 s       = s
        evalP BIzap (_:s)   = s
        evalP BIdup (x:s)   = x:x:s
        evalP BIqot (x:s)   = TQuot e x : s
        evalP BIcat (TQuot m1 g : TQuot m2 f : s) = let m = m1 %% m2 in TQuot m (TComp m f g) : s
        -- Unit
        evalP BIunit s      = TUnit e : s
        -- Products
        evalP BIpair (b:a:s) = TPair e a b : s
        evalP BIunpair (TPair _ a b : s) = b:a:s
        -- Sums
        evalP BIina (x:s) = TSumA (getMeta x %% e) x : s
        evalP BIinb (x:s) = TSumB (getMeta x %% e) x : s
        -- Lists
        evalP BIlistw (TSumB m (TUnit _)                   : s) = TList (m %% e)      []     : s
        evalP BIlistw (TSumA m3 (TPair m1 x (TList m2 xs)) : s) = TList (m1 %% m2 %% m3) (x:xs) : s
        evalP BIlistu (TList m' []     : s) = let m = m' %% e in TSumB m (TUnit m)                : s
        evalP BIlistu (TList m' (x:xs) : s) = let m = m' %% e in TSumA m (TPair m x (TList m xs)) : s
        -- Integers
        evalP BIadd s = int2op (+) s
        evalP BIsub s = int2op (-) s
        evalP BImul s = int2op (*) s
        evalP BIdiv s = int2op div s
        evalP BIcmp (TInt _m1 y : TInt _m2 x : s) = cmpop x y s
        -- Floats
        evalP BIfadd s = flt2op (+) s
        evalP BIfsub s = flt2op (-) s
        evalP BIfmul s = flt2op (*) s
        evalP BIfdiv s = flt2op (/) s
        evalP BIfcmp (TFloat _m1 y : TFloat _m2 x : s) = cmpop x y s
        evalP BIfsin s = flt1op sin s
        evalP BIfpow s = flt2op (**) s
        evalP BIflog s = flt1op log s
        -- Conversions
        evalP BIfloor (TFloat m' x : s) = let m = m' %% e in TInt   m (floor x)        : s
        evalP BIfloat (TInt   m' x : s) = let m = m' %% e in TFloat m (fromIntegral x) : s
        evalP BIord   (TChar  m' c : s) = let m = m' %% e in TInt   m (ord c)          : s
        evalP BIchar  (TInt   m' x : s) = let m = m' %% e in TChar  m (chr x)          : s
        -- error condition
        evalP f s = error ("Could not evaluate " ++ show f ++ " on " ++ show s)
        -- helpers
        int2op op (TInt   m1 y : TInt   m2 x : s) = TInt   (m1 %% m2) (x `op` y) : s
        int2op _op _                              = error "Invalid integer binary operation"
        flt2op op (TFloat m1 y : TFloat m2 x : s) = TFloat (m1 %% m2) (x `op` y) : s
        flt2op _op _                              = error "Invalid floating-point binary operation"
        flt1op op (TFloat m1 x : s)               = TFloat (m1 %% m1) (op x)     : s
        flt1op _op _                              = error "Invalid floating-point unary operation"
        cmpop x y s = res : s
            where res = case compare x y of
                    EQ -> TSumB e (TUnit e)
                    LT -> TSumA e (TSumA e (TUnit e))
                    GT -> TSumA e (TSumB e (TUnit e))


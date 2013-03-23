
module Builtins.Eval (eval) where

import Sema.Term
import Parser.State
import Backend.Eval

import Data.Char

-- term evaluation
instance Evaluable (Term m) where
    eval (TFunc _ f) = eval f
    eval (TComp _ f g) = eval f >> eval g
    eval q = push (fmap (const ()) q)

-- function invokation
instance Evaluable (Function) where
    eval (FUser fn)   = lookupFunc fn >>= eval
    eval (FBuiltIn b) = eval b

-- builtin invokation
instance Evaluable BuiltIn where
    eval BIfix = error "Fixpoint evaluation not implemented"
    eval BIdip = do f <- pop; x <- pop; eval f; push x
    eval BIsel = do
        f0 <- pop; f1 <- pop; x <- pop
        case x of
            (TLeft  _ x') -> do push x'; eval f0
            (TRight _ x') -> do push x'; eval f1
            _ -> error ("Sum type is not either Left or Right: " ++ show x)
    eval bi = evalPure (evalP bi)
      where
        -- Combinators
        evalP BIid  s       = s
        evalP BIid2 s       = s
        evalP BIzap (_:s)   = s
        evalP BIdup (x:s)   = x:x:s
        evalP BIqot (x:s)   = TQuot () x : s
        evalP BIcat (g:f:s) = TComp () f g : s
        -- Products
        evalP BIpair (b:a:s) = TPair () a b : s
        evalP BIunpair (TPair () a b : s) = b:a:s
        -- Sums
        evalP BIlft (x:s) = TLeft  () x : s
        evalP BIrgt (x:s) = TRight () x : s
        -- Lists
        evalP BIlistw (TLeft  () (TUnit ())                 : s) = TList () []     : s
        evalP BIlistw (TRight () (TPair () x (TList () xs)) : s) = TList () (x:xs) : s
        evalP BIlistu (TList () []     : s) = TLeft  () (TUnit ())                 : s
        evalP BIlistu (TList () (x:xs) : s) = TRight () (TPair () x (TList () xs)) : s
        -- Integers
        evalP BIadd s = int2op (+) s
        evalP BIsub s = int2op (-) s
        evalP BImul s = int2op (*) s
        evalP BIdiv s = int2op div s
        evalP BIcmp (TInt () y : TInt () x : s) = cmpop x y s
        -- Floats
        evalP BIfadd s = flt2op (+) s
        evalP BIfsub s = flt2op (-) s
        evalP BIfmul s = flt2op (*) s
        evalP BIfdiv s = flt2op (/) s
        evalP BIfcmp (TFloat () y : TFloat () x : s) = cmpop x y s
        evalP BIfsin s = flt1op sin s
        evalP BIfpow s = flt2op (**) s
        evalP BIflog s = flt1op log s
        -- Conversions
        evalP BIfloor (TFloat () x : s) = TInt   () (floor x)        : s
        evalP BIfloat (TInt   () x : s) = TFloat () (fromIntegral x) : s
        evalP BIord   (TChar  () c : s) = TInt   () (ord c)          : s
        evalP BIchar  (TInt   () x : s) = TChar  () (chr x)          : s
        -- error condition
        evalP f s = error ("Could not evaluate " ++ show f ++ " on " ++ show s)
        -- helpers
        int2op op (TInt   () y : TInt   () x : s) = TInt   () (x `op` y) : s
        flt2op op (TFloat () y : TFloat () x : s) = TFloat () (x `op` y) : s
        flt1op op (TFloat () x : s)               = TFloat () (op x)     : s
        cmpop x y s = res : s
            where res = case compare x y of
                    EQ -> TRight () (TUnit ())
                    LT -> TLeft  () (TRight () (TUnit ()))
                    GT -> TLeft  () (TLeft  () (TUnit ()))


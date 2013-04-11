
module Builtins.Eval (eval) where

import Sema.Term
import Sema.Common
import Sema.Infer
import Parser.State
import Backend.Eval
import Builtins.Conversions

import Data.Char

-- term evaluation
instance Evaluable (Term m) where
    eval (TFunc _ f) = lookupFunc f >>= eval
    eval (TComp _ f g) = eval f >> eval g
    eval q = push (fmap (const ()) q)

-- function definition
instance Evaluable (FunctionDef m) where
    eval (FDBuiltIn  bi) = eval bi
    eval (FDUser _ term) = eval term

-- builtin invokation
instance Evaluable BuiltIn where
    eval BIlet = error "Let not implemented"
    eval BIdef = do
        name <- pop; TQuot _ body <- pop; env <- pTypeTable;
        case infer env body of
            Left err -> fail ("ERROR while inferring type for '" ++ termToString name ++ "': " ++ show err)
            Right ftype -> addFunc (Symbol $ termToString name) (FDUser (ftype) body)
    eval BIfix = do
        funq@(TQuot _ fun) <- pop
        push (TQuot () . TComp () funq . TFunc () $ Symbol "fix")
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
        evalP BIqot (x:s)   = TQuot () x : s
        evalP BIcat (TQuot _ g : TQuot _ f : s) = TQuot () (TComp () f g) : s
        -- Unit
        evalP BIunit s      = TUnit () : s
        -- Products
        evalP BIpair (b:a:s) = TPair () a b : s
        evalP BIunpair (TPair () a b : s) = b:a:s
        -- Sums
        evalP BIina (x:s) = TSumA () x : s
        evalP BIinb (x:s) = TSumB () x : s
        -- Lists
        evalP BIlistw (TSumB () (TUnit ())                 : s) = TList () []     : s
        evalP BIlistw (TSumA () (TPair () x (TList () xs)) : s) = TList () (x:xs) : s
        evalP BIlistu (TList () []     : s) = TSumB () (TUnit ())                 : s
        evalP BIlistu (TList () (x:xs) : s) = TSumA () (TPair () x (TList () xs)) : s
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
        int2op _op _                              = undefined
        flt2op op (TFloat () y : TFloat () x : s) = TFloat () (x `op` y) : s
        flt2op _op _                              = undefined
        flt1op op (TFloat () x : s)               = TFloat () (op x)     : s
        flt1op _op _                              = undefined
        cmpop x y s = res : s
            where res = case compare x y of
                    EQ -> TSumB () (TUnit ())
                    LT -> TSumA () (TSumA () (TUnit ()))
                    GT -> TSumA () (TSumB () (TUnit ()))


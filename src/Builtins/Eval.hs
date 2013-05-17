{-
    EEL -- Extensible Experimental Language
    by Lukáš Kuklínek, 2013
-}

module Builtins.Eval (eval, invoke, addRule, builtInsTable) where

import Sema.Term
import Sema.Infer
import Parser.State
import Parser.Eval
import Parser.Rule
import Control.Applicative
import Builtins.Table

import Data.Char
import qualified Data.Map as M
import Text.Parsec(try, anyChar, modifyState, Stream, Parsec)
import Data.Functor.Identity

-- empty metadata helper
e = mEps

-- | Invoke a parser
invoke nt = lookupParser nt >>= id

-- | Add a parsing rule
addRule :: (Stream s Identity Char) => Symbol -> Int -> Term Meta -> Parsec s (PState s c Meta) ()
addRule nt@(Symbol name) pri rhs = modifyState (\ste -> ste { pRules = updateRT (pRules ste) } )
  where
    updateRT oldRules = M.insert nt (RuleCache newDefs newParser) oldRules
      where
        err1 = error "old parser referenced"
        RuleCache oldDefs _ = M.findWithDefault (RuleCache M.empty err1) nt oldRules
        newDefs = M.insertWith (++) pri [rhs] oldDefs
        newParser = generateNamedParser name eval (map snd $ M.toDescList newDefs)

-- | convert a term to the string
termToString :: Monad m => Term me -> m String
termToString (TList _ xs) = mapM termToChar xs
  where
    termToChar (TChar _ x) = return x
    termToChar t           = fail ("Not a character: " ++ show t)
termToString x = fail (show x ++ " is not a string")

-- | Term evaluation
instance Evaluable (Term Meta) where
    eval (TFunc _ _ f) = eval f
    eval (TComp _ f g) = eval f >> eval g
    eval x = push x

-- | Function definition evaluation
instance Evaluable (FunctionDef Meta) where
    eval (FDBuiltIn bi) = eval bi
    eval (FDUser term)  = do
        case termType term of
            Right _ -> eval term
            Left _  -> fail "Calling a maltyped function"

-- | Builtin evaluation
instance Evaluable BuiltIn where

    -- fixed point combinator
    eval BIfix = do
        funq@(TQuot m0 fun) <- pop
        let m = m0 %% m0
        push (TQuot m . TComp m funq $ TFunc m (Symbol "fix") (FDBuiltIn BIfix))
        eval fun

    -- evaluate quotation one element below the stack top
    eval BIdip = do
        TQuot _ f <- pop
        x <- pop
        eval f
        push x

    -- select (sum type deconstructor)
    eval BIsel = do
        TQuot _ fb <- pop
        TQuot _ fa <- pop
        x <- pop
        case x of
            (TSumA _ a) -> do push a; eval fa
            (TSumB _ b) -> do push b; eval fb
            _ -> fail ("Type error: sel applied to " ++ show x)

    -- let binding
    --eval BIlet     = fail "Let not implemented"

    -- function definition
    eval BIdef = do
        name <- pop >>= termToString
        TQuot _ body <- pop
        addFunc (Symbol name) (FDUser $ infer body)

    -- function lookup from a string
    eval BIpromote = do
        name <- Symbol <$> (pop >>= termToString)
        term <- lookupFunc name
        push (TQuot mEps $ TFunc mEps name term)

    -- define a grammar rule
    eval BIdefrule = do
        TInt _ prio <- pop
        name <- pop >>= termToString
        TQuot _ body' <- pop
        let body = infer body'
        case termType body of
            Left err -> fail ("ERROR while inferring type for nonterminal '" ++ name ++ "': " ++ show err)
            Right _  -> addRule (Symbol name) prio body

    -- grammar nonterminal parsing invokation
    eval BIinvoke  = pop >>= termToString >>= (invoke . Symbol)

    -- primitive parser for single character
    eval BIppchar = do
        TQuot _ fq <- pop
        try $ do
            ch <- anyChar
            push (TChar mEps ch)
            eval fq
            result <- pop
            case result of
                TSumA _ _ -> return ()
                _ -> fail ("Unexpected character " ++ show ch)

    -- failing primitive parser
    eval BIppfail = pop >>= termToString >>= fail . ("User error: " ++)

    -- whitespace skipper from the core
    eval BIppcoreskip = (fst . pPrimRules <$> getState) >>= id
    -- term parser from the core
    eval BIppcoreterm = (snd . pPrimRules <$> getState) >>= id

    -- evaluate a function without side effects (parsing, function definitions etc.)
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
        evalP f s = fail ("Could not evaluate " ++ show f ++ " on stack " ++ show s ++ ": type mismatch")
        -- helpers
        int2op op (TInt   m1 y : TInt   m2 x : s) = TInt   (m1 %% m2) (x `op` y) : s
        int2op _op _                              = fail "Invalid integer binary operation"
        flt2op op (TFloat m1 y : TFloat m2 x : s) = TFloat (m1 %% m2) (x `op` y) : s
        flt2op _op _                              = fail "Invalid floating-point binary operation"
        flt1op op (TFloat m1 x : s)               = TFloat (m1 %% m1) (op x)     : s
        flt1op _op _                              = fail "Invalid floating-point unary operation"
        cmpop x y s = res : s
            where res = case compare x y of
                    EQ -> TSumB e (TUnit e)
                    LT -> TSumA e (TSumA e (TUnit e))
                    GT -> TSumA e (TSumB e (TUnit e))


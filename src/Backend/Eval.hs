
module Backend.Eval (
        Evaluable, eval,
        evalPure, push, pop, addRule, addFunc,
        stackType, checkAppliable
    ) where

import Sema.Term
import Sema.Types
import Parser.State
import Sema.Infer

import Text.Parsec
import Control.Applicative
import qualified Data.Map as M

-- | Perser-evaluable expression-like structures
class Evaluable expr where
    -- | Evaluate given expression in Parsec monad
    eval :: Monad m => expr -> ParsecT s (PState Meta) m ()

-- | Evaluate a pure function (mapping a stact to a stack) in Parsec monad
evalPure f = modifyState (\ste -> ste { pStack = onStack f (pStack ste) } )

-- | Push value onto the stack
push x = evalPure (\stk -> x : stk)
-- | Pop value from the stack
pop    = do
    Stack s1 <- pStack <$> getState
    evalPure (\(_:stk) -> stk)
    return (head s1)

-- | Add a rule to the rule table
addRule nt pri rhs = modifyState (\ste -> ste { pRules = updateRT (pRules ste) } )
    where updateRT = M.insertWith (M.unionWith (++)) nt (M.singleton pri [rhs])

-- | Get the type of current stack
stackType = do
    stk <- pStack <$> getState
    env <- pTypeTable
    return $ stackInfer env stk

-- | Check if a term (a function) is appliabe to current stack
checkAppliable term = do
    st <- stackType
    env <- pTypeTable
    case coerceToInput env (Just TyCompile) st term of
        Right term' -> return term'
        Left err -> fail (show err)

-- | Add a function to the symbol table
addFunc name term = do
    ste <- getState
    let st = pSymTable ste
    case (M.lookup name st) of
        Just _ -> fail ("Symbol already defined: '" ++ show name ++ "'")
        Nothing -> setState (ste { pSymTable = M.insert name term st })

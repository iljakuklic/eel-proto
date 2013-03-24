
module Backend.Eval (
        Evaluable, eval,
        evalPure, push, pop, addRule, addFunc
    ) where

import Sema.Term
import Parser.State

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

-- | Add a function to the symbol table
addFunc name term = do
    ste <- getState
    let st = pSymTable ste
    case (M.lookup name st) of
        Just _ -> fail ("Symbol already defined: '" ++ show name ++ "'")
        Nothing -> setState (ste { pSymTable = M.insert name term st })

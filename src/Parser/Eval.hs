{-
    EEL -- Extensible Experimental Language
    by Lukáš Kuklínek, 2013
-}

module Parser.Eval (
        -- * Term evaluation
        Evaluable, eval, evalPure, push, pop, addFunc,
        -- * Type manipulation
        stackType
    ) where

import Sema.Term
import Sema.Infer
import Parser.State

import Text.Parsec
import Control.Applicative
import Data.Functor.Identity
import qualified Data.Map as M

-- | Perser-evaluable expression-like structures
class Evaluable expr where
    -- | Evaluate given expression in Parsec monad
    eval :: (Stream s Identity Char) => expr -> Parsec s (PState s c Meta) ()

-- | Evaluate a pure function (mapping a stact to a stack) in Parsec monad
evalPure f = modifyState (\ste -> ste { pStack = onStack f (pStack ste) } )

-- | Push value onto the stack
push x = evalPure (\stk -> x : stk)
-- | Pop value from the stack
pop    = do
    Stack (top:rest) <- pStack <$> getState
    evalPure (const rest)
    return top

-- | Get the type of current stack
stackType = stackInfer . pStack <$> getState

-- | Add a function to the symbol table
addFunc name term = do
    ste <- getState
    let st = pSymTable ste
    case (M.lookup name st) of
        Just _ -> fail ("Symbol already defined: '" ++ show name ++ "'")
        Nothing -> setState (ste { pSymTable = M.insert name term st })


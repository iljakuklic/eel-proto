
module Parser.Eval (
        -- * Term evaluation
        Evaluable, eval, evalPure, push, pop, addFunc,
        -- * Parsing engine manipulation
        addRule, invokeUsing,
        -- * Type manipulation
        stackType, checkAppliable
    ) where

import Sema.Term
import Sema.Types
import Sema.Infer
import Parser.State
import Parser.Rule

import Text.Parsec
import Control.Applicative
import qualified Data.Map as M

-- | Perser-evaluable expression-like structures
class Evaluable expr where
    -- | Evaluate given expression in Parsec monad
    eval :: (Stream s m Char, Monad m) => expr -> ParsecT s (PState Meta) m ()

-- | Evaluate a pure function (mapping a stact to a stack) in Parsec monad
evalPure f = modifyState (\ste -> ste { pStack = onStack f (pStack ste) } )

-- | Push value onto the stack
push x = evalPure (\stk -> x : stk)
-- | Pop value from the stack
pop    = do
    Stack (top:rest) <- pStack <$> getState
    evalPure (const rest)
    return top

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

-- | Invoke grammar rule given by the symbol of the corresponding non-terminal
invokeUsing :: Stream s m t
            => (Production Meta -> ParsecT s (PState Meta) m b)
                                            -- ^ monadic evaluator to process the term
            -> Symbol                       -- ^ non-terminal to parse
            -> ParsecT s (PState Meta) m b  -- ^ resulting parsing monad
invokeUsing evaluator sym = do
    rulz <- pRules <$> getState
    parserForRule evaluator rulz sym

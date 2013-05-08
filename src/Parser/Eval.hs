
module Parser.Eval (
        -- * Term evaluation
        Evaluable, eval, evalPure, push, pop, addFunc,
        -- * Type manipulation
        stackType, stackApplyType, checkAppliable
    ) where

import Sema.Term
import Sema.Types
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
    Stack _t (top:rest) <- pStack <$> getState
    evalPure (const rest)
    return top

-- | Get the type of current stack
stackType = do
    stk <- pStack <$> getState
    env <- pTypeTable
    return $ stackInfer env stk

-- | Compute the new type of the stack
stackApplyType fty = do
    ste <- getState
    let Stack sty stk = pStack ste
    case inferResultType TyParse sty fty of
        Right rty' -> setState (ste { pStack = Stack rty' stk })
        Left err -> fail ("Stack type mismatch: " ++ show sty ++ " vs. " ++ show fty ++ ": " ++ show err)

-- | Check if a function is appliabe to the current stack
checkAppliable def = do
    st <- stackType
    case functionDefType def of
        Left _ -> fail ("Applying a bogus function, aborting.")
        Right ft ->
            case appliesTo ft st of
                Right _ -> return ()
                Left err -> fail (show err ++ " in " ++ show def ++
                     "\nCannot apply function type " ++ show ft ++ " to stack type " ++ show st)

-- | Add a function to the symbol table
addFunc name term = do
    ste <- getState
    let st = pSymTable ste
    case (M.lookup name st) of
        Just _ -> fail ("Symbol already defined: '" ++ show name ++ "'")
        Nothing -> setState (ste { pSymTable = M.insert name term st })


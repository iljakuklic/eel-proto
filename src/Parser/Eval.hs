
module Parser.Eval (
        -- * Term evaluation
        Evaluable, eval, evalPure, push, pop, addFunc,
        -- * Type manipulation
        stackType, checkAppliable
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
push x = do
    x' <- stackify x
    evalPure (\stk -> x' : stk)
-- | Pop value from the stack
pop    = do
    Stack (top:rest) <- pStack <$> getState
    evalPure (const rest)
    return top

-- | hack to map type from term to stack type
stackify = return
{-
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
-}

-- | Get the type of current stack
stackType = do
    stk <- pStack <$> getState
    env <- pTypeTable
    return $ stackInfer env stk

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


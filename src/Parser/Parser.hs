{-
    EEL -- Extensible Experimental Language
    by Lukáš Kuklínek, 2013
-}

module Parser.Parser(runEel, runEelExt, initState, initStack, emitModule, semaCheck) where

import Sema.Term
import Sema.Error
import Sema.Infer
import Parser.State
import Parser.Core
import Parser.Dump
import Builtins.Eval
import Builtins.Types
import Backend.Emit

import qualified Data.Map as M
import qualified Text.Parsec as P
import qualified System.FilePath as Path

-- | initial symbol table filled w/ built-ins
initSymTab = builtInsTable
-- | Initial stack is empty
initStack = Stack []

-- | EEL parser initial state bootstrap
initState = case P.runParser addRuleP initState' "" "" of
        Left _    -> error "Parser bootstrap failed"
        Right ste -> ste
  where
    -- add rule ".eel" to parsec state and return the new state
    addRuleP   = addRule (Symbol ".eel") 1 eelRule >> getState
    -- ".eel" rule definition --> skip whitespace, eel core call
    eelRule    = TComp mEps (biCall BIppcoreskip) (biCall BIppcoreterm)
    -- builtin call helper
    biCall     = TFunc mEps (Symbol "") . FDBuiltIn
    -- initial state without the ".eel" bootstrap
    initState' = PState {
            pSymTable = initSymTab,
            pRules = M.empty,
            pStack = initStack,
            pPrimRules = (coreSkipParser, coreParser)
        }

-- | Run parser with supplied initial state, extract semantic errors
runEel name ste nt = P.runParser eelp ste name
  where eelp = invoke nt >> P.eof >> getState

-- | Run EEL parser guessing the starting nonterminal from the file extension
runEelExt name ste = runEel name ste (Symbol $ getNT name)

-- | Get non-terminal name from the file extension
getNT name = case Path.takeExtensions name of "" -> ".eel"; ext -> ext

-- | Check the final state for semantic errors, main presence etc.
--   and return either errors or a list of functions to compile
semaCheck mainNme ste = case checkTypes ste ++ checkMain ste mainNme of
    [] -> Right (pSymTable ste)
    xs -> Left $ ErrorSet xs

-- | check the main function for presence and type
checkMain _ste Nothing = []
checkMain ste (Just nme) = case M.lookup (Symbol nme) (pSymTable ste) of
    Nothing -> mkErr SEMainMissing
    Just (FDUser term) -> case termType term of
        Left _er -> []
        Right ty -> either (mkErr . SEMain) (const []) (inferUnify ty mainType)
    _ -> error "checkMain: fatal error"
  where mkErr = return . TracedError (Symbol nme) []

-- | Check type metadata annotations
checkTypes ste = concatMap fproc funcs
  where
    funcs = [ (sym, term) | (sym, FDUser term) <- M.toList (pSymTable ste) ]
    fproc (sym, term) = [ TracedError sym tr err | (tr, err) <- withTrace [] tracer term ]
    tracer tr term = case termType term of
        Left err | nonTrivialError err -> [(tr, err)]
        _ -> []

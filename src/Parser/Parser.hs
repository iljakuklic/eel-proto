
module Parser.Parser(runEel, runEelExt, initState, initStack) where

import Sema.Term
import Parser.State
import Parser.Core
import Builtins.Eval

import qualified Data.Map as M
import qualified Text.Parsec as P
import qualified System.FilePath as Path

-- initial symbol table filled w/ built-ins
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

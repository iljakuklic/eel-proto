
import Builtins.Eval
import Sema.Term
import Parser.State
import Sema.Common
import Data.Char

import Text.Parsec hiding (many, (<|>))
import Control.Applicative
import qualified Data.Map as M

initSymTab :: SymTable m
initSymTab = M.fromList [ (builtInName bi, FDBuiltIn bi) | bi <- allBuiltIns ]

initState = PState { pSymTable = initSymTab, pRules = M.empty, pStack = [] }

ptok p = p <* spaces
pstok = ptok . string
pterm =  TComp () <$> pfunc <*> pterm
     <|> pure (TFunc () (FCBuiltIn BIid))
pfunc = TFunc () <$> (ptok psymb >>= lookupFuncCall)
     <|> TQuot () <$ pstok "[" *> pterm <* pstok "]"
psymb = Symbol <$> ((:) <$> satisfy isAlpha <*> many alphaNum)
peval = (pfunc >>= eval) >> peval <|> return ()
ptop  = spaces >> peval >> eof >> getState



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

initState = PState { pSymTable = initSymTab, pRules = M.empty, pStack = Stack [] }

ptok p = p <* spaces
pstok = ptok . string
pterm =  TComp () <$> pfunc <*> pterm
     <|> pure (TFunc () (FCBuiltIn BIid))
pfunc = TFunc () <$> (ptok psymb >>= lookupFuncCall)
     <|> TQuot () <$ pstok "[" *> pterm <* pstok "]"
     <|> TInt () . read <$> ptok (many1 digit)
psymb = Symbol <$> ((:) <$> satisfy isAlpha <*> many alphaNum)
peval = () <$ many (pfunc >>= eval)
ptop  = spaces >> peval >> eof >> getState

doparse = runParser ptop initState "<here>"


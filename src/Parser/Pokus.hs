
module Parser.Pokus where

import Builtins.Eval
import Sema.Term
import Parser.State
import Sema.Common
import Builtins.Builtins()
import Builtins.Table

import Data.Char
import Text.Parsec hiding (many, (<|>))
import Control.Applicative
import qualified Data.Map as M

initSymTab :: SymTable m
initSymTab = builtInsTable

initState = PState { pSymTable = initSymTab, pRules = M.empty, pStack = Stack [] }

ptok p = p <* skip
pstok = ptok . string
psbet a b = between (pstok a) (pstok b)
pterm =  TComp () <$> pfunc <*> pterm
     <|> pure (TFunc () (FCBuiltIn BIid))
pfunc =  TFunc () <$> (ptok psymb >>= lookupFuncCall)
     <|> TQuot () <$> psbet "[" "]" pterm
     <|> TInt () . read <$> ptok (many1 digit)
     <|> psbet "'" "'" pstr
pstr  = TList () <$> many pchr
pchr  = TChar () <$> satisfy (\ch -> isPrint ch && ch /= '\'')  -- TODO escape seqs
psymb = Symbol <$> ((:) <$> satisfy isAlpha <*> many alphaNum)
peval = () <$ many (pfunc >>= eval)
ptop  = skip >> peval >> eof >> getState
skip  = many ((space >> return ()) <|> (try (string "//") >> (anyChar `manyTill` eol) >> return ()))
eol   = (char '\n' >> return ()) <|> eof


testparse str =
    case runParser ptop initState "<here>" str of
        Left err  -> putStrLn (show err)
        Right ste -> printFuncs ste >> putStrLn "-----------------" >> printStack ste

printFuncs (PState st _ _) = mapM_ putStrLn [ show n ++ " = " ++ show d | (n, FDUser d) <- M.toList st]
printStack (PState _ _ (Stack stk)) = mapM_ (putStrLn . show) stk


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

ptok p = p <* spaces
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
ptop  = spaces >> peval >> eof >> getState


testparse str =
    case runParser ptop initState "<here>" str of
        Left err  -> putStrLn (show err)
        Right ste -> printState ste

printState (PState st _rul (Stack stk)) = do
    putStrLn "Funcs:"
    mapM_ putStrLn [ show n ++ " = " ++ show d | (n, FDUser d) <- M.toList st]
    putStrLn "Stack:"
    mapM_ (putStrLn . show) (reverse stk)


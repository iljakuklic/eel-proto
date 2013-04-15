
module Parser.Pokus where

import Builtins.Eval
import Sema.Term
import Parser.State
import Sema.Common
import Builtins.Builtins()
import Builtins.Types
import Builtins.Table

import Data.Char
import Text.Printf
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
     <|> pure (TFunc () (Symbol "id"))
pfunc =  TInt () . read <$> ptok (many1 digit)
     <|> TFunc () <$> (ptok psymb >>= (\s -> s <$ lookupFunc s))
     <|> TQuot () <$> psbet "[" "]" pterm
     <|> psbet "'" "'" pstr
pstr  = TList () <$> many pchr
pchr  = TChar () <$> satisfy (\ch -> isPrint ch && ch /= '\'')  -- TODO escape seqs
psymb = Symbol <$> many1 (satisfy (\c -> isAlpha c || c `elem` "0123456789+-*/.:&^%$#@!<>="))
peval = () <$ many (pfunc >>= eval)
ptop  = skip >> peval >> eof >> getState
skip  = many ((space >> return ()) <|> (try (string "//") >> (anyChar `manyTill` eol) >> return ()))
eol   = (char '\n' >> return ()) <|> eof


testparse str =
    case runParser ptop initState "<here>" str of
        Left err  -> putStrLn (show err)
        Right ste -> printFuncs ste >> putStrLn "-----------------" >> printStack ste

printFuncs :: PState Meta -> IO ()
printFuncs (PState st _ _) = sequence_ [ printFunc n f | (n, f@(FDUser _ _)) <- M.toList st]
printFunc  n (FDUser  t d) = printFunc' n t (show d)
printFunc  n (FDBuiltIn b) = printFunc' n (builtInType b) "<built-in>"
printFunc' n t d = printf "%-10s : %-50s = %s\n" (show n) (show t) d
printStack (PState _ _ (Stack stk)) = mapM_ (putStrLn . show) stk

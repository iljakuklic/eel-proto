
module Parser.Pokus where

import Builtins.Eval
import Sema.Term
import Parser.State
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

initStack = Stack []
initState = PState { pSymTable = initSymTab, pRules = M.empty, pStack = initStack }

e = mEps

ptok p = p <* skip
pstok = ptok . string
psbet a b = between (pstok a) (pstok b)
pterm = ploc (TComp e <$> pfunc <*> pterm
     <|> pure (TFunc e (Symbol "id") (FDBuiltIn BIid)))
pfunc = ploc (TInt e . read <$> ptok (many1 digit)
     <|> pfuncall
     <|> TQuot e <$> psbet "[" "]" pterm
     <|> psbet "'" "'" pstr)
pfuncall = do sym <- ptok psymb; term <- lookupFunc sym; return (TFunc e sym term)
pstr  = ploc (TList e <$> many pchr)
pchr  = TChar e <$> satisfy (\ch -> isPrint ch && ch /= '\'')  -- TODO escape seqs
psymb = Symbol <$> many1 (satisfy (\c -> isAlpha c || c `elem` "0123456789+-*/.:&^%$#@!<>="))
peval = () <$ many ((pfunc >>= eval) <|> pext)
ptop  = skip >> (peval) >> eof >> getState
pext  = string "~~~" >> invoke (Symbol "ext") >> return ()
skip  = many ((space >> return ()) <|> (try (string "//") >> (anyChar `manyTill` eol) >> return ()))
eol   = (char '\n' >> return ()) <|> eof
ploc innerP = do
    pstart <- plocation
    term <- innerP
    pend <- plocation
    return . termSetPos term $ mergePos pstart pend

plocation = conv <$> getPosition
    where conv pos = let p = (sourceLine pos, sourceColumn pos) in HasPos (sourceName pos) p p

testparse str =
    case runParser ptop initState "<here>" str of
        Left err  -> putStrLn (show err)
        Right ste -> printFuncs ste >> putStrLn "-----------------" >> printStack ste

printFuncs :: PState s c Meta -> IO ()
printFuncs (PState st _ _) = sequence_ [ printFunc n f | (n, f) <- M.toList st]
printFunc  n (FDUser    d) = printFunc' n (either show show $ termType d) (show d)
printFunc  n (FDBuiltIn b) = printFunc' n (show $ builtInType b) "<<built-in>>"
printFunc' n t d = printf "%-10s : %-50s = %s\n" (show n) t d
printStack (PState _ _ (Stack stk)) = mapM_ (putStrLn . show) stk

{-
    EEL -- Extensible Experimental Language
    by Lukáš Kuklínek, 2013
-}

-- | Module containing core parser grammar

module Parser.Core(coreParser, coreTopParser, coreTermParser, coreSkipParser) where

import Builtins.Eval
import Sema.Term
import Parser.State

import Data.Char
import Text.Parsec hiding (many, (<|>))
import Control.Applicative

e = mEps

-- token
ptok p = p <* skip
-- string token
pstok = ptok . string
-- parse input between two string tokens
psbet a b = between (pstok a) (pstok b)
-- term parser
pterm = ploc (TComp e <$> pfunc <*> pterm
     <|> pure (TFunc e (Symbol "id") (FDBuiltIn BIid)))
-- single function parser
pfunc = ploc (TInt e . read <$> ptok (many1 digit)
     <|> pfuncall
     <|> TQuot e <$> psbet "[" "]" pterm
     <|> pstr)
-- function call parser
pfuncall = do sym <- ptok psymb; term <- lookupFunc sym; return (TFunc e sym term)
-- core string parser
pstr  = ptok (between (char '"') (char '"') $ ploc (TList e <$> many pchr))
-- char parser
pchr  = TChar e <$> (satisfy (\ch -> isPrint ch && ch `notElem` "\"\\") <|> escSeq)
escSeq = char '\\' *> decodeEsc
decodeEsc = choice [ c <$ char d | (c, d) <- escapes ] <|> (chr . read) <$> many1 digit
    where escapes = [('\t', 't'), ('\n', 'n'), ('\r', 'r'), ('\\', '\\'), ('"', '"'), ('\'', '\'')]
-- symbol parser
psymb = Symbol <$> many1 (satisfy (\c -> isAlphaNum c || c == '_'))
-- term evaluating parser
peval = () <$ many (pfunc >>= eval)
-- top-level parser (skips whitespace at the begining, expects end of input after parse)
ptop  = skip >> peval >> eof >> getState
-- skipping parser (consumes whitespace and comments)
skip  = many ((space >> return ()) <|> (char '#' >> (anyChar `manyTill` eol) >> return ())) >> return ()
-- end of line or end of input
eol   = (char '\n' >> return ()) <|> eof
-- decorate the parser result with source location annotation
ploc innerP = do
    pstart <- plocation
    term <- innerP
    pend <- plocation
    return . termSetPos term $ mergePos pstart pend

-- get location
plocation = conv <$> getPosition
    where conv pos = let p = (sourceLine pos, sourceColumn pos) in HasPos (sourceName pos) p p

-- | Core parser
coreParser = peval

-- | Core parser for top-level invokation (parses the whole input or fails)
coreTopParser = ptop

-- | Core parser returning a term
coreTermParser = pterm

-- | Core parser for skipping white
coreSkipParser = skip

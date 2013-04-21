

module REPL(repl) where

import Parser.Pokus
import Parser.State
--import Parser.Dump
import Sema.Term(Stack(Stack))
import Sema.Infer

import System.IO
import Control.Applicative
import Text.Parsec
import Data.Char

-- | Read-Eval-Print Loop: quick, dirty, and ugly
repl n ste = do
    let quit = return (Right ste)
    let continue = repl (n + 1)
    end <- hIsEOF stdin
    if end then quit else do
        lineIn <- getLine
        case reverse . dropWhile isSpace . reverse $ lineIn of
            ""        -> continue ste
            ":q"      -> quit
            ":quit"   -> quit
            ":exit"   -> quit
            ':':'t':' ':str -> do
                withParse ste (infer <$> pTypeTable <*> pterm) str $ \term -> do
                    case termType term of
                        Left err -> printErr err
                        Right ty -> putStrLn (show term ++ ": " ++ show ty)
                continue ste
            --':':'d':' ':str -> quit
            ":clear"  -> continue (ste { pStack = Stack []})
            ":ls"     -> printFuncs ste >> continue ste
            line      -> do
                let parser' = fmap (flip setSourceLine n) getPosition >>= setPosition >> ptop
                ste'' <- case runParser parser' ste "<repl>" line of
                    Left err   -> printErr err >> return ste
                    Right ste' -> (putStrLn $ show $ pStack ste') >> return ste'
                continue ste''

printErr s = hPutStrLn stderr ("ERROR: " ++ show s)

withParse ste parser str action =
    case runParser (skip >> parser) ste "<repl>" str of
        Left err -> printErr err
        Right term -> action term


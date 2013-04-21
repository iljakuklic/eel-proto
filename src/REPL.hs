

module REPL(repl) where

import Parser.Pokus
import Parser.State
import Parser.Dump
import Sema.Term
import Sema.Infer
import Sema.Common

import System.IO
import Control.Applicative
import Text.Parsec
import Data.Char
import qualified Data.Map as M

-- | REPL + welcome message
repl n ste = do
    hSetBuffering stdout NoBuffering
    putStrLn "Welcome to the EEL interactive interpreter!"
    putStrLn "Type :? for help"
    putStr ">>> "
    repl' n ste

-- | Read-Eval-Print Loop: quick, dirty, and ugly
repl' n ste = do
    let quit = return (Right ste)
    let continue ste' = putStr ">>> " >> repl' (n + 1) ste'
    end <- hIsEOF stdin
    if end then quit else do
        lineIn <- getLine
        case reverse . dropWhile isSpace . reverse $ lineIn of
            ""        -> continue ste
            ":q"      -> quit
            ":quit"   -> quit
            ":exit"   -> quit
            ':':'t':' ':str -> do
                withParse ste pinfer str $ \term -> do
                    case termType term of
                        Left err -> printErr err
                        Right ty -> putStrLn (show term ++ ": " ++ show ty)
                continue ste
            ':':'d':' ':str -> do
                withParse ste pinfer str $ \term -> putStrLn (dumpTerm term)
                continue ste
            ':':'i':' ':str -> do
                case M.lookup (Symbol str) (pSymTable ste) of
                    Nothing -> printErrStr ("Could not find symbol: '" ++ str ++ "'")
                    Just fd -> case fd of
                        FDUser _ t  -> putStrLn (dumpTerm t)
                        FDBuiltIn _ -> putStrLn "<builtin>"
                continue ste
            ":clear"  -> continue (ste { pStack = Stack []})
            ":ls"     -> printFuncs ste >> continue ste
            ":l"      -> printFuncs ste >> continue ste
            ':':str   -> printErrStr ("Unknown command: " ++ show str) >> continue ste
            line      -> do
                let parser' = fmap (flip setSourceLine n) getPosition >>= setPosition >> ptop
                ste'' <- case runParser parser' ste "<repl>" line of
                    Left err   -> printErr err >> return ste
                    Right ste' -> (putStrLn $ show $ pStack ste') >> return ste'
                continue ste''

pinfer = infer <$> pTypeTable <*> pterm

printErr s    = printErrStr (show s)
printErrStr s = hPutStrLn stderr ("ERROR: " ++ s)

withParse ste parser str action =
    case runParser (skip >> parser) ste "<repl>" str of
        Left err -> printErr err
        Right term -> action term




module Main.REPL(repl) where

import Parser.Pokus
import Parser.State
import Parser.Dump
import Sema.Term
import Sema.Infer

import System.IO
import Control.Applicative
import Text.Parsec
import Data.Char
import qualified Data.Map as M

-- | REPL + welcome message
repl ste = do
    hSetBuffering stdout NoBuffering
    putStrLn "Welcome to the EEL interactive interpreter!"
    putStrLn "Type :? for help"
    prompt 1
    repl' 1 ste

-- | Read-Eval-Print Loop: quick, dirty, and ugly
repl' n ste = do
    let quit = return (Right ste)
    let continue ste' = prompt (succ n) >> repl' (succ n) ste'
    let continue' = continue ste
    end <- hIsEOF stdin
    if end then quit else do
        lineIn <- getLine
        case reverse . dropWhile isSpace . reverse $ lineIn of
            ""        -> continue ste
            ":q"      -> quit
            ":quit"   -> quit
            ":exit"   -> quit
            ":?"      -> help >> continue'
            ":h"      -> help >> continue'
            ":help"   -> help >> continue'
            ':':'t':' ':str -> do
                withParse n ste pinfer str $ \term -> do
                    case termType term of
                        Left err -> printErr err
                        Right ty -> putStrLn (show term ++ ": " ++ show ty)
                continue'
            ':':'d':' ':str -> do
                withParse n ste pinfer str $ \term -> putStrLn (dumpTerm term)
                continue'
            ':':'i':' ':str -> do
                case M.lookup (Symbol str) (pSymTable ste) of
                    Nothing -> printErrStr ("Could not find symbol: '" ++ str ++ "'")
                    Just fd -> case fd of
                        FDUser term -> putStrLn (dumpTerm term)
                        FDBuiltIn _ -> putStrLn "<builtin>"
                continue'
            ":x"      -> continue (ste { pStack = Stack []})
            ":ls"     -> printFuncs ste >> continue'
            ":l"      -> printFuncs ste >> continue'
            ":s"      -> stackDump  ste >> continue'
            ':':str   -> printErrStr ("Unknown command: " ++ show str) >> continue'
            line      -> do
                ste'' <- case runParser (onLine n >> ptop) ste "<interactive>" line of
                    Left err   -> printErr err >> return ste
                    Right ste' -> (putStrLn $ show $ pStack ste') >> return ste'
                continue ste''

stackDump ste = putStrLn (show stk) -- >> putStrLn ("Type: " ++ show ty)
    where
        stk = pStack ste
        --ty  = stackInfer (pTypeTablePure ste) stk

prompt n = putStr (zeroPad 3 (show (n :: Int)) ++ "> ")
zeroPad n = reverse . take n . (++ repeat '0') . reverse

pinfer = infer <$> pTypeTable <*> pterm

printErr s    = printErrStr (show s)
printErrStr s = hPutStrLn stderr ("ERROR: " ++ s)

withParse n ste parser str action =
    case runParser (onLine n >> skip >> parser) ste "<interactive>" str of
        Left err -> printErr err
        Right term -> action term

onLine n = fmap (flip setSourceLine n) getPosition >>= setPosition

help = putStr helpMsg
helpMsg = unlines [
    "  Available commands:",
    "    :?         show this help message",
    "    :q         quit",
    "    :t EXPR    print type of an expression",
    "    :d EXPR    dump AST of an expression",
    "    :i NAME    dump AST of an user-defined function",
    "    :x         clear the stack",
    "    :s         show current stack",
    "    :l         list of defined functions"
  ]

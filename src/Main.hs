

module Main(main) where

import Compiler
import CommandLine
import Parser.Pokus

import System.Environment
import System.IO
import Control.Monad
import qualified Control.Exception as E

main' = do
    args <- getArgs
    case parseCommandLine args of
        Left cmderr -> hPutStrLn stderr (show cmderr)
        Right mode -> case mode of
            Nothing -> putStr commandLineHelp
            Just settings -> do
                result <- runCompiler settings
                case result of
                    Left err  -> hPutStrLn stderr (show err)
                    Right ste -> do
                        when (verboseOutput settings) $ do
                            printFuncs ste
                            putStrLn "--------------------"
                        printStack ste

main = main' `E.catch` (\e -> putStrLn ("ERROR: " ++ show (e :: IOError)))


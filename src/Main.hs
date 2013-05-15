

module Main(main) where

import Main.Compiler
import Main.CommandLine
import Parser.Dump

import System.Environment
import System.IO
import Control.Monad
import qualified Control.Exception as E

main' = do
    args <- getArgs
    case parseCommandLine args of
        Left cmderr -> hPutStrLn stderr (show cmderr)
        Right mode -> case mode of
            Nothing -> writeHelp
            Just settings -> do
                result <- runCompiler settings
                case result of
                    Left err  -> hPutStrLn stderr (show err)
                    Right ste -> do
                        when (verboseOutput settings) $ do
                            putStrLn "--------------------"
                            printFuncs ste
                            putStrLn "--------------------"
                            unless (interactMode settings) (printStack ste)

main = main' `E.catch` (\err -> putStrLn ("ERROR: \n" ++ show (err :: IOError)))


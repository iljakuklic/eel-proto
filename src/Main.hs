

module Main(main) where

import Compiler
import CommandLine

import System.Environment
import System.IO

main = do
    args <- getArgs
    case parseCommandLine args of
        Left cmderr -> hPutStrLn stderr (show cmderr)
        Right mode -> case mode of
            Nothing -> putStr commandLineHelp
            Just settings -> do
                result <- runCompiler settings
                case result of
                    Left err  -> hPutStrLn stderr (show err)
                    Right ste -> printState ste



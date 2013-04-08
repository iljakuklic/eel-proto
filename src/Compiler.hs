
module Compiler(Settings(..), runCompiler, printState) where

import Parser.Pokus

import Control.Monad
import Text.Parsec

-- | Compiler settings
data Settings = Settings {
        outputFilePath :: FilePath,         -- ^ output binary file
        llvmFilePath   :: Maybe FilePath,   -- ^ output LLVM file
        verboseOutput  :: Bool,             -- ^ extra debugging output
        inputFilePaths :: [FilePath]        -- ^ input files to read
    } deriving Show

preludeName = "prelude.eel"

-- | Parse the list of strings with identifiers (filenames) in sequence
parseStrs initSte strs = foldM parseStr initSte strs
  where parseStr ste (name, str) = runParser ptop ste name str

-- | Run the compiler with given settings.
runCompiler settings = do
    inp <- input
    return $ parseStrs initState (zip infiles inp)
  where
    infiles = inputFilePaths settings
    input   = mapM readFile infiles




module Main.Compiler(Settings(..), InputSpec(..), runCompiler) where

import Parser.Pokus
import Main.REPL

import Prelude hiding (catch)
import Control.Monad
import Text.Parsec
import System.FilePath
import Control.Exception

-- | Input specification
data InputSpec
     = InputFile FilePath    -- ^ read file as input
     | InputLit  String      -- ^ literal string input

-- | Compiler settings
data Settings = Settings {
        outputFilePath :: FilePath,         -- ^ output binary file
        llvmFilePath   :: Maybe FilePath,   -- ^ output LLVM file
        verboseOutput  :: Bool,             -- ^ extra debugging output
        interactMode   :: Bool,             -- ^ enable interactive mode
        noPreludeFlag  :: Bool,             -- ^ disable prelude autoload
        evalString     :: String,           -- ^ string to evaluate
        inputFilePaths :: [FilePath]        -- ^ input files to read
    } deriving Show

-- | file lookup directories
lookupDirs = [".", "lib/", "test/"]

-- | prelude file name
preludeName = "prelude.eel"

-- | Parse the list of strings with identifiers (filenames) in sequence
parseStrs initSte strs = foldM parseStr initSte strs
  where parseStr ste (name, str) = runParser ptop ste name str

-- | Lookup file contents in a list of directories
lookupFile :: [FilePath] -> FilePath -> IO (Maybe String)
lookupFile _ file | isAbsolute file  = fmap Just (readFile file) `catch` constE (lookupFile [] "")
lookupFile (dir:dirs) file = fmap Just (readFile (dir </> file)) `catch` constE (lookupFile dirs file)
lookupFile [] _file        = return Nothing
constE = const :: a -> IOError -> a

-- | Read specified input
readIn :: InputSpec -> IO (String, String)
readIn (InputFile path) = do
    mbProg <- lookupFile lookupDirs path
    case mbProg of
        Just prog -> return (path, prog)
        Nothing -> fail ("Could not load " ++ show path)
readIn (InputLit  prog) = return ("<commandline>", prog)

-- | Run the compiler with given settings.
runCompiler settings = do
    inp <- input
    let res' = parseStrs initState inp
    if interactMode settings
        then either (return . Left) (\ste' -> repl ste') res'
        else return res'
  where
    infiles    = map InputFile $ inputFilePaths settings
    input      = mapM readIn inputSpec
    prelude'   = onFlag (not . noPreludeFlag) (InputFile preludeName)
    cmdeval'   = [InputLit (evalString settings)]
    inputSpec  = prelude' ++ infiles ++ cmdeval'
    onFlag f x = if f settings then [x] else []



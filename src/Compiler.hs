
module Compiler(Settings(..), InputSpec(..), runCompiler) where

import Parser.Pokus
import Parser.State
import Sema.Term(Stack(Stack))
import Control.Applicative
import Sema.Infer

import Prelude hiding (catch)
import Control.Monad
import Text.Parsec
import System.FilePath
import Control.Exception
import System.IO
import Data.Char

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
                case runParser (skip >> (infer <$> pTypeTable <*> pterm)) ste "<repl>" str of
                    Left err -> printErr err
                    Right term -> case termType term of
                        Left err -> printErr err
                        Right ty -> putStrLn (show term ++ ": " ++ show ty)
                continue ste
            ":clear"  -> continue (ste { pStack = Stack []})
            ":ls"     -> printFuncs ste >> continue ste
            line      -> do
                let parser' = fmap (flip setSourceLine n) getPosition >>= setPosition >> ptop
                ste'' <- case runParser parser' ste "<repl>" line of
                    Left err   -> printErr err >> return ste
                    Right ste' -> (putStrLn $ show $ pStack ste') >> return ste'
                continue ste''
  where printErr s = hPutStrLn stderr ("ERROR: " ++ show s)
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
        then either (return . Left) (\ste' -> repl 1 ste') res'
        else return res'
  where
    infiles    = map InputFile $ inputFilePaths settings
    input      = mapM readIn inputSpec
    prelude'   = onFlag (not . noPreludeFlag) (InputFile preludeName)
    cmdeval'   = [InputLit (evalString settings)]
    inputSpec  = prelude' ++ infiles ++ cmdeval'
    onFlag f x = if f settings then [x] else []



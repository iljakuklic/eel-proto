{-
    EEL -- Extensible Experimental Language
    by Lukáš Kuklínek, 2013
-}

module Main.Compiler(Settings(..), InputSpec(..), runCompiler) where

import Main.REPL
import Parser.Parser

import Prelude hiding (catch)
import Control.Monad
import System.FilePath
import System.Directory
import System.Process
import System.IO
import Control.Exception

-- | Input specification
data InputSpec
     = InputFile FilePath    -- ^ read file as input
     | InputLit  String      -- ^ literal string input

-- | Compiler settings
data Settings = Settings {
        outputFilePath :: Maybe FilePath,   -- ^ output binary file
        llvmFilePath   :: Maybe FilePath,   -- ^ output LLVM file
        asmFilePath    :: Maybe FilePath,   -- ^ output assembly file
        verboseOutput  :: Bool,             -- ^ extra debugging output
        interactMode   :: Bool,             -- ^ enable interactive mode
        noPreludeFlag  :: Bool,             -- ^ disable prelude autoload
        evalString     :: String,           -- ^ string to evaluate
        mainFuncName   :: String,           -- ^ name of the main function
        inputFilePaths :: [FilePath]        -- ^ input files to read
    } deriving Show

-- | file lookup directories
lookupDirs = [".", "lib/", "test/"]

-- | prelude file name
preludeSpec = [ InputFile "prelude.eel" ]

-- | Parse the list of strings with identifiers (filenames) in sequence
parseStrs initSte strs = foldM parseStr initSte strs
  where parseStr ste (name, str) = runEelExt name ste str

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
    res'' <- if interactMode settings
        then either (return . Left) (\ste' -> repl ste') res'
        else return res'
    ste'' <- either (fail . show) return res''
    case semaCheck mainName ste'' of
        Right tab -> when genCode $ do
            -- LLVM file creation
            (llFName, llFHandle) <- openTempFile "." "out.ll"
            hPutStrLn llFHandle (code tab)
            hClose llFHandle
            let asmFName = flip replaceExtension "s"   llFName
            let outFName = flip replaceExtension "bin" llFName
            -- call LLVM compiler
            _ <- rawSystem "llc" ["-o", asmFName, llFName]
            -- call gcc to link stuff
            when (outputFilePath settings /= Nothing) . void $
                rawSystem "gcc" ["-lgc", "-lm", "-o", outFName, asmFName]
            -- move or delete .ll file
            moveOrDel llFName (llvmFilePath settings)
            -- move or delete .s file
            moveOrDel asmFName (asmFilePath settings)
            -- move or delete output binary
            when (outputFilePath settings /= Nothing) $
                moveOrDel outFName (outputFilePath settings)
        Left errs -> putStrLn (show errs)
    return res''
  where
    infiles    = map InputFile $ inputFilePaths settings
    input      = mapM readIn inputSpec
    prelude'   = onFlag (not . noPreludeFlag) preludeSpec
    cmdeval'   = [InputLit (evalString settings)]
    inputSpec  = prelude' ++ infiles ++ cmdeval'
    onFlag f x = if f settings then x else []
    code       = show . emitModule mainName
    moveOrDel fn Nothing = removeFile fn
    moveOrDel fn (Just fn') = renameFile fn fn'
    genCode = any genFile [llvmFilePath, asmFilePath, outputFilePath]
    genFile fun = (fun settings) /= Nothing
    mainSett = mainFuncName settings
    mainName = if genCode && mainSett /= "" then Just mainSett else Nothing

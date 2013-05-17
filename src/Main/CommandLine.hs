{-
    EEL -- Extensible Experimental Language
    by Lukáš Kuklínek, 2013
-}

module Main.CommandLine(CommandLineError, parseCommandLine, writeHelp) where

import Main.Compiler

import Text.Parsec
import Text.Parsec.Perm
import Text.Parsec.Pos
import Data.List
import qualified Text.PrettyPrint.HughesPJ as P
import Text.PrettyPrint.HughesPJ((<+>), ($+$), ($$))

import System.Environment
import Paths_eel as EEL
import Data.Version

-- | Command-line parsing error
newtype CommandLineError = CommandLineError ParseError

instance Show CommandLineError where 
    show (CommandLineError e) = unlines (header : origMsg)
      where
        colStr  = show (sourceColumn $ errorPos e)
        header  = "Could not parse command-line argument no. " ++ colStr
        origMsg = drop 1 $ lines $ show e

-- | Position-annotated command-line token
data CTok = CTok Int String

instance Show CTok where
    show (CTok pos tok) = "argument " ++ show tok ++ " on position " ++ show pos

-- | Command-line token
cmdTok test = token showT posT testT
  where
    showT (CTok _ tok) = show (tok :: String)
    posT  (CTok pos _) = newPos "<command-line>" 0 pos
    testT (CTok _ tok) = test tok


-- | token satisfying given predicate
satisfyTok p = cmdTok (\t -> if p t then Just t else Nothing)
-- | switch token
switchTok names = satisfyTok (\t -> t `elem` names) <?> "command line switch"
-- | command-line token
anyCmdTok = cmdTok Just

-- | Parse boolean switch
boolSwitch names = (False, switchTok names >> return True)
-- | Parse required switch
reqSwitch names fun = switchTok names >> (anyCmdTok <?> "switch argument") >>= return . fun
-- | Parse optional switch
optSwitch names def fun = (def, reqSwitch names fun)
-- | Parse optional switch w/ maybe
maybeSwitch names fun = optSwitch names Nothing (Just . fun) -- this is just fun
-- | One positional argument
onePosArg  = satisfyTok (\t -> not ("-" `isPrefixOf` t)) <?> "positional argument"
-- | One or more positional arguments
reqPosArgs = many1 onePosArg
-- | Zero or more positional arguments
optPosArgs = ([], reqPosArgs)

-- | Description of available command-line options for normal mode (not help)
cmdLineDesc = Settings
    <$?> maybeSwitch ["-o", "--output"] id
    <|?> maybeSwitch ["-L", "--LLVM", "--llvm"] id
    <|?> maybeSwitch ["-S", "--ASM", "--asm"] id
    <|?> boolSwitch  ["-v", "--verbose"]
    <|?> boolSwitch  ["-i", "--interactive"]
    <|?> boolSwitch  ["-P", "--no-prelude"]
    <|?> optSwitch   ["-e", "--eval"] "" id
    <|?> optSwitch   ["-M", "--main"] "main" id
    <|?> optPosArgs

-- | Command-line parser
cmdLineParser = pp >>= (\s -> eof >> return s)
  where
    pp         = helpswitch <|> switches
    helpswitch = switchTok ["-h", "--help"] >> return Nothing
    switches   = permute cmdLineDesc >>= return . Just

-- | parse the command line
parseCommandLine args = case parse cmdLineParser "" (zipWith CTok [1..] args) of
    Right settings -> Right settings
    Left err -> Left (CommandLineError err)

-- | write help message
writeHelp = getProgName >>= putStrLn . show . commandLineHelp

-- | command-line help/usage message
commandLineHelp prog = P.text "" $+$ descr $+$ usage $+$ optDesc
  where
    descr = section "EEL -- Extensible Experimental Language" einfo
    einfo = P.vcat . map P.text $ [
        "version " ++ showVersion EEL.version,
        "by Lukáš Kuklínek <xkukli01@stud.fit.vutbr.cz>",
        "Part of the master's thesis for Faculty of Information Technology,",
        "Brno University of Technology, Brno, Czech Republic, 2013."
      ]
    usage = section "Usage:" (P.text prog <+> P.text "[OPTIONS] [SOURCES...]")
    optDesc = section "Options:" optList
    optList = P.vcat [
        ln "-o FILE, --output FILE" "generate output binary file named FILE",
        ln "-L FILE, --llvm FILE" "generate LLVM IR text file named FILE",
        ln "-S FILE, --asm FILE" "generate assembly source file named FILE",
        ln "-v, --verbose" "increase output verbosity",
        ln "-i, --interactive" "launch interactive read-eval-print interpreter",
        ln "-P, --no-prelude" "do not load the prelude library automatically",
        ln "-e EXPR, --eval EXPR" "evaluate given EXPRession in EEL core",
        ln "-M NAME, --main NAME" "specify the name of the main function",
        ln "-h, --help" "show this help message"
      ]
    ln opts desc = P.text opts $$ P.nest 4 (P.text desc)
    section heading body = P.text heading $+$ P.nest 4 body $+$ P.text ""

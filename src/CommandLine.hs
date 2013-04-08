
module CommandLine(CommandLineError, parseCommandLine, commandLineHelp) where

import Compiler

import Text.Parsec
import Text.Parsec.Perm
import Text.Parsec.Pos
import Data.List

-- | Command-line parsing error
newtype CommandLineError = CommandLineError ParseError

instance Show CommandLineError where 
    show (CommandLineError e) = unlines (("Could not parse command-line argument no. " ++ show (sourceColumn $ errorPos e)) : (drop 1 $ lines $ show e))

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
    <$?> optSwitch   ["-o", "--output"] "a.out" id
    <|?> maybeSwitch ["-L", "--LLVM", "--llvm"] id
    <|?> boolSwitch  ["-v", "--verbose"]
    <|?> boolSwitch  ["-i", "--interactive"]
    <|?> optSwitch   ["-e", "--eval"] "" id
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

-- | command-line help/usage message
commandLineHelp = undefined


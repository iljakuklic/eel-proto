

module Backend.CodeGen(
        -- * Code generation functions
        fill, codegen,
        -- * LLVM types
        LLType, llTypeDoc, llInt, llIntNative, llRaw, llPair, llSum, llFloat, llChar, llNamed, llStruct, llPtr, llSize, llBool,
        -- * Platform specifics
        ptrBytes, ptrBits, sizeBits, intBits, charBits,
        -- * Variables and identifiers
        LLVar(..), llVarName, llVarDoc,
        llVarInt, llVarChar, llVarFloat, llVarSize, llVarNull,
        freshRaw, fresh, fresh1, freshGlobal1,
        -- * Stack manipulation
        getStack, setStack,
        -- * Genral code emission
        mapCode, appendRaw, append, appendTmpl, blankLine,
        -- * LLVM constructs
        label, instr, comment, braceBlock, funcHeader, funcPrototype,
        -- * LLVM instructions
        phi, branch, branchCond, gep, gepFst, gepSnd, load, store, bitcast, callRaw, ret
    ) where

import Control.Monad.State(get, put, modify, runState)
import qualified Data.Map as M
import qualified Text.PrettyPrint as P
import Text.PrettyPrint(($+$), (<+>), (<>))
import Control.Applicative
import Data.Char

import Foreign.C.Types
import Foreign.Storable
 
-- | bit version of the sizeof operator
bitSize x = sizeOf x * 8

-- platform-dependent bit sizes of various data types
sizeBits    = bitSize (undefined :: CSize)
intBits     = bitSize (undefined :: CInt)
charBits    = bitSize (undefined :: CChar)
ptrBits     = bitSize (undefined :: CIntPtr)
ptrBytes    = sizeOf  (undefined :: CIntPtr)

-- | LLVM types (low-level)
data LLType = LLVoid                    -- ^ void type (actually roughly unit type)
            | LLFloat                   -- ^ floating-point type
            | LLDouble                  -- ^ double precision floating-point type
            | LLInt Int                 -- ^ n-bit integer
            | LLPtr LLType              -- ^ Pointer to a type
            | LLStruct [LLType]         -- ^ Struct
            | LLFunc (LLType) [LLType]  -- ^ function type
            | LLNamed String            -- ^ named type
            deriving Eq

-- | render type in LLVM syntax
instance Show LLType where show = show . llTypeDoc

-- | convert a LLVM type to a Doc
llTypeDoc LLVoid        = P.text "void"
llTypeDoc LLFloat       = P.text "float"
llTypeDoc LLDouble      = P.text "double"
llTypeDoc (LLInt b)     = P.char 'i' <> P.int b
llTypeDoc (LLPtr t)     = llTypeDoc t <> P.char '*'
llTypeDoc (LLStruct ts) = P.braces $ llTypesDoc ts
llTypeDoc (LLFunc r ts) = llTypeDoc r <+> (P.parens $ llTypesDoc ts)
llTypeDoc (LLNamed nm)  = P.text nm
llTypesDoc = P.hsep . P.punctuate P.comma . fmap llTypeDoc

-- helpers
llInt  = LLInt ptrBits
llBool = LLInt 1
llChar = LLInt charBits
llRaw  = LLPtr (LLInt 8)
llPairT a b = LLStruct [a, b]
llPair = llPairT llRaw llRaw
llSum  = llPairT llRaw llBool
llFloat = if ptrBits == 64 then LLDouble else LLFloat
llSize = LLInt sizeBits
llNamed = LLNamed
llStruct = LLStruct
llPtr = LLPtr
llIntNative = LLInt intBits
llIsFloating LLFloat = True
llIsFloating LLDouble = True
llIsFloating _ = False

-- | LLVM variable
data LLVar = LLVar LLType String deriving (Eq)

-- | Show LLVM variable
instance Show LLVar where show = show . llVarDoc

-- | convert LLVM variable to a doc
llVarDoc (LLVar ty name) = llTypeDoc ty <+> P.text name
-- | Doc-ify a number of comma-separated variables
llVarsDoc = P.sep . P.punctuate P.comma . map llVarDoc
-- | get LLVM variable name
llVarName (LLVar _ty nm) = nm

-- | LLVM variable holding an integer constant
llVarInt   = LLVar llInt . show
-- | LLVM variable holding a floating-point constant
llVarFloat = LLVar llFloat . show
-- | LLVM variable holding a character constant
llVarChar  = LLVar llInt . show . ord
-- | LLVM variable holding a size_t constant
llVarSize  = LLVar llSize . show
-- | LLVM variable holding a NULL pointer constant
llVarNull  = LLVar llRaw "null"

-- | Code generator state
data CodeGenState = CodeGenState {
        cgStack  :: LLVar,             -- ^ pointer to the top of the stack
        cgVars   :: M.Map String Int,  -- ^ fresh variables counter
        cgOutput :: P.Doc              -- ^ generated document with source
    } deriving Show

-- | generate fresh variables w/ given prefixes (must all be unique)
freshRaw xs = do
    ste <- get
    let vars = cgVars ste
    let n = maximum . fmap (\k -> M.findWithDefault 0 k vars) $ xs
    put (ste { cgVars = M.union (M.fromList [(v,n+1) | v <- xs]) vars })
    return . fmap (++ show n) $ xs

-- | generate fresh local variables ('%' will be prepended to the begining, prefixes must be unique)
fresh ty nms = freshRaw (fmap ('%':) nms) >>= return . fmap (LLVar ty)

-- | generate a fresh local variable ('%' will be prepended to the begining)
fresh1 ty nm = head <$> fresh ty [nm]

-- | generate a fresh global identifier ('@' will be prepended at the begining)
freshGlobal1 ty nm = fmap (LLVar ty) <$> freshRaw ['@':nm]

-- | get current stack
getStack = cgStack <$> get

-- | set current stack
setStack stk = modify (\ste -> ste { cgStack = stk })

-- | Modify the code generated so far
mapCode f = modify (\ste -> ste { cgOutput = f $ cgOutput ste})

-- | get the generated code
getCode = cgOutput <$> get

-- | Add source code to the output buffer
appendRaw code = mapCode ($+$ code)

-- | Add source code to the output buffer, indented to the instruction level
append = appendRaw . indent

-- | Add code by filling a templete
appendTmpl tmpl = append . fill tmpl

-- | Add a blank line
blankLine = appendRaw (P.text "")

-- | Fill arguments into the string template
fill tmpl args = P.text $ fill' tmpl args
  where
    fill' ('#':xs) (a:as) = show a ++ fill' xs as
    fill' (x:xs) as = x : fill' xs as
    fill' rest []  | '#' `elem` rest = error ("Too many placeholders in " ++ tmpl)
                   | otherwise       = rest
    fill' [] (_:_) = error ("Excessive arguments in " ++ tmpl)

-- | indent the source text
indent = P.nest 8

-- | Convert any Show-able type to a Doc
showDoc = P.text . show

-- | Emit label LLVM code
label lab = appendRaw (P.nest 4 (P.text lab <> P.colon))

-- | Emit an instruction w/ return value
instr ty varname tmpl args = do
    var <- fresh1 ty varname
    append (P.text (llVarName var) <+> P.equals <+> fill tmpl args)
    return var

-- | Emit a comment
comment text args = append (P.semi <+> fill text args)

-- | Enclose a document in braces
braceBlock doc = P.lbrace $+$ indent doc $+$ P.rbrace

-- | type conversion instrunction
bitcast ty nme val@(LLVar vty _) | vty == ty = return val
bitcast ty@(LLInt _) nme val@(LLVar (LLPtr _) _) = instr ty nme "ptrtoint # to #" [llVarDoc val, llTypeDoc ty]
bitcast ty@(LLPtr _) nme val@(LLVar (LLInt _) _) = instr ty nme "inttoptr # to #" [llVarDoc val, llTypeDoc ty]
bitcast ty' nme val@(LLVar (LLPtr _) _)    | llIsFloating ty' = bitcast llInt "tmp" val >>= bitcast ty' nme
bitcast ty'@(LLPtr _) nme val@(LLVar ty _) | llIsFloating ty  = bitcast llInt "tmp" val >>= bitcast ty' nme
bitcast ty nme val = instr ty nme "bitcast # to #" [llVarDoc val, llTypeDoc ty]
-- | getelementptr instruction
gep ty nme var idxs = instr ty nme "getelementptr #, #" [llVarDoc var, llVarsDoc $ map (LLVar (LLInt 32) . show) idxs]
-- | get a pointer to the first member of a pair
gepFst ty nme var = gep ty nme var [0, 0 :: Int]
-- | get a pointer to the second member of a pair
gepSnd ty nme var = gep ty nme var [0, 1 :: Int]
-- | load a value from the memory
load ty nme ptr = instr ty nme "load #" [ptr]
-- | store a value to the memory
store val ptr = appendTmpl "store #, #" [val, ptr]
-- | function call
callRaw ty name fname args = instr ty name "call # #(#)" [llTypeDoc ty, P.text fname, llVarsDoc args]
-- | return
ret val = appendTmpl "ret #" [val]
-- | LLVM SSA phi node
phi ty name entries = instr ty name "phi # #" [llTypeDoc ty, inbbs]
  where inbbs = P.sep $ P.punctuate P.comma [ P.brackets $ P.text v <> P.comma <+> P.char '%' <> P.text l | (LLVar _ v, l) <- entries]
-- | branch
branch lab = appendTmpl "br label %#" [P.text lab]
-- | conditional branch
branchCond var thenL elseL = appendTmpl "br #, label %#, label %#" [llVarDoc var, P.text thenL, P.text elseL]

-- | function header
funcHeader rtype fname input = P.text "define" <+> llTypeDoc rtype <+> P.text fname <> P.parens (llVarsDoc input)
-- | function prototype
funcPrototype rtype fname input = appendRaw $ P.text "declare" <+> llTypeDoc rtype <+> P.text fname <> P.parens (llTypesDoc input)

-- | Run code generator
codegen g = fst $ runState (g >> getCode) (CodeGenState llVarNull M.empty P.empty)



module Backend.CodeGen(
        -- * LLVM types
        LLType, llInt, llPtr,
        -- * Variables and identifiers
        LLVar, freshRaw, fresh, fresh1, freshGlobal1,
        -- * Stack manipulation
        getStack, setStack, push, pop, dipn,
        -- * Genral code emission
        mapCode, appendRaw, append, appendTmpl,
        -- * LLVM rendering
        label, instr, comment, braceBlock,
        -- * Helper functions
        fill
    ) where

import Control.Monad.State(get, put, modify, runState)
import Data.Map as M
import Text.PrettyPrint as P
import Text.PrettyPrint(($+$), (<+>), (<>))
import Control.Applicative

import Foreign.C.Types
import Foreign.Storable
 
-- | bit version of the sizeof operator
bitSize x = sizeOf x * 8

-- platform-dependent bit sizes of various data types
sizeBits    = bitSize (undefined :: CSize)
intBits     = bitSize (undefined :: CInt)
charBits    = bitSize (undefined :: CChar)


-- | LLVM types (low-level)
data LLType = LLVoid                    -- ^ void type (actually roughly unit type)
            | LLInt Int                 -- ^ n-bit integer
            | LLPtr LLType              -- ^ Pointer to a type
            | LLStruct [LLType]         -- ^ Struct
            | LLFunc (LLType) [LLType]  -- ^ function type
            | LLNamed LLVar             -- ^ named type

-- | Boxed values representation
data BoxVal b v
              = BoxInt  v    -- ^ boxed and unboxed integer
              | BoxPair b b  -- ^ boxed pair of values
              | BoxSum  b v  -- ^ boxed tagged union
              | BoxPoly      -- ^ boxed polymorphic type
              deriving Show

-- | Generic box type
data Box v = Box v (BoxVal (Box v) v) deriving Show
-- | Specialized box type
type BoxV = Box LLVar

-- | Calculate the LLVM type of the box
boxType (Box _ bv) = LLPtr $ boxValType bv

-- | Calculate the LLVM type of the box value
boxValType (BoxInt _) = llInt
boxValType (BoxPair a b) = LLStruct [boxType a, boxType b]
boxValType (BoxSum  b v) = llSum
boxValType (BoxPoly)     = LLInt 8

-- | render type in LLVM syntax
instance Show LLType where show = show . llTypeDoc

-- | convert a LLVM type to a Doc
llTypeDoc (LLInt b)     = P.char 'i' <> P.int b
llTypeDoc (LLPtr t)     = llTypeDoc t <+> P.char '*'
llTypeDoc (LLStruct ts) = P.braces $ llTypesDoc ts
llTypeDoc (LLFunc r ts) = llTypeDoc r <+> (P.parens $ llTypesDoc ts)
llTypeDoc (LLNamed nm)  = P.text (show nm)
llTypesDoc = hsep . punctuate P.comma . fmap llTypeDoc

-- helpers
llInt  = LLInt intBits
llBool = LLInt 1
llSize = LLInt sizeBits
llChar = LLInt intBits
llRaw  = LLPtr (LLInt 8)
llPairT a b = LLStruct [a, b]
llPair = llPairT llRaw llRaw
llSum  = llPairT llRaw llBool
llFunc as bs =  LLFunc (LLStruct bs) as
llList = LLPtr . LLNamed . LLVar $ "%eel.list"
llListDef = llPairT llRaw llList

-- | LLVM variable
data LLVar = LLVar String deriving (Ord, Eq)

-- | Show LLVM variable
instance Show LLVar where show (LLVar name) = name

-- | Code generator state
data CodeGenState = CodeGenState {
        cgStack  :: [ LLVar ],         -- ^ Stack of LLVM variables
        cgVars   :: M.Map String Int,  -- ^ fresh variables counter
        cgOutput :: P.Doc              -- ^ generated document with source
    } deriving Show

-- | generate fresh variables w/ given prefixes (must all be unique)
freshRaw xs = do
    ste <- get
    let vars = cgVars ste
    let n = maximum . fmap (\k -> M.findWithDefault 0 k vars) $ xs
    put (ste { cgVars = M.union (M.fromList [(v,n+1) | v <- xs]) vars })
    return . fmap (\x -> LLVar (x ++ show n)) $ xs

-- | generate fresh local variables ('%' will be prepended to the begining, prefixes must be unique)
fresh ns = freshRaw $ fmap ('%':) ns

-- | generate a fresh local variable ('%' will be prepended to the begining)
fresh1 n = head <$> fresh [n]

-- | generate a fresh global identifier ('@' will be prepended at the begining)
freshGlobal1 n = freshRaw ['@':n]

-- | get current stack
getStack = cgStack <$> get

-- | set current stack
setStack stk = modify (\ste -> ste { cgStack = stk })

-- | Push an item onto the stack
push x = modify (\ste -> ste { cgStack = x : cgStack ste })

-- | Pop an item from the stack
pop = do (x:xs) <- getStack; setStack xs; return x

-- | perform code generation n levels deep in the stack
dipn n codegen = do
    (topn, stk) <- splitAt n <$> getStack
    setStack stk
    codegen
    modify (\ste -> ste { cgStack = topn ++ cgStack ste })

-- | Modify the code generated so far
mapCode f = modify (\ste -> ste { cgOutput = f $ cgOutput ste})

-- | Add source code to the output buffer
appendRaw code = mapCode ($+$ code)

-- | Add source code to the output buffer, indented to the instruction level
append = appendRaw . P.nest 4

-- | Add code by filling a templete
appendTmpl tmpl = append . fill tmpl

-- | Fill arguments into the string template
fill tmpl args = P.text $ fill' tmpl args
  where
    fill' ('#':xs) (a:as) = show a ++ fill' xs as
    fill' (x:xs) as = x : fill' xs as
    fill' rest []  | '#' `elem` rest = error ("Too many placeholders in " ++ tmpl)
                   | otherwise       = rest
    fill' [] (_:_) = error ("Excessive arguments in " ++ tmpl)

-- | indent the source text
indent = P.nest 4

-- | Convert any Show-able type to a Doc
showDoc = P.text . show

-- | Emit label LLVM code
label var = append (showDoc var <> P.colon)

-- | Emit an instruction w/ return value
instr varname tmpl args = do
    var <- fresh1 varname
    append (showDoc var <+> P.equals <+> fill tmpl args)
    return var

-- | Emit a comment
comment text args = append (P.semi <+> fill text args)

-- | Enclose a document in braces
braceBlock doc = P.lbrace $+$ P.nest 4 doc $+$ P.rbrace

-- | test code generator
testCodeGen = flip runState (CodeGenState [] M.empty)

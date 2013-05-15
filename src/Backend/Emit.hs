

module Backend.Emit(emitTerm, emitModule, codegen, genTerm) where

import Sema.Term
import Sema.Types
import Backend.CodeGen
import Backend.Preamble

--import Control.Applicative
import qualified Text.PrettyPrint as P
import Text.PrettyPrint((<+>))
import qualified Data.Map as M

-- | Emit LLVM code for all entries in a symbol table
emitModule mainName symTab = codegen $ do
    preamble (fmap eelName mainName)
    sequence [ blankLine >> emitFunc (show n) t | (n, FDUser t) <- M.toList symTab ]

-- | 
genTerm = show . codegen . emitFunc "cmdline"

-- | Emit EEL code for a function from a term
emitFunc fname term = generateFunc (eelName fname) (emitTerm term)

-- | Emit EEL code for a function using a generator
generateFunc fname gen = do
    blankLine
    input <- fresh1 llStack "stk"
    setStack input
    appendRaw (funcHeader llStack fname [input] <+> P.lbrace)
    () <- gen
    blankLine
    comment "Return value" ([] :: String)
    output <- getStack
    ret output
    appendRaw P.rbrace
    
eelName nme = "@eel." ++ nme

-- | Emit code for EEL term
emitTerm (TComp _ f g)   = emitTerm f >> emitTerm g
emitTerm (TFunc _ nme t) = emitFD nme t
emitTerm (TQuot _ f)     = comment "Quoting not implemented: #" [f]
emitTerm t = do
    blankLine
    comment "Pushing value # to the stack" [t]
    emitVal t

-- | Emit code for EEL value
emitVal (TInt _ x)   = push $ llVarInt   x
emitVal (TFloat _ x) = push $ llVarFloat x
emitVal (TChar _ x)  = push $ llVarChar  x
emitVal v = comment "Not implemented: #" [v]

-- | Emit function call or builtin code
emitFD nme (FDUser _term) = do
    blankLine
    comment "Call #" [nme]
    inp <- getStack
    out <- callRaw llStack "stk" (eelName $ show nme) [inp]
    setStack out
emitFD _nme (FDBuiltIn bi) = do
    blankLine
    comment "Emitting builtin #" [drop 2 (show bi)]
    emitBI bi

-- combinators
emitBI BIzap = pop llRaw "dead" >> return ()
emitBI BIdup = do x <- pop llRaw "dup"; push x; push x
emitBI BIid  = return ()
emitBI BIid2  = return ()
-- integers
emitBI BIadd = arith llInt "add"
emitBI BIsub = arith llInt "sub"
emitBI BImul = arith llInt "mul"
emitBI BIdiv = arith llInt "sdiv"
-- floats
emitBI BIfadd = arith llFloat "fadd"
emitBI BIfsub = arith llFloat "fsub"
emitBI BIfmul = arith llFloat "fmul"
emitBI BIfdiv = arith llFloat "fdiv"
-- catch
emitBI bi = comment "Builtin not implemented: #" [drop 2 $ show bi]

arith ty iname = do
    y <- pop ty "y"
    x <- pop ty "x"
    r <- instr ty "r" (iname ++ " #, " ++ llVarName y) [x]
    push r

-- | Get size of EEL type in bytes for allocation
llSizeOf (TyBin TyProd _ _) = 2 * ptrBytes
llSizeOf (TyBin TySum  _ _) = 2 * ptrBytes
llSizeOf _x = ptrBytes

-- | Get LLVM type for given EEL type
llType t | t == tUnit  = llRaw
llType t | t == tInt   = llInt
llType t | t == tFloat = llFloat
llType t | t == tChar  = llInt
llType (TyAtom x)  = error ("Missing atomic type " ++ show x)
llType (TyList _) = llRaw
llType (TyBin TyProd _ _) = llPair
llType (TyBin TySum  _ _) = llSum
llType (TyBin (TyFunc _) _ _) = llPair
llType (TyVar _) = llRaw
llType (TyPhase _) = error "No LLVM counterpart to a phase type"

-- | Push an item onto the stack
push x = do
    oldStk <- getStack
    newStk' <- alloc llStack "stk" (2 * ptrBytes)
    setStack newStk'
    newStk <- bitcast llStackPtr "tstk" newStk'
    nextStk <- gepFst (llPtr llStack) "nstk" newStk
    store oldStk nextStk
    valStk <- gepSnd (llPtr llRaw) "vstk" newStk
    x' <- bitcast llRaw "push" x
    store x' valStk

-- | Pop an item from the stack
pop ty nme = do
    oldStk' <- getStack
    oldStk <- bitcast (llStackPtr) "tstk" oldStk'
    valPtr <- gepSnd (llPtr llRaw) "vptr" oldStk
    valPtr' <- bitcast (llPtr ty) "val" valPtr
    val <- load ty nme valPtr'
    newStkP <- gepFst (llPtr llStack) "pstk" oldStk
    newStk <- load llStack "stk" newStkP
    setStack newStk
    return val

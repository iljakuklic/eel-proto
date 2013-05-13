

module Backend.Emit(emitTerm, codegen, genTerm) where

import Sema.Term
import Sema.Types
import Backend.CodeGen
import Parser.State

--import Control.Applicative
import qualified Text.PrettyPrint as P
import Text.PrettyPrint(($+$), (<+>), (<>))

genTerm = show . codegen . emitTerm

takeRows term = (tyRow tyIn, tyRow tyOut)
  where Right (TyBin (TyFunc _) tyIn tyOut) = termType term

-- | Emit EEL code for a function
emitFunc fname term = do
    appendRaw (P.text "; Function definition:" <+> P.text fname <+> (P.parens . P.text . show . termType $ term))
    

    

-- | Emit code for EEL term
emitTerm (TComp _ f g)   = emitTerm f >> emitTerm g
emitTerm (TFunc _ nme t) = emitFD nme t
emitTerm (TQuot _ f)     = comment "Quoting not implemented: #" [f]
emitTerm t = emitVal t

-- | Emit code for EEL value
emitVal (TInt _ x)   = push $ llVarInt   x
emitVal (TFloat _ x) = push $ llVarFloat x
emitVal (TChar _ x)  = push $ llVarChar  x
emitVal v = comment "Not implemented: #" [v]

emitFD nme (FDUser _term) = comment "Not implemented: function call (#)" [nme]
emitFD _nme (FDBuiltIn bi) = emitBI bi

-- combinators
emitBI BIzap = pop >> return ()
emitBI BIdup = do x <- pop; push x; push x
emitBI BIid  = return ()
emitBI BIid2  = return ()
-- integers
emitBI BIadd = arith llInt "add"
emitBI BIsub = arith llInt "sub"
emitBI BImul = arith llInt "mul"
emitBI BIdiv = arith llInt "div"
emitBI BIfadd = arith llFloat "fadd"
emitBI BIfsub = arith llFloat "fsub"
emitBI BIfmul = arith llFloat "fmul"
emitBI BIfdiv = arith llFloat "fdiv"
emitBI bi = comment "Builtin not implemented: #" [drop 2 $ show bi]

arith ty iname = do
    y <- pop
    x <- pop
    y' <- bitcast ty "y" y
    x' <- bitcast ty "x" x
    r <- instr ty "n" (iname ++ " #, " ++ llVarName y') [x']
    r' <- bitcast llRaw "r" r
    push r'

-- | Allocate a piece of memory for given type
alloc ty nme = do
    ptr <- callRaw llRaw "ptr" "call" [llVarSize (llSizeOf ty)]
    bitcast (llType ty) nme ptr

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


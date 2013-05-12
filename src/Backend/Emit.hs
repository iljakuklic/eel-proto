

module Backend.Emit(emitTerm, codegen, genTerm) where

import Sema.Term
import Sema.Types
import Backend.CodeGen

--import Control.Applicative
import qualified Text.PrettyPrint as P
--import Text.PrettyPrint(($+$), (<+>), (<>))

genTerm = show . codegen . emitTerm

-- | Emit code for EEL term
emitTerm (TComp _ f g)   = emitTerm f >> emitTerm g
emitTerm (TFunc _ nme t) = emitFD nme t
emitTerm (TQuot _ f)     = comment "Quoting not implemented: #" [f]
emitTerm t = emitVal t

-- | Emit code for EEL value
emitVal (TInt _ x) = do
    n <- instr llInt "n" "extractvalue {#} {#}, 0" [llTypeDoc llInt, P.int x]
    push n
emitVal v = comment "Not implemented: #" [v]

emitFD nme (FDUser _term) = comment "Not implemented: function call (#)" [nme]
emitFD _nme (FDBuiltIn bi) = emitBI bi

-- combinators
emitBI BIzap = pop >> return ()
emitBI BIdup = do x <- pop; push x; push x
-- integers
emitBI BIadd = arith llInt "add"
emitBI BIsub = arith llInt "sub"
emitBI BImul = arith llInt "mul"
emitBI BIdiv = arith llInt "div"
emitBI BIid  = return ()
emitBI bi = comment "Builtin not implemented: #" [drop 2 $ show bi]

arith ty iname = do
    y <- pop
    x <- pop
    instr ty "n" (iname ++ " #, " ++ llVarName y) [x] >>= push


alloc ty nme = do
    ptr <- instr llRaw "ptr" "call i8* @GC_malloc(i# #)" [sizeBits, llSize ty]
    instr (llType ty) nme ("bitcast # to " ++ show (llType ty)) [ptr]

llSize (TyBin TyProd _ _) = 2 * ptrBytes
llSize (TyBin TySum  _ _) = 2 * ptrBytes
llSize _x = ptrBytes

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

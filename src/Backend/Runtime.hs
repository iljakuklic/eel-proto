

module Backend.Runtime(
        -- * EEL LLVM types
        llStack, llStackDef, llStackPtr, llStackFunc, llList, llListDef, llQotDef, llQot,
        -- * Runtime functions
        alloc, push, pop, generateFunc, mkPair, unPair, call,
        -- * Quotation runtime support
        mkQotData, mkQotFunc, mkQotComp, mkQotFixp, runQot
    ) where

import qualified Text.PrettyPrint as P
import Text.PrettyPrint((<+>))

import Backend.CodeGen

-- | Stack LLVM type
llStack = llPtr (llNamed "%eelstack")
-- | LLVM stack type definition
llStackDef = llStruct [llStack, llRaw]
-- | Pointer to an one-level-unwrapped stack
llStackPtr = llPtr llStackDef
-- | Pointer to an EEL function
llStackFunc = llPtr (llFunc llStack [llStack])

-- | List LLVM type
llList = llPtr (llNamed "%eellist")
-- | List LLVM type definition
llListDef = llStruct [llRaw, llList]

-- | EEL quaotation
llQot = llPtr (llNamed "%eelqot")
-- | EEL quotation definition
llQotDef = llStruct [llQotFunc, llRaw, llRaw]
llQotFunc = llPtr $ llFunc llStack [llStack, llRaw, llRaw]

-- | make a quotation
mkQotRaw f a b = do
    qot <- alloc llQot "qot" (3*ptrBytes)
    ra <- bitcast llRaw "raw" a
    rb <- bitcast llRaw "raw" b
    gep (llPtr llQotFunc) "ptrF" qot [0, 0] >>= store f
    gep (llPtr llRaw)     "ptrA" qot [0, 1] >>= store ra
    gep (llPtr llRaw)     "ptrB" qot [0, 2] >>= store rb
    return qot

-- | Create a function composition quotation
mkQotComp a b = mkQotRaw (LLVar llQotFunc "@eelrun.comp") a b
-- | Create a constant function quotation
mkQotData d   = mkQotRaw (LLVar llQotFunc "@eelrun.data") d (LLVar llRaw "null")
-- | Create a simple function quotation
mkQotFunc f   = mkQotRaw (LLVar llQotFunc "@eelrun.func") f (LLVar llRaw "null")
-- | Create a fixed point quotation
mkQotFixp f   = mkQotRaw (LLVar llQotFunc "@eelrun.fixp") f (LLVar llRaw "null")

-- | run a quotation
runQot qot' = do
    qot <- bitcast llQot "qot" qot'
    vf <- gep (llPtr llQotFunc) "ptrR" qot [0, 0] >>= load llQotFunc "runner"
    va <- gep (llPtr llRaw)     "ptrA" qot [0, 1] >>= load llRaw     "argA"
    vb <- gep (llPtr llRaw)     "ptrB" qot [0, 2] >>= load llRaw     "argB"
    stk <- getStack
    instr llStack "stk" "call # #(#, #, #)" [llTypeDoc llStack, P.text (llVarName vf),
        llVarDoc stk, llVarDoc va, llVarDoc vb] >>= setStack

-- | Allocate a piece of garbage-collected memory of given size
alloc ty nme size = do
    ptr <- callRaw llRaw "ptr" "@GC_malloc" [llVarSize size]
    bitcast ty nme ptr

-- | Push an item onto the stack
push x = do
    oldStk <- getStack
    newStk <- alloc llStack "stk" (2 * ptrBytes)
    setStack newStk
    nextStk <- gepFst (llPtr llStack) "nstk" newStk
    store oldStk nextStk
    valStk <- gepSnd (llPtr llRaw) "vstk" newStk
    x' <- bitcast llRaw "push" x
    store x' valStk

-- | Pop an item from the stack
pop ty nme = do
    oldStk <- getStack
    valPtr <- gepSnd (llPtr llRaw) "vptr" oldStk
    valPtr' <- bitcast (llPtr ty) "val" valPtr
    val <- load ty nme valPtr'
    newStkP <- gepFst (llPtr llStack) "pstk" oldStk
    newStk <- load llStack "stk" newStkP
    setStack newStk
    return val

-- | Emit EEL code for a function using a generator
generateFunc fname extraComment gen = do
    blankLine
    input <- fresh1 llStack "stk"
    setStack input
    appendRaw $ P.semi <+> extraComment
    appendRaw (funcHeader llStack fname [input] <+> P.lbrace)
    () <- gen
    comment "Return value" ([] :: String)
    output <- getStack
    ret output
    appendRaw P.rbrace

-- | create a pair of values
mkPair nam aData@(LLVar aTy _) bData@(LLVar bTy _) = do
    pair <- alloc (llPtr (llStruct [aTy, bTy])) nam (2*ptrBytes)
    gepFst (llPtr aTy) "aPtr" pair >>= store aData
    gepSnd (llPtr bTy) "bPtr" pair >>= store bData
    return pair

-- | deconstruct a pair of values
unPair nmA nmB pair@(LLVar (LLPtr (LLStruct [aTy, bTy])) _) = do
    a <- gepFst (llPtr aTy) "aPtr" pair >>= load aTy nmA
    b <- gepSnd (llPtr bTy) "bPtr" pair >>= load bTy nmB
    return (a, b)
unPair _ _ _ = error "unPair: wrong type"

-- | Call EEL function
call nme = getStack >>= callRaw llStack "stk" nme . return >>= setStack



module Backend.Preamble(
        -- * EEL LLVM types
        llStack, llStackDef, llStackPtr, llStackFunc, llList, llListDef,
        -- * Preamble generation
        preamble,
        -- * Runtime functions
        alloc, push, pop, generateFunc, mkPair, unPair, call,
        -- * Quotation runtime support
        mkQotData, mkQotFunc, mkQotComp, runQot
    ) where

import qualified Text.PrettyPrint as P
import Text.PrettyPrint((<+>))

import Backend.CodeGen

import Paths_eel as EEL
import Data.Version

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

-- | Generate the preamble
preamble mainName = do
    appendRaw $ P.semi <+> P.text "generated by EEL compiler version" <+> P.text (showVersion EEL.version)
    blankLine
    appendRaw $ P.semi <+> P.text "type definitions"
    appendRaw $ fill "%eelstack = type #" [llStackDef]
    appendRaw $ fill "%eellist  = type #" [llListDef]
    appendRaw $ fill "%eelqot   = type #" [llQotDef]
    blankLine
    appendRaw $ P.semi <+> P.text "external declarations"
    funcPrototype llRaw "@GC_malloc" [llSize]
    mapM_ (\nme -> funcPrototype llFloat ("@llvm." ++ nme ++ ".f" ++ show ptrBits) [llFloat]) ["sin", "log"]
    funcPrototype llFloat ("@llvm.pow.f" ++ show ptrBits) [llFloat, llFloat]
    funcPrototype llIntNative "@fgetc" [llRaw]
    funcPrototype llIntNative "@fputc" [llIntNative, llRaw]
    appendRaw $ P.text "@stdin = external global i8*"
    appendRaw $ P.text "@stdout = external global i8*"
    blankLine
    -- Main function
    printMain mainName
    blankLine
    -- C string to EEL string
    printC2S
    blankLine
    -- EEL quotation runners
    printEelRunners
    blankLine
    -- EEL fixpoint operator
    printFix
    blankLine

-- | C string to EEL string conversion
printC2S = do
    -- C string to EEL string
    [strin, nextstr] <- fresh llRaw ["strin", "nextstr"]
    [lptrnext] <- fresh (llPtr llList) ["lptrnext"]
    appendRaw $ P.text "; C string to EEL string conversion"
    appendRaw $ funcHeader llList "@eelC2S" [strin] <+> P.lbrace
    label "start"
    list <- instr (llPtr llList) "list" "alloca #" [llList]
    store (LLVar llList "null") list
    branch "loop"
    label "loop"
    str <- phi llRaw "str" [(strin, "start"), (nextstr, "body")]
    lptr <- phi (llPtr llList) "lptr" [(list, "start"), (lptrnext, "body")]
    chr <- load llChar "chr" str
    cmp <- instr llBool "cmp" "icmp eq #, 0" [chr]
    branchCond cmp "end" "body"
    label "body"
    appendTmpl "# = getelementptr #, i32 1" [P.text (llVarName nextstr), llVarDoc str]
    newElem <- alloc (llPtr llListDef) "elem" (2 * ptrBytes)
    newElemR <- bitcast llList "elemR" newElem
    store newElemR lptr
    appendTmpl "# = getelementptr #, i32 0, i32 1" [P.text (llVarName lptrnext), llVarDoc newElem]
    store (LLVar llList "null") lptrnext
    gepFst (llPtr llRaw) "charpr" newElem >>= bitcast llRaw "charp" >>= store chr
    branch "loop"
    label "end"
    load llList "ret" list >>= ret
    appendRaw $ P.rbrace

-- | EEL runners
printEelRunners = do
    appendRaw $ P.text "; Composition quotation runner"
    printOneEelRunner "comp" $ \fr gr -> do
        f <- bitcast llQot "f" fr
        g <- bitcast llQot "g" gr
        runQot f >> runQot g
        --call f stk >>= call g
    blankLine
    appendRaw $ P.text "; Constant data quotation runner"
    printOneEelRunner "data" $ \fr _gr -> do
        push fr
    blankLine
    appendRaw $ P.text "; Single function runner"
    printOneEelRunner "func" $ \fr _gr -> do
        fun <- bitcast llStackFunc "fun" fr 
        getStack >>= callRaw llStack "stk" (llVarName fun) . return >>= setStack


printOneEelRunner name body = do
    [fr, gr] <- fresh llRaw ["fr", "gr"]
    stk <- fresh1 llStack "stk"
    appendRaw $ funcHeader llStack ("@eelrun." ++ name) [stk, fr, gr] <+> P.lbrace
    setStack stk
    () <- body fr gr
    stk' <- getStack
    ret stk'
    appendRaw $ P.rbrace

-- | Main function wrapper: stack initialisation and argument passing
printMain Nothing = comment "No main function" ([] :: [Int])
printMain (Just mainName) = do
    let argvTy = llPtr (llPtr llChar)
    argc <- fresh1 llIntNative "argc"
    [argv, argvPNext] <- fresh argvTy ["argv", "argvPNext"]
    [arglPNext] <- fresh (llPtr llList) ["arglPNext"]
    -- function header
    appendRaw . P.text $ "; main function is " ++ show mainName
    appendRaw $ funcHeader llIntNative "@main" [argc, argv] <+> P.lbrace
    -- initialise
    label "start"
    argl <- instr (llPtr llList) "argl" "alloca #" [llList]
    store (LLVar llList "null") argl
    branch "loop"
    -- loop condition
    label "loop"
    argvP <- phi argvTy "argvP" [(argvPNext, "body"), (argv, "start")]
    arglP <- phi (llPtr llList) "arglP" [(arglPNext, "body"), (argl, "start")]
    arg <- load llRaw "arg" argvP
    cmp <- instr llBool "cmp" "icmp eq #, null"  [arg]
    branchCond cmp "main" "body"
    -- loop body
    label "body"
    appendTmpl "# = getelementptr #, i32 1" [P.text (llVarName argvPNext), llVarDoc argvP]
    argR <- load llRaw "argR" argvP
    argS <- callRaw llList "argS" "@eelC2S" [argR]
    argE <- alloc llList "argE" (2*ptrBytes)
    store argE arglP
    appendTmpl "# = getelementptr #, i32 0, i32 1" [P.text (llVarName arglPNext), llVarDoc argE]
    store (LLVar llList "null") arglPNext
    gepFst (llPtr llRaw) "argDR" argE >>= bitcast (llPtr llList) "argD" >>= store argS
    branch "loop"
    -- main
    label "main"
    comment "initialise the stack" ([] :: [Int])
    setStack (LLVar llStack "null")
    args <- load llList "args" arglP
    push args
    comment "Invoke main #" [mainName]
    initStk <- getStack
    finalStk <- callRaw llStack "stk" mainName [initStk]
    comment "Convert final stack to return value" ([] :: [Int])
    finalStkS <- bitcast (llPtr llStackDef) "stkS" finalStk
    retvalI <- gepSnd (llPtr llRaw) "retRP" finalStkS >>= bitcast (llPtr llInt) "retP" >>= load llInt "ret"
    retval <- if intBits < ptrBits
                then instr llIntNative "ret" "trunc # to #" [llVarDoc retvalI, llTypeDoc llIntNative]
                else return retvalI
    comment "return exit code" ([] :: [Int])
    ret retval
    appendRaw $ P.rbrace

printFix = generateFunc "@eelfix" (P.text "Fixpoint operator") $ do
    comment "Get recursed function" ([] :: [Int])
    funq <- pop llRaw "func"
    comment "EELfix quotation" ([] :: [Int])
    fix <- mkQotFunc (LLVar llStackFunc "@eelfix")
    comment "Create the fixpoint function and push" ([] :: [Int])
    mkQotComp funq fix >>= push
    comment "Run the function with itself on the top of the stack" ([] :: [Int])
    runQot funq

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
